## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName
defineModule(sim, list(
  name = "EasternCanadaDataPrep",
  description = "Loads and prepares spatial inputs (FMUs, protected areas, hydrology) for downstream landbase and harvesting analyses.",
  keywords = c("Eastern Canada", "Data Prep", "FMU", "CPCAD", "Hydrology"),
  authors = structure(list(list(given = c("Shirin", "Middle"), family = "Varkouhi", role = c("aut", "cre"), email = "shirin.varkuhi@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(EasternCanadaDataPrep = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "EasternCanadaDataPrep.Rmd"),
  reqdPkgs = list(
    "SpaDES.core (>= 2.1.8.9001)",
    "ggplot2",
    "reproducible",
    "sf",
    "terra",
    "LandR" ,
    "rnaturalearth"
  ),  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(
      ".plotInitialTime",
      "numeric",
      NA,
      NA,
      NA,
      "Simulation time at which the first plot event should occur"
    ),
    
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
   
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(
      "dataYear",
      "numeric",
      2001,
      NA,
      NA,
      "Year of NFI stand age dataset (e.g., 2001 or 2011)"
    ),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    
  ),
  inputObjects = bindrows(
    
    expectsInput("studyArea",
                 objectClass = c("sf", "SpatVector"),
                 desc = "Study area polygon",
                 sourceURL = NA),
    
    expectsInput("CPCAD",
                 objectClass = c("sf", "SpatVector"),
                 desc = "Protected areas",
                 sourceURL = NA),
    
    expectsInput("FMU",
                 objectClass = c("sf", "SpatVector"),
                 desc = "Forest Management Units",
                 sourceURL = NA),
    
    expectsInput("LandCover",
                 objectClass = "SpatRaster",
                 desc = "Land cover raster",
                 sourceURL = NA),
    expectsInput(
      "rasterToMatch",
      objectClass = "SpatRaster",
      desc = "Template raster used to align all spatial layers",
      sourceURL = NA
    ),
    
    expectsInput("standAge",
                 objectClass = "SpatRaster",
                 desc = "Stand age raster",
                 sourceURL = NA)
    
  ),
  outputObjects = bindrows(
    
    createsOutput(
      objectName = "LegalConstraints",
      objectClass = "list",
      desc = "Legal and administrative spatial constraints derived from FMUs and protected areas."
    ),
    createsOutput(
      objectName = "PlanningGrid",
      objectClass = "SpatRaster",
      desc = "PlanningGrid used for landbase accounting and downstream AAC calculations."
    )
    
)))

doEvent.EasternCanadaDataPrep <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- EasternCanadaInit(sim)
    },
    warning(noEventWarning(sim))
  )
  invisible(sim)
}

## Build the PlanningGrid and core landbase components.
## This function establishes the spatial analysis grid and
## derives legal/managerial constraints (FMUs, protected areas).
##
## Importantly:
## - No ecological interpretation is performed here
## - No harvest or policy decisions are applied
## - Outputs are intended for reuse by multiple downstream modules


## Build provincial boundaries for the study area.
##
## This function exists to provide a clean, explicit
## jurisdictional layer for downstream modules.
##
## Provincial boundaries are NOT used here for decisions,
## but allow other modules (e.g., hydrology, landbase policy)
## to apply province-specific rules in a transparent way.


###########################
## NOTE:
## This module is responsible for preparing spatial inputs only.
## No policy interpretation or landbase decisions are made here.
##
## The Provinces object is produced solely to enable
## jurisdiction-aware processing in downstream modules
## (e.g., province-based riparian policies in EasternCanadaHydrology).
## This module does not apply or interpret those policies.
#browser()

buildPlanningGrid <- function(sim) {
 # browser()
  
  message("🔵 Building PlanningGrid and aligning layers...")
  
  if (inherits(sim$studyArea, "SpatVector")) {
    study_v <- sim$studyArea
  } else {
    study_v <- terra::vect(sim$studyArea)
  }
  
  targetRes <- terra::res(sim$rasterToMatch)[1]
  ext <- terra::ext(study_v)
  
  width  <- ext[2] - ext[1]
  height <- ext[4] - ext[3]
  ncol <- width / targetRes
  nrow <- height / targetRes
  
  message("Estimated columns: ", round(ncol))
  message("Estimated rows: ", round(nrow))
  message("Estimated total cells: ", round(ncol * nrow))
  # ---------------------------------------------------------
 
  
  # ---------------------------------------------------------
  # 2) Align LandCover
  # ---------------------------------------------------------
  
  lc_src <- sim$LandCover  
  message("LandCover ncell BEFORE crop: ", terra::ncell(lc_src))
  
  # 1️⃣ crop (safe)
  if (!terra::same.crs(study_v, lc_src)) {
    study_v_original_crs <- terra::project(study_v, terra::crs(lc_src))
  } else {
    study_v_original_crs <- study_v
  }
  
  message("STARTING EXTENT CROP")
  
  lc_src <- terra::crop(
    lc_src,
    terra::ext(study_v_original_crs),
    snap = "out"
  )
  
  message("EXTENT CROP FINISHED")
  
   
  if (is.null(lc_src) || terra::ncell(lc_src) == 0) {
    stop("❌ Crop produced empty raster.")
  }
  
  # 2️⃣ project to analysis CRS
  planning_template <- sim$rasterToMatch 
  message("STARTING PROJECT/RESAMPLE")
  message("STARTING PRE-AGGREGATION")
  

  res_lc <- terra::res(lc_src)[1]
  
  fact <- floor(targetRes / res_lc)
  
  if (is.na(fact) || fact < 1) {
    fact <- 1
  }  

  if (fact > 1) {
    
    lc_src <- terra::aggregate(
      lc_src,
      fact = fact,
      fun = terra::modal,
      na.rm = TRUE
    )
    
  }
  
  message("PRE-AGGREGATION FINISHED")
  sim$LandCover <- terra::project(
    lc_src,
    planning_template,
    method = "near",
    mask = TRUE
  )
  
  message("PROJECT/RESAMPLE FINISHED")
  #browser()
  
  # -----------------------------------------------------
  # ---------------------------------------------------------
  # 3) FINAL PlanningGrid
  # ---------------------------------------------------------
  
  message("Building PlanningGrid from studyArea")
  sim$PlanningGrid <- sim$rasterToMatch
  
  terra::values(sim$PlanningGrid) <- 1
  
  planning <- sim$PlanningGrid
  # ---------------------------------------------------------
  # 4) Align standAge
  # ---------------------------------------------------------
  
  if (!is.null(sim$standAge)) {
    
    sa_src <- sim$standAge    
    # ---------------------------------------------
    # crop FIRST in native CRS
    # ---------------------------------------------
    
    if (!terra::same.crs(study_v, sa_src)) {
      
      study_v_sa <- terra::project(
        study_v,
        terra::crs(sa_src)
      )
      
    } else {
      
      study_v_sa <- study_v
      
    }
    
    sa_src <- terra::crop(
      sa_src,
      terra::ext(study_v_sa),
      snap = "out"
    )
    
    # ---------------------------------------------
    # aggregate BEFORE project
    # ---------------------------------------------
    
    res_sa <- terra::res(sa_src)[1]
    fact_sa <- round(targetRes / res_sa)
    if (is.na(fact_sa) || fact_sa < 1) {
      fact_sa <- 1
    }
    
    if (fact_sa > 1) {
      
      sa_src <- terra::aggregate(
        sa_src,
        fact = fact_sa,
        fun = terra::modal,
        na.rm = TRUE
      )
      
    }
    
    # ---------------------------------------------
    # project AFTER crop + aggregate
    # ---------------------------------------------
    
    sa_src <- terra::project(
      sa_src,
      terra::crs(planning),
      method = "near"
    )
    sim$standAge <- terra::resample(
      sa_src,
      planning,
      method = "near"
    )
  }
  # ---------------------------------------------------------
  # 5) Rasterize FMU & CPCAD
  # ---------------------------------------------------------
  
  ## Rasterize FMU
  if (!"FMU_ID" %in% names(sim$FMU)) {
    sim$FMU$FMU_ID <- seq_len(nrow(sim$FMU))
  }
  #browser()
  fmu_r <- terra::rasterize(
    sim$FMU,
    planning,
    field = "FMU_ID",
    touches = FALSE
  )
  #browser()
  ## Rasterize CPCAD
  prot_r <- terra::rasterize(
    sim$CPCAD,
    planning,
    field = 1,
    background = 0
  )
  
  ## Legal mask
  LegalHarvestMask <- terra::ifel(
    !is.na(fmu_r) & prot_r == 0,
    1,
    0
  )
  
  sim$LegalConstraints <- list(
    FMU_Raster = fmu_r,
    CPCAD_Raster = prot_r,
    LegalHarvestMask = LegalHarvestMask
  )
  
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  ## ---------------------------------------------------------
  ## 1) Create studyArea if not provided by user
  ## ---------------------------------------------------------
  if (!SpaDES.core::suppliedElsewhere("studyArea")) {
    
    message("🔵 Creating default studyArea (Eastern Canada)...")
    
    can <- rnaturalearth::ne_states(
      country = "Canada",
      returnclass = "sf"
    )
    
    east <- can[can$name_en %in% c(
      "Ontario","Quebec","New Brunswick",
      "Nova Scotia","Prince Edward Island",
      "Newfoundland and Labrador"
    ), ]
    
    east_union <- sf::st_union(east)
    
    sim$studyArea <- sf::st_sf(
      data.frame(id = 1),
      geometry = sf::st_transform(east_union, "ESRI:102001")
    )
  }
  
  studyArea_sf <- sim$studyArea
  if (inherits(studyArea_sf, "SpatVector")) {
    studyArea_v <- studyArea_sf
  } else {
    studyArea_v <- terra::vect(studyArea_sf)
  }  ## ---------------------------------------------------------
 
  ## ---------------------------------------------------------
  
  ## ---------------------------------------------------------
  ## 2) CPCAD – Protected & conserved areas
  ## ---------------------------------------------------------
  if (!SpaDES.core::suppliedElsewhere("CPCAD")){
    
    cpcad_dir <- file.path(dPath, "CPCAD")
   # dir.create(cpcad_dir, recursive = TRUE, showWarnings = FALSE)
    
    message("▶ Preparing CPCAD...")
    
    sim$CPCAD <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1ELIaRgO5PNgliGh0Tq2BI6V5654ydxYu",
      destinationPath = file.path(dPath, "CPCAD"),#cpcad_dir,
      targetFile = "CPCAD_2024.gpkg",
      fun = terra::vect,
      layer = "ProtectedConservedArea_2024 ProtectedConservedArea_2024",
      cropTo    = studyArea_sf,
      projectTo = studyArea_sf
    )
    
  }
  
  cpcad <- sim$CPCAD
  
  ## filters (policy-level, not ecological)
  if ("STATUS" %in% names(cpcad))
    cpcad <- cpcad[cpcad$STATUS %in% c(1, 2), ]
  
  if ("PA_OECM_DF" %in% names(cpcad))
    cpcad <- cpcad[cpcad$PA_OECM_DF %in% c(1, 2, 3), ]
  
  if ("IUCN_CAT" %in% names(cpcad))
    cpcad <- cpcad[cpcad$IUCN_CAT %in% 1:7, ]
  
  sim$CPCAD <- cpcad
  
  if (!terra::same.crs(sim$CPCAD, studyArea_v)) {
    sim$CPCAD <- terra::project(sim$CPCAD, studyArea_v)
  }
  
  message("✔ CPCAD ready. Features: ", nrow(sim$CPCAD))
  
  ## ---------------------------------------------------------
  ## 3) FMU – Forest Management Units
  ## ---------------------------------------------------------
  ## ---------------------------------------------------------
  ## 3) FMU – Forest Management Units
  ## ---------------------------------------------------------
  if (!SpaDES.core::suppliedElsewhere("FMU")) {
    
    fmu_dir <- file.path(dPath, "FMU")
    
    message("▶ Preparing FMU...")
    
    sim$FMU <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1jfrgLrpB2nitynfZMS7lk-H7Fmbt0GK-",
      destinationPath = fmu_dir,
      targetFile = "Canada_FMU4.shp",
      fun = terra::vect,
      cropTo = studyArea_sf,
      projectTo = studyArea_sf
    )
    
  }
  
  if (!terra::same.crs(sim$FMU, studyArea_v)) {
    sim$FMU <- terra::project(sim$FMU, studyArea_v)
  }
  
  # =========================================================
  # 2) LandCover
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("LandCover", sim)) {
    
    message("✔ Using LandCover supplied from upstream or user.")
    
  } else {
    
    dPath <- SpaDES.core::dataPath(sim)
    
    lc_dir <- file.path(dPath, "LandCover")
    dir.create(lc_dir, showWarnings = FALSE, recursive = TRUE)
    
    lc_file <- file.path(lc_dir, "LandCover.tif")
    
    if (file.exists(lc_file)) {
      
      message("✔ LandCover found locally. Loading...")
      
      sim$LandCover <- terra::rast(lc_file)
      
    } else {
      
      message("⬇ LandCover not found locally. Downloading from Drive...")
      
      sim$LandCover <- Cache(
        prepInputs,
        url = "https://drive.google.com/uc?export=download&id=1Gzhd5VnIZ7MqRSRJmNFiGfVUHrKkP9Ag",
        destinationPath = lc_dir,
        targetFile = "LandCover.tif",
        fun = terra::rast,
        overwrite = FALSE
      )
    }
  }
  
  # =========================================================
  # 3) standAge (SCANFI 2020 only)
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("standAge", sim)) {
    
    message("✔ Using standAge supplied from upstream or user.")
    
  } else {
    
    dPath <- SpaDES.core::dataPath(sim)
    
    sa_dir <- file.path(dPath, "standAge")
    dir.create(sa_dir, showWarnings = FALSE, recursive = TRUE)
    
    sa_file <- file.path(
      sa_dir,
      "standAge.tif"
    )
    
    if (file.exists(sa_file)) {
      
      message("✔ standAge found locally. Loading...")
      
      sim$standAge <- terra::rast(sa_file)
      
    } else {
      
      message("⬇ standAge not found locally. Downloading from Drive...")
      
      sim$standAge <- Cache(
        prepInputs,
        url = "https://drive.google.com/uc?export=download&id=1OdZ7Tznk53KceEyt9dFOBOkxDHEX5X0U",
        destinationPath = sa_dir,
        targetFile = "standAge.tif",
        fun = terra::rast,
        overwrite = FALSE
      )
    }
  }
  
  return(invisible(sim))
  
}  # end .inputObjects

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
