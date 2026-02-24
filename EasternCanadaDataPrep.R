## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules)
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
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("studyArea", objectClass = c("sf", "SpatVector"), desc = "Study area polygon used for cropping and masking", sourceURL = NA),
    expectsInput("CPCAD",objectClass = c("sf", "SpatVector"), desc = "CPCAD protected areas — generated inside this module", sourceURL = NA),
    expectsInput("FMU",objectClass = c("sf", "SpatVector"), desc = "Forest Management Units — generated inside this module",sourceURL = NA)),
  outputObjects = bindrows(
    
    createsOutput(
      objectName = "LegalConstraints",
      objectClass = "list",
      desc = "Legal and administrative spatial constraints derived from FMUs and protected areas."
    ),
    createsOutput(
      objectName = "PlanningGrid_250m",
      objectClass = "SpatRaster",
      desc = "PlanningGrid_250m used for landbase accounting and downstream AAC calculations."
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

## Build the PlanningGrid_250m and core landbase components.
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
buildPlanningGrid <- function(sim) {
  
  message("🔵 Building PlanningGrid and aligning layers...")
  
  study_v <- terra::vect(sim$studyArea)
  
  planning <- terra::rast(
    extent = terra::ext(study_v),
    resolution = 250,
    crs = terra::crs(study_v)
  )
  
  terra::origin(planning) <- c(0, 0)
  
  values(planning) <- NA
  sim$PlanningGrid_250m <- planning
  
  ## Align LandCover
  ## Fast window crop before projection
  ## Step 1: window crop (fast)
  lc_src <- sim$LandCover
  
  ## 1) Reproject first (if needed)
  if (!terra::same.crs(lc_src, planning)) {
    lc_src <- terra::project(lc_src, terra::crs(planning), method = "near")
  }
  
  ## 2) Then window crop (faster on aligned grid)
  lc_window <- terra::crop(
    lc_src,
    terra::ext(planning),
    snap = "out"
  )
  
  ## Step 3: aggregate from 30m to 250m (much faster than full project)
  res_lc <- terra::res(lc_window)[1]
  fact <- as.integer(round(250 / res_lc))
  
  if (abs(res_lc * fact - 250) > 1) {
    stop("LandCover resolution not compatible with 250m aggregation.")
  }  
  lc_agg <- terra::aggregate(
    lc_window,
    fact = fact,
    fun = modal,
    na.rm = TRUE,
    filename = file.path(tempdir(), "lc_agg_250m.tif"),
    overwrite = TRUE
  )
  
  ## Step 4: align exactly to planning grid
  if (!terra::compareGeom(lc_agg, planning, stopOnError = FALSE)) {
    sim$LandCover_250m <- terra::resample(lc_agg, planning, method = "near")
  } else {
    sim$LandCover_250m <- lc_agg
  }
  
  ## Align standAge (only if exists)
  if (!is.null(sim$standAgeMap)) {
    
    sa_aligned <- terra::project(sim$standAgeMap, planning, method = "near")
    sa_aligned <- terra::resample(sa_aligned, planning, method = "near")
    sim$standAge_250m <- sa_aligned
    
  }
  ## Rasterize FMU
  if (!"FMU_ID" %in% names(sim$FMU)) {
    sim$FMU$FMU_ID <- seq_len(nrow(sim$FMU))
  }
  
  fmu_r <- terra::rasterize(
    sim$FMU,
    planning,
    field = "FMU_ID",
    touches = FALSE
  )
  
  ## Rasterize CPCAD
  prot_r <- terra::rasterize(
    sim$CPCAD,
    planning,
    field = 1,
    background = 0
  )
  
  ## Legal mask
  LegalHarvestMask_250m <- terra::ifel(
    !is.na(fmu_r) & prot_r == 0,
    1,
    0
  )
  
  sim$LegalConstraints <- list(
    FMU_Raster_250m = fmu_r,
    CPCAD_Raster_250m = prot_r,
    LegalHarvestMask_250m = LegalHarvestMask_250m
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
      "Ontario",
      "Quebec",
      "New Brunswick",
      "Nova Scotia",
      "Prince Edward Island",
      "Newfoundland and Labrador"
    ), ]
    
    east_union <- sf::st_union(east)
    
    sim$studyArea <- sf::st_sf(
      data.frame(id = 1),
      geometry = sf::st_transform(east_union, "ESRI:102001")
    )
  }
  
  studyArea_sf <- sim$studyArea
  studyArea_v  <- terra::vect(studyArea_sf)
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
  if (!SpaDES.core::suppliedElsewhere("FMU")){
    
    fmu_dir <- file.path(dPath, "FMU")
   # dir.create(fmu_dir, recursive = TRUE, showWarnings = FALSE)
    
    message("▶ Preparing FMU...")
    
    sim$FMU <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1qp4TRgFArANp1YNEoOpeuwLlM-khf4v1",
      destinationPath = file.path(dPath, "FMU"),#fmu_dir,
      targetFile = "Forest_Management_Units_CA.gpkg",
      fun = terra::vect,
      cropTo    = studyArea_sf,
      projectTo = studyArea_sf
    )
    
  }
  
  if (!terra::same.crs(sim$FMU, studyArea_v)) {
    sim$FMU <- terra::project(sim$FMU, studyArea_v)
  }
  
  
  ## ---------------------------------------------------------
  ## LandCover
  ## ---------------------------------------------------------
  ## ---------------------------------------------------------
  ## LandCover (Upstream OR Download)
  ## ---------------------------------------------------------
  
  ## ---------------------------------------------------------
  ## LandCover (Upstream → Local → Download)
  ## ---------------------------------------------------------
  
  if (SpaDES.core::suppliedElsewhere("LandCover")) {
    
    message("✔ Using LandCover supplied from upstream module.")
    
  } else {
    
    lc_dir  <- file.path(dPath, "LandCover")
    lc_file <- file.path(lc_dir, "LandCover_SCANFI_2020.tif")
    
    dir.create(lc_dir, showWarnings = FALSE, recursive = TRUE)
    
    if (!file.exists(lc_file)) {
      
      message("LandCover not found locally. Downloading from Google Drive...")
      
      Cache(
        prepInputs,
        url = "https://drive.google.com/uc?export=download&id=1Gzhd5VnIZ7MqRSRJmNFiGfVUHrKkP9Ag",
        destinationPath = lc_dir,
        targetFile = "LandCover_SCANFI_2020.tif",
        fun = terra::rast,
        overwrite = FALSE,
        useCache = TRUE
      )
      
    } else {
      message("✔ LandCover found locally. Skipping download.")
    }
    
    sim$LandCover <- terra::rast(lc_file)
    ## ---------------------------------------------------------
    ## standAgeMap (Upstream → Local → Download → Fast Crop)
    ## ---------------------------------------------------------
    
    if (SpaDES.core::suppliedElsewhere("standAgeMap")) {
      
      message("✔ Using standAgeMap supplied from upstream module.")
      
    } else {
      
      sa_dir  <- file.path(dPath, "StandAge")
      sa_file <- file.path(sa_dir, "SCANFI_att_age_S_2020_v1_1.tif")
      
      dir.create(sa_dir, showWarnings = FALSE, recursive = TRUE)
      
      ## اگر فایل قبلاً دانلود شده → دوباره دانلود نکن
      if (!file.exists(sa_file)) {
        
        message("standAgeMap not found locally. Downloading from Google Drive...")
        
        Cache(
          prepInputs,
          url = "https://drive.google.com/uc?export=download&id=1OdZ7Tznk53KceEyt9dFOBOkxDHEX5X0U",
          destinationPath = sa_dir,
          targetFile = "SCANFI_att_age_S_2020_v1_1.tif",
          fun = terra::rast,
          overwrite = FALSE,
          useCache = TRUE
        )
        
      } else {
        message("✔ standAgeMap found locally. Skipping download.")
      }
      
      ## فقط rast کن — هیچ LandR pipeline اجرا نشه
      sim$standAgeMap <- terra::rast(sa_file)
      
    }
    
    ## فقط یک crop سریع
    sim$standAgeMap <- terra::crop(sim$standAgeMap, studyArea_v)
  }
  return(invisible(sim))

  }

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
