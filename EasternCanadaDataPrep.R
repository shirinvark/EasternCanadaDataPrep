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
    study_v,
    resolution = 250,
    crs = terra::crs(study_v)
  )
  
  values(planning) <- NA
  sim$PlanningGrid_250m <- planning
  
  ## Align LandCover
  lc_aligned <- terra::project(sim$LandCover, planning, method = "near")
  lc_aligned <- terra::resample(lc_aligned, planning, method = "near")
  sim$LandCover_250m <- lc_aligned
  
  ## Align standAge
  sa_aligned <- terra::project(sim$standAgeMap, planning, method = "near")
  sa_aligned <- terra::resample(sa_aligned, planning, method = "near")
  sim$standAge_250m <- sa_aligned
  
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
  
  ##LCC
  ## LCC — upstream only
  #if (SpaDES.core::suppliedElsewhere("rstLCC")) {
    
    
    #lcc <- sim$rstLCC
    
    #if (!terra::same.crs(lcc, studyArea_v)) {
     # lcc <- terra::project(lcc, studyArea_v)
   # }
    
    #lcc <- terra::crop(lcc, studyArea_v)
    
    #sim$rstLCC <- lcc
  ## ---- LandCover (SCANFI) ----
  ## ---------------------------------------------------------
  ## LandCover
  ## ---------------------------------------------------------
  
  if (!is.null(sim$LandCover)) {
    
    message("✔ Using LandCover supplied externally (user or upstream module).")
    lc <- sim$LandCover
    
  } else {
    
    message("LandCover not supplied. Building using prepInputs_SCANFI_LCC_FAO...")
    
    lc <- prepInputs_SCANFI_LCC_FAO(
      studyArea = sim$studyArea,
      destinationPath = dPath
    )
    
    if (is.null(lc)) {
      stop("LandCover was not supplied and could not be built internally.")
    }
    
    sim$LandCover <- lc
  }
  ## Harmonize LandCover
  if (!terra::same.crs(sim$LandCover, studyArea_v)) {
    message("Reprojecting LandCover to studyArea CRS...")
    sim$LandCover <- terra::project(sim$LandCover, studyArea_v, method = "near")
  }
  
  message("Cropping LandCover to studyArea...")
  sim$LandCover <- terra::crop(sim$LandCover, studyArea_v)

  ## ---------------------------------------------------------
  ## standAgeMap
  ## ---------------------------------------------------------
  
  if (!is.null(sim$standAgeMap)) {
    
    message("✔ Using standAgeMap supplied externally (user or upstream module).")
    sa <- sim$standAgeMap
    
  } else {
    
    message("standAgeMap not supplied. Building using prepInputsStandAgeMap...")
    
    sa <- prepInputsStandAgeMap(
      rasterToMatch = sim$LandCover,
      studyArea = sim$studyArea,
      destinationPath = dPath,
      dataYear = 2001
    )
    
    if (is.null(sa)) {
      stop("standAgeMap was not supplied and could not be built internally.")
    }
    
    sim$standAgeMap <- sa
  }
  ## Harmonize standAgeMap
  if (!terra::same.crs(sim$standAgeMap, studyArea_v)) {
    message("Reprojecting standAgeMap to studyArea CRS...")
    sim$standAgeMap <- terra::project(sim$standAgeMap, studyArea_v, method = "near")
  }
  
  message("Cropping standAgeMap to studyArea...")
  sim$standAgeMap <- terra::crop(sim$standAgeMap, studyArea_v)
  return(invisible(sim))

  }

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
