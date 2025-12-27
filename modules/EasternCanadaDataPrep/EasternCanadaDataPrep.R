## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "EasternCanadaDataPrep",
  description = "Prepare spatial layers for Eastern Canada (FMUs, CPCAD, hydrology, landcover)",
  keywords = c("Eastern Canada", "Data Prep", "FMU", "CPCAD", "Hydrology"),
  authors = structure(list(list(given = c("Shirin", "Middle"), family = "Varkouhi", role = c("aut", "cre"), email = "shirin.varkuhi@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(EasternCanadaDataPrep = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "EasternCanadaDataPrep.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.8.9001)", "ggplot2", "reproducible", "sf", "terra"),
  parameters = bindrows(
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
    
    ## ---- model parameters (YOUR logic) ----
    defineParameter(
      "riparianBuffer_m",
      "numeric",
      30,
      0,
      500,
      "Buffer distance (m) for hydrology features (flowline, watercourse, waterbody)"
    ),
    defineParameter(
      "buildHLB",
      "logical",
      FALSE,   # 👈  
      NA,
      NA,
      "Should Harvestable Land Base (HLB) be built where possible?"
    ),
    defineParameter(
      "targetFMU",
      "character",
      NA,
      NA,
      NA,
      "FMU name for which HLB should be built (e.g. 'Temagami Forest')"
    ),
    defineParameter(
      "hydroRaster_m",
      "numeric",
      100,
      30,
      1000,
      "Resolution (m) of hydrology raster template"
    )
    
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("studyArea", objectClass = c("sf", "SpatVector"), desc = "Study area polygon used for cropping and masking", sourceURL = NA),
    expectsInput(
      "LandCover",
      objectClass = "SpatRaster",
      desc = "Land cover raster (provided by another module, e.g. LandR)",
      sourceURL = NA
    ),
    
    expectsInput("CPCAD",objectClass = c("sf", "SpatVector"), desc = "CPCAD protected areas — generated inside this module", sourceURL = NA),
    expectsInput("FMU",objectClass = c("sf", "SpatVector"), desc = "Forest Management Units — generated inside this module",sourceURL = NA)
  ),
  outputObjects = bindrows(
    
    createsOutput(
      objectName = "EasternCanadaLandbase",
      objectClass = "list",
      desc = "A list containing assembled spatial layers for Eastern Canada (FMU, CPCAD, Hydrology, LandCover, and optional HLB)."
    ),
    
    createsOutput(
      objectName = "PlanningRaster",
      objectClass = "SpatRaster",
      desc = "Planning raster used for landbase accounting and downstream AAC calculations."
    ),
    
    createsOutput(
      objectName = "LandbaseTable",
      objectClass = "data.frame",
      desc = "Cell-based landbase accounting table with effective harvestable area after protection and riparian constraints."
    )
    
  )
  
))

### template initialization
doEvent.EasternCanadaDataPrep <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(
        sim,
        time(sim),
        "EasternCanadaDataPrep",
        "buildLandbaseRaster"
      )
    },
    
    
    buildLandbaseRaster = {
      sim <- buildLandbaseRaster(sim)
      
      sim <- scheduleEvent(
        sim,
        time(sim),
        "EasternCanadaDataPrep",
        "buildLandbase"
      )
    },
    
    buildLandbase = {
      sim <- buildLandbase(sim)
    },
    
    warning(noEventWarning(sim))
  )
  
  return(invisible(sim))
}

### template for save events
#Save <- function(sim) {
# ! ----- EDIT BELOW ----- ! #
# do stuff for this event
#sim <- saveFiles(sim)

# ! ----- STOP EDITING ----- ! #
#return(invisible(sim))
#}

### template for plot events
#plotFun <- function(sim) {
# ! ----- EDIT BELOW ----- ! #
# do stuff for this event
# sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
#Plots(sampleData, fn = ggplotFn) # needs ggplot2

# ! ----- STOP EDITING ----- ! #
#return(invisible(sim))
#}

### template for your event1
#Event1 <- function(sim) {
# ! ----- EDIT BELOW ----- ! #
# THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
# sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
# sim$event1Test2 <- 999 # for dummy unit test

# ! ----- STOP EDITING ----- ! #
# return(invisible(sim))
#}

### template for your event2
#Event2 <- function(sim) {
# ! ----- EDIT BELOW ----- ! #
# THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
# sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
# sim$event2Test2 <- 777  # for dummy unit test

# ! ----- STOP EDITING ----- ! #
# return(invisible(sim))
#}
# NOTE:
# This function builds a quantitative landbase accounting layer.
# It intentionally excludes age, yield curves, and AAC logic,
# which are handled in downstream modules.
# Builds a coarse-resolution planning raster and a cell-based landbase
# accounting table (m2), excluding age, yields, and AAC logic.

buildLandbaseRaster <- function(sim) {
  
  message("🔵 Building planning raster & landbase accounting table...")
  
  ## -----------------------------
  ## sanity checks
  ## -----------------------------
  if (is.null(sim$LandCover))
    stop("LandCover is required for planning raster.")
  
  if (is.null(sim$Hydrology$riparianFraction))
    stop("Hydrology riparianFraction is missing.")
  
  ## -----------------------------
  ## inputs
  ## -----------------------------
  landcover <- sim$LandCover
  fmu       <- sim$FMU
  cpcad     <- sim$CPCAD
  riparian  <- sim$Hydrology$riparianFraction
  
  res_m <- P(sim)$hydroRaster_m
  
  ## -----------------------------
  ## 1) planning raster
  ## -----------------------------
  planning <- terra::rast(
    terra::ext(terra::vect(sim$studyArea)),
    resolution = res_m,
    crs = terra::crs(landcover)
  )
  
  values(planning) <- NA
  
  sim$PlanningRaster <- planning
  
  ## -----------------------------
  ## 2) rasterize FMU
  ## -----------------------------
  fmu_r <- terra::rasterize(
    fmu,
    planning,
    field = "FM_UNIT_NAME"
  )
  
  ## -----------------------------
  ## 3) rasterize CPCAD (protected)
  ## -----------------------------
  prot_r <- terra::rasterize(
    cpcad,
    planning,
    field = 1,
    background = 0
  )
  
  ## -----------------------------
  ## 4) align riparian fraction
  ## -----------------------------
  rip_r <- terra::resample(
    riparian,
    planning,
    method = "bilinear"
  )
  
  ## -----------------------------
  ## 5) build accounting table
  ## -----------------------------
  df <- data.frame(
    FMU = terra::values(fmu_r),
    protected = terra::values(prot_r),
    riparian_frac = terra::values(rip_r)
  )
  
  df <- df[!is.na(df$FMU), ]
  
  ## cell area in m2
  cell_area <- prod(terra::res(planning))
  
  df$cell_area <- cell_area
  
  ## sanitize riparian fraction
  rip <- df$riparian_frac
  rip[is.na(rip)] <- 0
  rip <- pmin(pmax(rip, 0), 1)
  
  ## effective harvestable area per cell (m2)
  df$effective_area <- ifelse(
    df$protected == 1,
    0,
    cell_area * (1 - rip)
  )
  
  
  sim$LandbaseTable <- df
  
  message("✔ Landbase raster & accounting table created")
  if (any(df$effective_area < 0)) {
    stop("Negative effective_area detected — check riparian fraction logic.")
  }
  
  message(
    "ℹ️ Mean effective area per cell (ha): ",
    round(mean(df$effective_area) / 10000, 3)
  )
  
  return(invisible(sim))
}
############################
buildLandbase <- function(sim) {
  
  message("🔵 Building Eastern Canada Landbase...")
  
  if (is.null(sim$CPCAD))
    stop("CPCAD was not created or supplied.")
  
  if (is.null(sim$FMU))
    stop("FMU was not created or supplied.")
  
  landcover <- sim$LandCover
  cpcad <- sim$CPCAD
  fmu   <- sim$FMU
  
  ## ---------------------------------------------------------
  ## HLB SWITCH
  ## ---------------------------------------------------------
  if (!isTRUE(P(sim)$buildHLB)) {
    
    sim$EasternCanadaLandbase <- list(
      LandCover = sim$LandCover,
      CPCAD     = cpcad,
      FMU       = fmu,
      Hydrology = sim$Hydrology,
      PlanningRaster  = sim$PlanningRaster,
      LandbaseTable   = sim$LandbaseTable,
      HLB             = NULL
    )
    
    message("✔ Eastern Canada Landbase created (NO HLB).")
    return(invisible(sim))
  }
  
  ## ---------------------------------------------------------
  ## Select target FMU
  ## ---------------------------------------------------------
  if (is.na(P(sim)$targetFMU)) {
    stop("buildHLB = TRUE but targetFMU is not set")
  }
  
  fmu_i <- fmu[fmu$FM_UNIT_NAME == P(sim)$targetFMU, ]
  
  if (nrow(fmu_i) != 1) {
    stop("targetFMU not found or not unique: ", P(sim)$targetFMU)
  }
  
  message("🟢 Building HLB for FMU: ", P(sim)$targetFMU)
  
  ## ---------------------------------------------------------
  ## Build HLB
  ## ---------------------------------------------------------
  # NOTE: Hydrology effects are applied later at the raster-based AAC stage
  cpcad_i <- terra::intersect(cpcad, fmu_i)
  hlb <- terra::erase(fmu_i, cpcad_i)
  ## ---------------------------------------------------------
  ## SAVE HLB TO DISK  👈👈 اینجاست
  ## ---------------------------------------------------------
  out_dir <- file.path(outputPath(sim), "HLB")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  hlb_file <- file.path(
    out_dir,
    paste0("HLB_", gsub(" ", "_", P(sim)$targetFMU), ".gpkg")
  )
  
  terra::writeVector(hlb, hlb_file, overwrite = TRUE)
  
  message("💾 HLB saved to disk: ", hlb_file)
  
  ## ---------------------------------------------------------
  ## SAVE TO sim
  ## ---------------------------------------------------------
  sim$EasternCanadaLandbase <- list(
    LandCover = sim$LandCover,
    CPCAD     = cpcad,
    FMU       = fmu,
    Hydrology = sim$Hydrology,
    HLB       = hlb
  )
  
  message("✔ Eastern Canada Landbase created (HLB mode).")
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #
  
  
  ## ---------------------------------------------------------
  ## 1) Create studyArea if not provided by user
  ## ---------------------------------------------------------
  if (!suppliedElsewhere("studyArea", sim)) {
    
    message("🔵 Creating default studyArea (Eastern Canada)...")
    
    can <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf")
    
    east <- can %>%
      dplyr::filter(name_en %in% c(
        "Ontario","Quebec","New Brunswick",
        "Nova Scotia","Prince Edward Island",
        "Newfoundland and Labrador"
      ))
    
    east_union <- sf::st_union(east)
    
    # CRS MUST MATCH CPCAD → ESRI:102001 (Canada Albers)
    sim$studyArea <- sf::st_transform(east_union,"ESRI:102001")
  }
  
  studyArea <- sim$studyArea
  
  ## ---------------------------------------------------------
  ## 2) LCC2020
  ## ---------------------------------------------------------
  #if (!suppliedElsewhere("LCC2020", sim)) {
  # Require::Require("httr2")
  
  #lcc_dir <- file.path(dPath, "LCC2020")
  #dir.create(lcc_dir, recursive = TRUE, showWarnings = FALSE)
  #zip_file <- file.path(lcc_dir, "land_cover_2020v2_30m_tif.zip")
  #if (file.exists(zip_file))
  # file.remove(zip_file)
  
  #message("▶ Preparing LCC 2020...")
  
  #sim$LCC2020 <- Cache(
  # prepInputs,
  #url = "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/land_cover_2020v2_30m_tif.zip",
  #destinationPath = lcc_dir,
  #targetFile = "NA_NALCMS_landcover_2020v2_30m/NA_NALCMS_landcover_2020v2_30m.tif",
  #archive = "land_cover_2020v2_30m_tif.zip",
  #overwrite = FALSE,  
  #quick = TRUE,
  #cropTo = studyArea,
  #maskTo = studyArea,
  #projectTo = studyArea
  #)
  #}
  

  ## ---------------------------------------------------------
  ## 3) CPCAD – SAFE MODE
  ## ---------------------------------------------------------
  
  if (!suppliedElsewhere("CPCAD", sim)) {
    
    cpcad_dir <- file.path(dPath, "CPCAD")
    dir.create(cpcad_dir, recursive = TRUE, showWarnings = FALSE)
    
    message("▶ Preparing CPCAD...")
    
    sim$CPCAD <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1ELIaRgO5PNgliGh0Tq2BI6V5654ydxYu",
      destinationPath = cpcad_dir,
      targetFile = "CPCAD_2024.gpkg",
      fun = "terra::vect",
      layer = "ProtectedConservedArea_2024 ProtectedConservedArea_2024",
      cropTo = studyArea,
      maskTo = studyArea,
      projectTo = studyArea
    )
    
    message("✔ CPCAD loaded and stored in sim$CPCAD")
  }
  
  cpcad <- sim$CPCAD
  
  ### 1) Remove Proposed (3) + Delisted (5) using STATUS
  if ("STATUS" %in% names(cpcad)) {
    cpcad <- cpcad[cpcad$STATUS %in% c(1, 2), ]
  }
  
  ### 2) Remove Proposed (4) + Delisted (5) using PA_OECM_DF
  if ("PA_OECM_DF" %in% names(cpcad)) {
    cpcad <- cpcad[cpcad$PA_OECM_DF %in% c(1, 2, 3), ]
  }
  
  ### 3) Keep IUCN values 1–7 (most valid categories)
  if ("IUCN_CAT" %in% names(cpcad)) {
    cpcad <- cpcad[cpcad$IUCN_CAT %in% 1:7, ]
  }
  
  sim$CPCAD <- cpcad
  
  message("✔ CPCAD filtering complete. Remaining features: ", nrow(sim$CPCAD))
  
  
  
  ## ---------------------------------------------------------
  ## 4) FMU
  ## ---------------------------------------------------------
  if (!suppliedElsewhere("FMU", sim)) {
    
    fmu_dir <- file.path(dPath, "FMU")
    dir.create(fmu_dir, recursive = TRUE, showWarnings = FALSE)
    
    message("▶ Preparing FMU...")
    
    sim$FMU <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1qp4TRgFArANp1YNEoOpeuwLlM-khf4v1",
      destinationPath = fmu_dir,
      targetFile = "Forest_Management_Units_CA.gpkg",
      fun = "terra::vect",
      cropTo = studyArea,
      maskTo = studyArea,
      projectTo = studyArea
    )
  }
 
  ## ---------------------------------------------------------
  ## 4.5) Road Network (RNF – Statistics Canada, 2011)
  ## ---------------------------------------------------------
  
  if (!suppliedElsewhere("RoadNetwork", sim)) {
    
    message("▶ Preparing Road Network (RNF 2011)...")
    
    rnf_dir <- file.path(dPath, "RoadNetwork")
    dir.create(rnf_dir, recursive = TRUE, showWarnings = FALSE)
    
    sim$RoadNetwork <- Cache(
      prepInputs,
      url = "https://www12.statcan.gc.ca/census-recensement/2011/geo/RNF-FRR/files-fichiers/lrnf000r25a_e.zip",
      destinationPath = rnf_dir,
      archive = "lrnf000r25a_e.zip",
      targetFile = "lrnf000r25a_e.shp",
      fun = terra::vect,
      cropTo = studyArea,
      projectTo = studyArea,
      overwrite = FALSE
    )
    
    message("✔ Road Network loaded and stored in sim$RoadNetwork")
  }
  
  ## ---------------------------------------------------------
  ## ---------------------------------------------------------
  ## Hydrology – HydroRIVERS (PRODUCTION SAFE)
  ## ---------------------------------------------------------
  
  if (!suppliedElsewhere("Hydrology", sim)) {
    
    message("▶ Preparing Hydrology from HydroRIVERS...")
    
    hydro_dir <- file.path(dPath, "Hydrology")
    dir.create(hydro_dir, recursive = TRUE, showWarnings = FALSE)
    
    ## 1) Load HydroRIVERS (vector)
    streams <- Cache(
      prepInputs,
      url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip",
      destinationPath = hydro_dir,
      archive = "HydroRIVERS_v10_na_shp.zip",
      targetFile = "HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp",
      fun = terra::vect,
      cropTo = studyArea,
      projectTo = studyArea
    )
    
    ## 2) Buffer streams
    buf_m <- P(sim)$riparianBuffer_m
    streams_buf <- terra::buffer(streams, width = buf_m)
    
    ## 3) CREATE COARSE TEMPLATE (KEY FIX)
    hydro_res <- P(sim)$hydroRaster_m
    
    ## 3) CREATE COARSE TEMPLATE
    template <- terra::rast(
      terra::ext(terra::vect(studyArea)),
      resolution = hydro_res,
      crs = terra::crs(sim$LandCover)
    )
    
    ## 🔴🔴🔴 THIS IS THE CRITICAL FIX 🔴🔴🔴
    values(template) <- 0
    
    
    
    # rasterize buffer: area-weighted
    riparian_area <- terra::rasterize(
      streams_buf,
      template,
      fun = "sum",
      background = 0
    )
    
    # cell area (m²)
    cell_area <- prod(res(template))
    
    # fraction
    riparian_frac <- riparian_area / cell_area
    riparian_frac[riparian_frac > 1] <- 1
    
    
    
    ## 5) Store hydrology
    sim$Hydrology <- list(
      source = "HydroRIVERS_v10_na",
      buffer_m = buf_m,
      raster_m = hydro_res,
      riparianFraction = riparian_frac
    )
    
    
    message(
      "✔ Hydrology riparian fraction created (",
      hydro_res, " m resolution)"
    )
  }
  
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
