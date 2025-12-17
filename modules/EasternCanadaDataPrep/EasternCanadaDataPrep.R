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
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
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
      "hydroBuffer_m",
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
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "EasternCanadaLandbase", objectClass = "list", desc = "A list containing all processed and clipped layers (FMU, CPCAD, Hydrology, Landcover) for Eastern Canada.")
  )
))

### template initialization
doEvent.EasternCanadaDataPrep <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(sim, time(sim), "EasternCanadaDataPrep", "buildLandbase")
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
      HLB       = NULL
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
  cpcad_i <- terra::intersect(cpcad, fmu_i)
  hlb <- terra::erase(fmu_i, cpcad_i)
  
  hlb <- terra::erase(hlb, sim$Hydrology$buffered$flowline)
  hlb <- terra::erase(hlb, sim$Hydrology$buffered$watercourse)
  hlb <- terra::erase(hlb, sim$Hydrology$buffered$waterbody)
  
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
  ## 5) Hydrology (NHN – SAFE MODE)
  ## ---------------------------------------------------------
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
  ## 5) Hydrology (NHN – LOAD ONLY)
  ## ---------------------------------------------------------
  
  if (is.null(sim$Hydrology)) {
    
    message("▶ Loading prepared Hydrology layers...")
    
    hydro_dir <- file.path(dPath, "Hydrology")
    
    ## 1) Extract only (NO LOAD)
    Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1_isixvFVMAUThrt8r6gQ6T1lJWLMM3Mh",
      destinationPath = hydro_dir,
      archive = "ONTARIO_FMU.zip",
      overwrite = FALSE,
      purge = 7,
      fun = NULL
    )
    
    ## 2) Load prepared vectors
    flowline <- terra::vect(
      file.path(hydro_dir, "ONTARIO_FMU", "FLOWLiNE_FMUs.shp")
    )
    
    watercourse <- terra::vect(
      file.path(hydro_dir, "ONTARIO_FMU", "Watercourse_FMUs.shp")
    )
    
    waterbody <- terra::vect(
      file.path(hydro_dir, "ONTARIO_FMU", "WaterBodyEasternOntario.shp")
    )
    
    ## 3) Reproject ONLY (no crop)
    targetCRS <- sf::st_crs(studyArea)$wkt
    
    flowline    <- terra::project(flowline, targetCRS)
    watercourse <- terra::project(watercourse, targetCRS)
    waterbody   <- terra::project(waterbody, targetCRS)
    
    ## 4) Apply buffer (using module parameter)
    buf <- as.numeric(sim$hydroBuffer_m)
    if (length(buf) == 0 || is.na(buf)) buf <- 30
    
    flowline_buf    <- terra::buffer(flowline,    width = buf)
    watercourse_buf <- terra::buffer(watercourse, width = buf)
    waterbody_buf   <- terra::buffer(waterbody,   width = buf)
    
    ## 5) Store raw + buffered hydrology in sim
    sim$Hydrology <- list(
      raw = list(
        flowline    = flowline,
        watercourse = watercourse,
        waterbody   = waterbody
      ),
      buffered = list(
        flowline    = flowline_buf,
        watercourse = watercourse_buf,
        waterbody   = waterbody_buf
      )
    )
    
    message("✔ Hydrology loaded and buffered.")
    
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
