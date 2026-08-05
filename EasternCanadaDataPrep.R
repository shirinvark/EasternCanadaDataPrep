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
    expectsInput(
      "BCR",
      objectClass = c("sf", "SpatVector"),
      desc = "Bird Conservation Regions",
      sourceURL = NA
    ),
    expectsInput(
      "Ownership",
      objectClass = "SpatRaster",
      desc = "National ownership raster",
      sourceURL = NA
    ),
    
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
    expectsInput(
      "Jurisdiction",
      objectClass = c("sf", "SpatVector"),
      desc = "Province / State polygons"
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
  
  
  if ("IUCN_CAT" %in% names(cpcad))
    cpcad <- cpcad[cpcad$IUCN_CAT %in% c(1, 2, 3, 4, 5, 6), ]
  
  sim$CPCAD <- cpcad
  
  if (!terra::same.crs(sim$CPCAD, studyArea_v)) {
    sim$CPCAD <- terra::project(sim$CPCAD, studyArea_v)
  }
  
  message("✔ CPCAD ready. Features: ", nrow(sim$CPCAD))
  
  
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
  ## ---------------------------------------------------------
  ## 4) BCR – Bird Conservation Regions
  ## ---------------------------------------------------------
  
  if (!SpaDES.core::suppliedElsewhere("BCR")) {
    
    message("▶ Preparing BCR...")
    
    bcr_dir <- file.path(dPath, "BCR")
    
    ## اگر قبلاً extract شده باشد
    gdb <- list.dirs(
      bcr_dir,
      recursive = FALSE,
      full.names = TRUE
    )
    gdb <- gdb[grepl("\\.gdb$", gdb)]
    
    ## اگر هنوز وجود ندارد، دانلود و extract
    if (length(gdb) == 0) {
      
      prepInputs(
        url = "https://drive.google.com/uc?export=download&id=18pnd5-qDDwTmgHN2NxyU_VyBP3tk9R97",
        destinationPath = bcr_dir,
        fun = NA,
        verbose = 1
      )
      
      gdb <- list.dirs(
        bcr_dir,
        recursive = FALSE,
        full.names = TRUE
      )
      gdb <- gdb[grepl("\\.gdb$", gdb)]
    }
    
    stopifnot(length(gdb) == 1)
    
    sim$BCR <- terra::vect(
      gdb,
      layer = "BCR_Terrestrial_Master"
    )
    
    sim$BCR <- terra::crop(sim$BCR, studyArea_sf)
    sim$BCR <- terra::project(sim$BCR, terra::crs(studyArea_sf))
  }
  ## ---------------------------------------------------------
  ## 5) Jurisdiction – Administrative boundaries
  ## ---------------------------------------------------------
  
  if (!SpaDES.core::suppliedElsewhere("Jurisdiction")) {
    
    message("▶ Preparing Jurisdiction...")
    
    sim$Jurisdiction <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1rJQCUJXN3m0pGBGo-bmf4qDfiZbCAg1p",
      destinationPath = file.path(dPath, "Jurisdiction"),
      targetFile = file.path(
        "politicalboundaries_shapefile",
        "NA_PoliticalDivisions",
        "data",
        "boundaries_p_2021_v3.shp"
      ),
      fun = terra::vect,
      cropTo = studyArea_sf,
      projectTo = studyArea_sf
    )
    
  }
  
  message("✔ Jurisdiction ready. Features: ", nrow(sim$Jurisdiction))
  
  ## ---------------------------------------------------------
  ## 6) Ownership – National ownership layer
  ## ---------------------------------------------------------
  
  if (!SpaDES.core::suppliedElsewhere("Ownership")) {
    
    message("▶ Preparing Ownership...")
    
    ownership_dir <- file.path(dPath, "Ownership")
    
    sim$Ownership <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1dntQglGsEm6cpJSsPAmwtiPYQxZuNX-D",
      destinationPath = ownership_dir,
      targetFile = "Ownership.tif",
      fun = terra::rast,
      cropTo = studyArea_sf,
      projectTo = studyArea_sf
    )
    
  }
  
  if (!terra::same.crs(sim$Ownership, studyArea_v)) {
    
    sim$Ownership <- terra::project(
      sim$Ownership,
      studyArea_v
    )
    
  }
  
  message("✔ Ownership ready.")
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
  # =========================================================
  # 3) standAge (optional)
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
      
      message("ℹ No standAge available. Continuing without standAge.")
      
      sim$standAge <- NULL
    }
  }
  
  return(invisible(sim))
  
}  # end .inputObjects

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
