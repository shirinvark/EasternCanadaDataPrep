## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
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
    
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("studyArea", objectClass = c("sf", "SpatVector"), desc = "Study area polygon used for cropping and masking", sourceURL = NA),
    expectsInput("CPCAD",objectClass = c("sf", "SpatVector"), desc = "CPCAD protected areas — generated inside this module", sourceURL = NA),
    expectsInput("FMU",objectClass = c("sf", "SpatVector"), desc = "Forest Management Units — generated inside this module",sourceURL = NA),
    expectsInput(
      "rstLCC",
      objectClass = "SpatRaster",
      desc = "Classified land cover raster (upstream)(forest / non-forest)",
      sourceURL = NA
    )
  ),
  outputObjects = bindrows(
    
    createsOutput(
      objectName = "EasternCanadaLandbase",
      objectClass = "list",
      desc = "A list containing spatial constraints and derived landbase products for Eastern Canada."
    ),
    createsOutput(
      objectName  = "Provinces",
      objectClass = c("sf", "SpatVector"),
      desc        = "Canadian provincial boundaries (ON, QC, NB, NS, PE, NL) cropped to study area"
    ),createsOutput(
      objectName  = "Hydrology_streams",
      objectClass = "SpatVector",
      desc        = "Raw stream network from HydroRIVERS"
    ),
    
    createsOutput(
      objectName  = "Hydrology_lakes",
      objectClass = "SpatVector",
      desc        = "Raw lake polygons from HydroLAKES"
    ),
    
    createsOutput(
      objectName  = "Hydrology_basins",
      objectClass = "SpatVector",
      desc        = "HydroBASINS level 8 polygons"
    ),
    
    
    createsOutput(
      objectName = "PlanningRaster",
      objectClass = "SpatRaster",
      desc = "Planning raster used for landbase accounting and downstream AAC calculations."
    )
    
  )
  
))

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

## Build the planning raster and core landbase components.
##
## This function establishes the spatial analysis grid and
## derives legal/managerial constraints (FMUs, protected areas).
##
## Importantly:
## - No ecological interpretation is performed here
## - No harvest or policy decisions are applied
## - Outputs are intended for reuse by multiple downstream modules

buildPlanningGrid <- function(sim) {
  
  message("🔵 Building planning raster & landbase accounting table...")
  
  ## -----------------------------
  ## sanity checks
  ## -----------------------------
  if (is.null(sim$FMU))
    stop("FMU is missing.")
  
  if (is.null(sim$CPCAD))
    stop("CPCAD is missing.")
  
  ## -----------------------------
  ## inputs
  ## -----------------------------
  fmu      <- sim$FMU
  cpcad    <- sim$CPCAD
  ## -----------------------------
  ## 1) planning raster (NO land cover)
  ## -----------------------------
  study_v <- terra::vect(sim$studyArea)
  
  planning <- terra::rast(
    study_v,
    resolution = 250,
    crs = terra::crs(study_v)
  )
  
  
  values(planning) <- NA
  
  sim$PlanningRaster <- planning
  
  ## -----------------------------
  ## 2) rasterize FMU
  ## -----------------------------
  if (!"FMU_ID" %in% names(fmu)) {
    fmu$FMU_ID <- seq_len(nrow(fmu))
  }
  
  if (!terra::same.crs(fmu, planning)) {
    fmu <- terra::project(fmu, planning)
  }
  
  if (!terra::same.crs(cpcad, planning)) {
    cpcad <- terra::project(cpcad, planning)
  }
  
  # Raster–polygon assignment uses the center-of-pixel rule:
  # a cell is assigned to an FMU only if its cell center falls within the FMU polygon.
  fmu_r <- terra::rasterize(
    fmu,
    planning,
    field = "FMU_ID",
    touches = FALSE
  )
  
  if (all(is.na(terra::values(fmu_r)))) {
    stop(
      "FMU rasterization failed: all cells are NA.\n",
      "Check CRS, extent, or resolution."
    )
  }
  
  ## -----------------------------
  ## 3) rasterize protected areas
  ## -----------------------------
  prot_r <- terra::rasterize(
    cpcad,
    planning,
    field = 1,
    background = 0
  )
  
  if (all(is.na(terra::values(prot_r)))) {
    message("ℹ️ No CPCAD intersects planning raster — protected set to 0")
    terra::values(prot_r) <- 0
  }
  
  ## -----------------------------
  ## 4) harvestable mask (LEGAL / MANAGERIAL)
  ## -----------------------------
  ## This mask reflects only legal and administrative constraints
  ## (FMU presence and protected areas).
  ##
  ## It does NOT represent ecological suitability, operability,
  ## or harvest decisions. Those are deferred to downstream modules.
  harvestable_mask <- terra::ifel(
    !is.na(fmu_r) & prot_r == 0,
    1,
    0
  )
  
  ## -----------------------------
  ## diagnostics
  ## -----------------------------
  message("---- DIAGNOSTIC CHECK ----")
  message("planning cells  = ", terra::ncell(planning))
  message("FMU NA cells    = ", sum(is.na(terra::values(fmu_r))))
  message("Protected cells = ", sum(terra::values(prot_r) == 1, na.rm = TRUE))
  message("---- END DIAGNOSTIC ----")
  
  
  ## -----------------------------
  ## 7) assemble output object
  ## -----------------------------
  sim$EasternCanadaLandbase <- list(
    PlanningRaster     = planning,
    FMU_raster         = fmu_r,
    HarvestableMask    = harvestable_mask
    # LandCover and forest masks are provided by downstream DataPrep modules
  )
  
  ## -----------------------------
  ## 8) save rasters
  ## -----------------------------
  out_dir <- file.path(outputPath(sim), "EasternCanadaDataPrep")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  terra::writeRaster(
    planning,
    file.path(out_dir, "PlanningRaster_250m.tif"),
    overwrite = TRUE
  )
  
  terra::writeRaster(
    harvestable_mask,
    file.path(out_dir, "HarvestableMask_250m.tif"),
    overwrite = TRUE,
    datatype = "INT1U"
  )
  
  message("💾 Output rasters written to: ", out_dir)
  
  return(invisible(sim))
}
## Build provincial boundaries for the study area.
##
## This function exists to provide a clean, explicit
## jurisdictional layer for downstream modules.
##
## Provincial boundaries are NOT used here for decisions,
## but allow other modules (e.g., hydrology, landbase policy)
## to apply province-specific rules in a transparent way.

buildProvinces <- function(sim) {
  
  message("🔵 Building Provinces layer...")
  
  ## 1) Provincial boundaries
  prov <- rnaturalearth::ne_states(
    country = "Canada",
    returnclass = "sf"
  )
  
  ## 2) Only eastern provinces
  prov <- prov[prov$name_en %in% c(
    "Ontario",
    "Quebec",
    "New Brunswick",
    "Nova Scotia",
    "Prince Edward Island",
    "Newfoundland and Labrador"
  ), ]
  
  ## 3) Province codes (jurisdiction)
  ## Explicit short codes for downstream policy joins
  prov$jurisdiction <- c("ON", "QC", "NB", "NS", "PE", "NL")
  
  ## 4) Reproject to study area CRS
  prov <- sf::st_transform(prov, sf::st_crs(sim$studyArea))
  
  ## 5) Clip to study area
  prov <- sf::st_intersection(prov, sim$studyArea)
  
  ## 6) Keep only required attribute(s) BEFORE conversion
  prov <- prov[, "jurisdiction", drop = FALSE]
  
  ## 7) Convert to SpatVector (SpaDES standard)
  sim$Provinces <- terra::vect(prov)
  
  # Provinces are a data source;
  # jurisdiction is the abstract policy interface exposed downstream
  sim$jurisdiction <- sim$Provinces
  
  ## 8) Message (FIXED)
  message(
    "✔ Provinces ready: ",
    paste(unique(sim$Provinces$jurisdiction), collapse = ", ")
  )
  
  return(invisible(sim))
}

###########################
## NOTE:
## This module is responsible for preparing spatial inputs only.
## No policy interpretation or landbase decisions are made here.
##
## The Provinces object is produced solely to enable
## jurisdiction-aware processing in downstream modules
## (e.g., province-based riparian policies in EasternCanadaHydrology).
## This module does not apply or interpret those policies.

.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  ## ---------------------------------------------------------
  ## 1) Create studyArea if not provided by user
  ## ---------------------------------------------------------
  if (is.null(sim$studyArea)) {
    
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
  if (is.null(sim$CPCAD)) {
    
    cpcad_dir <- file.path(dPath, "CPCAD")
    dir.create(cpcad_dir, recursive = TRUE, showWarnings = FALSE)
    
    message("▶ Preparing CPCAD...")
    
    sim$CPCAD <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1ELIaRgO5PNgliGh0Tq2BI6V5654ydxYu",
      destinationPath = cpcad_dir,
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
  if (is.null(sim$FMU)) {
    
    fmu_dir <- file.path(dPath, "FMU")
    dir.create(fmu_dir, recursive = TRUE, showWarnings = FALSE)
    
    message("▶ Preparing FMU...")
    
    sim$FMU <- Cache(
      prepInputs,
      url = "https://drive.google.com/uc?export=download&id=1qp4TRgFArANp1YNEoOpeuwLlM-khf4v1",
      destinationPath = fmu_dir,
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
  if (!is.null(sim$rstLCC)) {
    
    lcc <- sim$rstLCC
    
    if (!terra::same.crs(lcc, studyArea_v)) {
      lcc <- terra::project(lcc, studyArea_v)
    }
    
    lcc <- terra::crop(lcc, studyArea_v)
    
    sim$rstLCC <- lcc
  }
  
  ## ---------------------------------------------------------
  ## 4) Hydrology – HydroRIVERS + HydroLAKES
  ## Raw hydrology inputs only (no buffering, no policy)
  ## ---------------------------------------------------------
  ## ---------------------------------------------------------
  ## 4) Hydrology – HydroRIVERS + HydroLAKES + HydroBASINS
  ## Raw hydrology inputs only (no buffering, no policy)
  ## ---------------------------------------------------------
  
  if (is.null(sim$Hydrology)) {
    
    message("▶ Preparing Hydrology from HydroRIVERS, HydroLAKES, and HydroBASINS...")
    
    hydro_dir <- file.path(dPath, "Hydrology")
    dir.create(hydro_dir, recursive = TRUE, showWarnings = FALSE)
    
    ## ---- Streams (HydroRIVERS) ---
    streams <- Cache(
      prepInputs,
      url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_na_shp.zip",
      destinationPath = hydro_dir,
      archive = "HydroRIVERS_v10_na_shp.zip",
      targetFile = "HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp",
      fun = terra::vect,
      cropTo = studyArea_sf,
      projectTo = studyArea_sf
    )
    
    ## ---- Lakes (HydroLAKES) ----
    lakes <- Cache(
      prepInputs,
      url = "https://data.hydrosheds.org/file/hydrolakes/HydroLAKES_polys_v10_shp.zip",
      destinationPath = hydro_dir,
      archive = "HydroLAKES_polys_v10_shp.zip",
      targetFile = "HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp",
      fun = terra::vect,
      cropTo = studyArea_sf,
      projectTo = studyArea_sf
    )
    
    ## ---- Basins (HydroBASINS – Level 8, North America) ----
    basins <- Cache(
      prepInputs,
      url = "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_na_lev08_v1c.zip",
      destinationPath = hydro_dir,
      archive = "hybas_na_lev08_v1c.zip",
      targetFile = "hybas_na_lev08_v1c/hybas_na_lev08_v1c.shp",
      fun = terra::vect,
      cropTo = studyArea_sf,
      projectTo = studyArea_sf
    )
    
    ## ---- Assemble hydrology object (internal structure) ----
    ## Internal grouping only — downstream modules use exposed objects
    
    sim$Hydrology <- list(
      source  = c(
        "HydroRIVERS_v10_na",
        "HydroLAKES_v10",
        "HydroBASINS_v1c_lev08"
      ),
      streams = streams,
      lakes   = lakes,
      basins  = basins
    )
    
    ## ---- Expose components for downstream modules (interface) ---
    ## Required by RiparianBuffers
    sim$Hydrology_streams <- streams
    sim$Hydrology_lakes   <- lakes
    sim$Hydrology_basins  <- basins
    
    message(
      "✔ Hydrology ready (raw geometry): ",
      nrow(streams), " stream features, ",
      nrow(lakes), " lake features, ",
      nrow(basins), " basin polygons."
    )
  }
  
  return(invisible(sim))
  
}
## Summary:
## EasternCanadaDataPrep standardizes spatial inputs and
## exposes clean, reusable objects for downstream analysis.
##
## Policy interpretation, ecological modeling, and harvest
## decisions are intentionally excluded from this module.

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
