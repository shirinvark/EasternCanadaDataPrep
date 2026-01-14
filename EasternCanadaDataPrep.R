## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "EasternCanadaDataPrep",
  description = "Cell-based landbase accounting table defining effective harvestable area after legal (protected areas) and riparian constraints.",
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
      "hydroRaster_m",
      "numeric",
      250,
      250,
      250,
      "Resolution (m) of hydrology raster template"
    )
    
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("studyArea", objectClass = c("sf", "SpatVector"), desc = "Study area polygon used for cropping and masking", sourceURL = NA),
    expectsInput("CPCAD",objectClass = c("sf", "SpatVector"), desc = "CPCAD protected areas — generated inside this module", sourceURL = NA),
    expectsInput("FMU",objectClass = c("sf", "SpatVector"), desc = "Forest Management Units — generated inside this module",sourceURL = NA),
    expectsInput(
      "Hydrology",
      objectClass = "list",
      desc = "Hydrology data including riparianFraction raster",
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

doEvent.EasternCanadaDataPrep <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      message("🔵 init: building landbase")
      sim <- buildLandbaseRaster(sim)
    }
    ,
    
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

buildLandbaseRaster <- function(sim) {
  
  message("🔵 Building planning raster & landbase accounting table...")
  
  ## -----------------------------
  ## sanity checks
  ## -----------------------------
  if (is.null(sim$FMU))
    stop("FMU is missing.")
  
  if (is.null(sim$CPCAD))
    stop("CPCAD is missing.")
  
  if (is.null(sim$Hydrology$riparianFraction))
    stop("Hydrology riparianFraction is missing.")
  
  ## -----------------------------
  ## inputs
  ## -----------------------------
  fmu      <- sim$FMU
  cpcad    <- sim$CPCAD
  riparian <- sim$Hydrology$riparianFraction
  
  res_m <- P(sim)$hydroRaster_m
  
  ## -----------------------------
  ## 1) planning raster (NO land cover)
  ## -----------------------------
  planning <- terra::rast(
    sim$studyArea,
    resolution = res_m,
    crs = terra::crs(fmu)
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
  
  fmu_r <- terra::rasterize(
    fmu,
    planning,
    field = "FMU_ID",
    touches = TRUE
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
  harvestable_mask <- terra::ifel(
    !is.na(fmu_r) & prot_r == 0,
    1,
    0
  )
  
  ## -----------------------------
  ## 5) align riparian fraction
  ## -----------------------------
  rip_r <- terra::project(
    riparian,
    planning,
    method = "bilinear"
  )
  
  rip_r[rip_r < 0] <- 0
  rip_r[rip_r > 1] <- 1
  rip_r[is.na(rip_r)] <- 0
  
  ## -----------------------------
  ## diagnostics
  ## -----------------------------
  message("---- DIAGNOSTIC CHECK ----")
  message("planning cells  = ", terra::ncell(planning))
  message("FMU NA cells    = ", sum(is.na(terra::values(fmu_r))))
  message("Protected cells = ", sum(terra::values(prot_r) == 1, na.rm = TRUE))
  message("---- END DIAGNOSTIC ----")
  
  ## -----------------------------
  ## 6) landbase accounting table
  ## -----------------------------
  harv_vals <- terra::values(harvestable_mask)
  
  idx <- harv_vals == 1
  
  if (sum(idx, na.rm = TRUE) == 0) {
    stop("No harvestable cells remain after constraints.")
  }
  
  
  
  cell_area <- prod(terra::res(planning))  # m2
  
  df <- data.frame(
    FMU               = terra::values(fmu_r)[idx],
    riparian_fraction = terra::values(rip_r)[idx]
  )
  
  df$riparian_fraction[is.na(df$riparian_fraction)] <- 0
  df$riparian_fraction <- pmin(pmax(df$riparian_fraction, 0), 1)
  
  df$cell_area_m2   <- cell_area
  df$effective_area <- cell_area * (1 - df$riparian_fraction)
  
  sim$LandbaseTable <- df
  
  message(
    "✔ LandbaseTable created | mean effective area (ha): ",
    round(mean(df$effective_area) / 10000, 3)
  )
  
  ## -----------------------------
  ## 7) assemble output object
  ## -----------------------------
  sim$EasternCanadaLandbase <- list(
    PlanningRaster     = planning,
    FMU_raster         = fmu_r,
    HarvestableMask    = harvestable_mask,
    RiparianFraction   = rip_r,
    LandbaseTable      = df
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
  
  terra::writeRaster(
    rip_r,
    file.path(out_dir, "RiparianFraction_250m.tif"),
    overwrite = TRUE,
    datatype = "FLT4S"
  )
  
  message("💾 Output rasters written to: ", out_dir)
  
  return(invisible(sim))
}

###########################

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
      fun = "terra::vect",
      layer = "ProtectedConservedArea_2024 ProtectedConservedArea_2024",
      cropTo    = studyArea_sf,
      maskTo    = studyArea_sf,
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
      fun = "terra::vect",
      cropTo    = studyArea_sf,
      maskTo    = studyArea_sf,
      projectTo = studyArea_sf
    )
  }
  
  if (!terra::same.crs(sim$FMU, studyArea_v)) {
    sim$FMU <- terra::project(sim$FMU, studyArea_v)
  }
  
  ## ---------------------------------------------------------
  ## 4) Hydrology – HydroRIVERS → riparianFraction
  ## ---------------------------------------------------------
  if (is.null(sim$Hydrology)) {
    
    message("▶ Preparing Hydrology from HydroRIVERS...")
    
    hydro_dir <- file.path(dPath, "Hydrology")
    dir.create(hydro_dir, recursive = TRUE, showWarnings = FALSE)
    
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
    
    buf_m <- P(sim)$riparianBuffer_m
    streams_buf <- terra::buffer(streams, width = buf_m)
    
    template <- terra::rast(
      studyArea_v,
      resolution = P(sim)$hydroRaster_m,
      crs = terra::crs(sim$FMU)
    )
    values(template) <- 0
    
    riparian_area <- terra::rasterize(
      streams_buf,
      template,
      fun = "sum",
      background = 0
    )
    
    cell_area <- prod(res(template))
    riparian_frac <- riparian_area / cell_area
    riparian_frac[riparian_frac > 1] <- 1
    
    sim$Hydrology <- list(
      source = "HydroRIVERS_v10_na",
      buffer_m = buf_m,
      raster_m = P(sim)$hydroRaster_m,
      riparianFraction = riparian_frac
    )
    
    message("✔ Hydrology riparian fraction created.")
  }
  
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}
