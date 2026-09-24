EasternCanadaInit <- function(sim) {
  message("init: building Eastern Canada spatial products")
  
  stopifnot(
    !is.null(sim$studyArea),
    !is.null(sim$FMU),
    !is.null(sim$CPCAD)
  )
  
  if (is.null(sim$rasterToMatch)) {
    
    message("No rasterToMatch supplied. Creating default 240 m rasterToMatch.")
    
    sim$rasterToMatch <- terra::rast(
      ext = terra::ext(sim$studyArea),
      resolution = 240,
      crs = terra::crs(sim$studyArea)
    )
  }
  
  sim <- buildPlanningGrid(sim)
  sim <- buildSYU(sim)
  message("Aligning LandCover to PlanningGrid...")
  
  stopifnot(inherits(sim$LandCover, "SpatRaster"))
  stopifnot(inherits(sim$PlanningGrid, "SpatRaster"))
  
  if (!terra::same.crs(sim$LandCover, sim$PlanningGrid)) {
    
    sim$LandCover <- terra::project(
      sim$LandCover,
      sim$PlanningGrid,
      method = "near"
    )
  }
  
  sim$LandCover <- terra::resample(
    sim$LandCover,
    sim$PlanningGrid,
    method = "near"
  )
  
  names(sim$LandCover) <- "landCover"
  
  message("LandCover aligned to PlanningGrid.")
  
  sim <- buildJurisdiction(sim)
  sim <- buildDMFL(sim)
  sim <- buildYieldCurveFamily(sim)
  sim <- buildOwnership(sim)
  
  # ---------------------------------------------------------
  # Align Ownership to PlanningGrid
  # ---------------------------------------------------------
  
  message("Aligning Ownership to PlanningGrid...")
  
  stopifnot(
    inherits(sim$Ownership, "SpatRaster"),
    inherits(sim$PlanningGrid, "SpatRaster")
  )
  
  if (!terra::same.crs(sim$Ownership, sim$PlanningGrid)) {
    
    sim$Ownership <- terra::project(
      sim$Ownership,
      sim$PlanningGrid,
      method = "near"
    )
    
  } else if (!terra::compareGeom(
    sim$Ownership,
    sim$PlanningGrid,
    stopOnError = FALSE
  )) {
    
    sim$Ownership <- terra::resample(
      sim$Ownership,
      sim$PlanningGrid,
      method = "near"
    )
  }
  
  names(sim$Ownership) <- "Ownership"
  
  stopifnot(
    terra::compareGeom(
      sim$Ownership,
      sim$PlanningGrid,
      stopOnError = FALSE
    )
  )
  
  message("Ownership aligned to PlanningGrid.")
  
  sim <- buildProtectedAreas(sim)
  sim <- buildBCR(sim)
  
  invisible(sim)
}


