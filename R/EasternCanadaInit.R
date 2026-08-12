EasternCanadaInit <- function(sim) {
  
  message("🔵 init: building Eastern Canada spatial products")
  
  stopifnot(
    !is.null(sim$studyArea),
    !is.null(sim$FMU),
    !is.null(sim$CPCAD)
  )
  
  if (is.null(sim$rasterToMatch)) {
    
    message("No rasterToMatch supplied. Creating default 240 m rasterToMatch.")
    
    study_v <- if (inherits(sim$studyArea, "SpatVector")) {
      sim$studyArea
    } else {
      terra::vect(sim$studyArea)
    }
    
    sim$rasterToMatch <- terra::rast(
      ext = terra::ext(study_v),
      resolution = 240,
      crs = terra::crs(study_v)
    )
  }
  
  sim <- buildPlanningGrid(sim)
  
  message("▶ Aligning LandCover to PlanningGrid...")
  
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
  
  message("✔ LandCover aligned to PlanningGrid.")
  
  sim <- buildJurisdiction(sim)
  sim <- buildYieldCurveFamily(sim)
  sim <- buildOwnership(sim)
  sim <- buildProtectedAreas(sim)
  sim <- buildBCR(sim)
  
  invisible(sim)
}
    
    
