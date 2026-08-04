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
  sim <- buildJurisdiction(sim)
  sim <- buildOwnership(sim)
  sim <- buildBCR(sim)
  
  invisible(sim)
}