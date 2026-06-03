EasternCanadaInit <- function(sim) {
  
  message("🔵 init: building Eastern Canada spatial products")
  
  stopifnot(
    !is.null(sim$studyArea),
    !is.null(sim$FMU),
    !is.null(sim$CPCAD)
  )
  
  # standalone fallback
  if (is.null(sim$LandCover) &&
      !is.null(sim$LandCover_250m)) {
    
    sim$LandCover <- sim$LandCover_250m
  }
  
  if (is.null(sim$standAge_250m) &&
      !is.null(sim$standAge_250m)) {
    
    sim$standAge_250m <- sim$standAge_250m
  }
  
  stopifnot(
    !is.null(sim$LandCover),
    !is.null(sim$standAge_250mp)
  )
  
  sim <- buildPlanningGrid(sim)
  
  invisible(sim)
}