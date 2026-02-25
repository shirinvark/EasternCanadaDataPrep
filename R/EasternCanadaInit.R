EasternCanadaInit <- function(sim) {
  
  message("🔵 init: building Eastern Canada spatial products")
  
  ## --------------------------------------------------------
  ## sanity checks (fail early, fail loud)
  ## -------------------------------------------------------
  stopifnot(
    !is.null(sim$studyArea),
    !is.null(sim$FMU),
    !is.null(sim$CPCAD),
    !is.null(sim$LandCover),
    !is.null(sim$standAgeMap)
  )
  
  ## ---------------------------------------------------------
  ## Build Planning Grid & align layers
  ## ---------------------------------------------------------
  sim <- buildPlanningGrid(sim)
  
  invisible(sim)
}