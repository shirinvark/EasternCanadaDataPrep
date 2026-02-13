EasternCanadaInit <- function(sim) {
  
  message("🔵 init: building Eastern Canada spatial products")
  
  ## ---------------------------------------------------------
  ## sanity checks (fail early, fail loud)
  ## ---------------------------------------------------------
  stopifnot(
    !is.null(sim$studyArea),
    !is.null(sim$FMU),
    !is.null(sim$CPCAD)
  )
  
  ## ---------------------------------------------------------
  ## 1) Jurisdictional layer (lightweight, vector)
  ## ---------------------------------------------------------
  sim <- buildProvinces(sim)
  
  ## ---------------------------------------------------------
  ## 2) Planning grid & landbase (heavy raster work)
  ## ---------------------------------------------------------
  sim <- PlanningGrid_250m(sim)
  
  invisible(sim)
}
