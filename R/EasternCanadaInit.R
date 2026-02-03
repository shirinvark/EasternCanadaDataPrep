EasternCanadaInit <- function(sim) {
  
  message("🔵 init: building Eastern Canada spatial products")
  
  ## land cover must come from upstream
  if (is.null(sim$rstLCC)) {
    stop(
      "rstLCC is missing.\n",
      "EasternCanadaDataPrep expects land cover from an upstream module ",
      "(e.g., Biomass_borealDataPrep)."
    )
  }
  
  sim <- buildPlanningGrid(sim)
  sim <- buildProvinces(sim)
  
  invisible(sim)
}
