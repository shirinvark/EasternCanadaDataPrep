EasternCanadaInit <- function(sim) {
  
  message("🔵 init: building Eastern Canada spatial products")
  
  ## Core spatial products
  sim <- buildPlanningGrid(sim)
  
  ## Jurisdictional layer for downstream policy modules
  sim <- buildProvinces(sim)
  
  invisible(sim)
}
