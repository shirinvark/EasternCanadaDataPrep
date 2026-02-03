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
  ## LandCover: فقط crop + CRS/grid alignment
lc <- sim$LandCover
lc <- terra::crop(lc, sim$studyArea)
lc <- terra::mask(lc, sim$studyArea)
lc <- terra::project(lc, sim$PlanningRaster, method = "near")
lc <- terra::resample(lc, sim$PlanningRaster, method = "near")
sim$LandCoverAligned <- lc
  sim <- buildProvinces(sim)
  
  invisible(sim)
}
