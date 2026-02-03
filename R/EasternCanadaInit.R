EasternCanadaInit <- function(sim) {

  message("🔵 init: building Eastern Canada spatial products")

  ## land cover must come from upstream
  if (is.null(sim$rstLCC)) {
    stop(
      "rstLCC is missing.\n",
      "EasternCanadaDataPrep expects land cover from an upstream module."
    )
  }

  ## 1) build planning grid first
  sim <- buildPlanningGrid(sim)

  ## 2) Land cover: ONLY crop + CRS/grid alignment
  lc <- sim$rstLCC

  # crop + mask to study area
  lc <- terra::crop(lc, sim$studyArea)
  lc <- terra::mask(lc, sim$studyArea)

  # align to PlanningRaster (CRS + resolution)
  lc <- terra::project(lc, sim$PlanningRaster, method = "near")
  lc <- terra::resample(lc, sim$PlanningRaster, method = "near")

  sim$LandCoverAligned <- lc

  ## 3) provinces (independent of land cover)
  sim <- buildProvinces(sim)

  invisible(sim)
}
