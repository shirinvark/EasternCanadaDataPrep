#------------------------------------------------------------------------------
# Build PlanningGrid
#
# Creates the analysis grid and aligns all core raster/vector inputs to the
# common spatial framework used by downstream modules.
#------------------------------------------------------------------------------
buildPlanningGrid <- function(sim) {
  # browser()
  
  message("Building PlanningGrid and aligning layers...")
  
  study_v <- sim$studyArea
  
  if (!inherits(study_v, "SpatVector")) {
    study_v <- terra::vect(study_v)
  }
  
  targetRes <- terra::res(sim$rasterToMatch)[1]
  ext <- terra::ext(study_v)
  
  width  <- ext[2] - ext[1]
  height <- ext[4] - ext[3]
  ncol <- width / targetRes
  nrow <- height / targetRes
  
  message("Estimated columns: ", round(ncol))
  message("Estimated rows: ", round(nrow))
  message("Estimated total cells: ", round(ncol * nrow))
  
  # ---------------------------------------------------------
  # 2) Align LandCover
  # ---------------------------------------------------------
  
  lc_src <- sim$LandCover  
  message("LandCover ncell BEFORE crop: ", terra::ncell(lc_src))
  
  #  crop (safe)
  if (!terra::same.crs(study_v, lc_src)) {
    study_v_original_crs <- terra::project(study_v, terra::crs(lc_src))
  } else {
    study_v_original_crs <- study_v
  }
  
  message("STARTING EXTENT CROP")
  
  lc_src <- terra::crop(
    lc_src,
    terra::ext(study_v_original_crs),
    snap = "out"
  )
  
  message("EXTENT CROP FINISHED")
  
  
  if (is.null(lc_src) || terra::ncell(lc_src) == 0) {
    stop("crop produced empty raster.")
  }
  
  #  project to analysis CRS
  planning_template <- sim$rasterToMatch 
  message("STARTING PROJECT/RESAMPLE")
  message("STARTING PRE-AGGREGATION")
  
  
  res_lc <- terra::res(lc_src)[1]
  
  fact <- floor(targetRes / res_lc)
  
  if (is.na(fact) || fact < 1) {
    fact <- 1
  }  
  
  if (fact > 1) {
    
    lc_src <- terra::aggregate(
      lc_src,
      fact = fact,
      fun = terra::modal,
      na.rm = TRUE
    )
    
  }
  
  message("PRE-AGGREGATION FINISHED")
  sim$LandCover <- terra::project(
    lc_src,
    planning_template,
    method = "near",
    mask = TRUE
  )
  
  message("PROJECT/RESAMPLE FINISHED")
  
  # ---------------------------------------------------------
  # 3) FINAL Planning Grid
  # ---------------------------------------------------------
  
  message("Building PlanningGrid from studyArea")
  sim$PlanningGrid <- sim$rasterToMatch
  
  terra::values(sim$PlanningGrid) <- 1
  

  return(invisible(sim))
}