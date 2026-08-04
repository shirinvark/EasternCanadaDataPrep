#------------------------------------------------------------------------------
# Build PlanningGrid
#
# Creates the analysis grid and aligns all core raster/vector inputs to the
# common spatial framework used by downstream modules.
#------------------------------------------------------------------------------
buildPlanningGrid <- function(sim) {
  # browser()
  
  message("🔵 Building PlanningGrid and aligning layers...")
  
  if (inherits(sim$studyArea, "SpatVector")) {
    study_v <- sim$studyArea
  } else {
    study_v <- terra::vect(sim$studyArea)
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
  
  
  # ---------------------------------------------------------
  # 2) Align LandCover
  # ---------------------------------------------------------
  
  lc_src <- sim$LandCover  
  message("LandCover ncell BEFORE crop: ", terra::ncell(lc_src))
  
  # 1️⃣ crop (safe)
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
    stop("❌ Crop produced empty raster.")
  }
  
  # 2️⃣ project to analysis CRS
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
  #browser()
  
  # -----------------------------------------------------
  # ---------------------------------------------------------
  # 3) FINAL PlanningGrid
  # ---------------------------------------------------------
  
  message("Building PlanningGrid from studyArea")
  sim$PlanningGrid <- sim$rasterToMatch
  
  terra::values(sim$PlanningGrid) <- 1
  
  planning <- sim$PlanningGrid
  # ---------------------------------------------------------
  # 4) Align standAge
  # ---------------------------------------------------------
  
  if (!is.null(sim$standAge)) {
    
    sa_src <- sim$standAge    
    # ---------------------------------------------
    # crop FIRST in native CRS
    # ---------------------------------------------
    
    if (!terra::same.crs(study_v, sa_src)) {
      
      study_v_sa <- terra::project(
        study_v,
        terra::crs(sa_src)
      )
      
    } else {
      
      study_v_sa <- study_v
      
    }
    
    sa_src <- terra::crop(
      sa_src,
      terra::ext(study_v_sa),
      snap = "out"
    )
    
    # ---------------------------------------------
    # aggregate BEFORE project
    # ---------------------------------------------
    
    res_sa <- terra::res(sa_src)[1]
    fact_sa <- round(targetRes / res_sa)
    if (is.na(fact_sa) || fact_sa < 1) {
      fact_sa <- 1
    }
    
    if (fact_sa > 1) {
      
      sa_src <- terra::aggregate(
        sa_src,
        fact = fact_sa,
        fun = terra::modal,
        na.rm = TRUE
      )
      
    }
    
    # ---------------------------------------------
    # project AFTER crop + aggregate
    # ---------------------------------------------
    
    sa_src <- terra::project(
      sa_src,
      terra::crs(planning),
      method = "near"
    )
    sim$standAge <- terra::resample(
      sa_src,
      planning,
      method = "near"
    )
  }
  # ---------------------------------------------------------
  # 5) Rasterize FMU & CPCAD
  # ---------------------------------------------------------
  
  ## Rasterize FMU
  if (!"FMU_ID" %in% names(sim$FMU)) {
    sim$FMU$FMU_ID <- seq_len(nrow(sim$FMU))
  }
  #browser()
  fmu_r <- terra::rasterize(
    sim$FMU,
    planning,
    field = "FMU_ID",
    touches = FALSE
  )
  #browser()
  ## Rasterize CPCAD
  prot_r <- terra::rasterize(
    sim$CPCAD,
    planning,
    field = 1,
    background = 0
  )
  
  ## Legal mask
  LegalHarvestMask <- terra::ifel(
    !is.na(fmu_r) & prot_r == 0,
    1,
    0
  )
  
  sim$LegalConstraints <- list(
    FMU_Raster = fmu_r,
    CPCAD_Raster = prot_r,
    LegalHarvestMask = LegalHarvestMask
  )
  
  return(invisible(sim))
}