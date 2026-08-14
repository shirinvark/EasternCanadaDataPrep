#------------------------------------------------------------------------------
# Build Sustained Yield Units
#
# Converts SYU polygons to a categorical raster aligned with PlanningGrid.
# If no user-defined SYU is supplied, FMUs are used as the default SYUs.
#------------------------------------------------------------------------------

buildSYU <- function(sim) {
  
  message("Building Sustained Yield Unit raster...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$SYU, "SpatVector")
  )
  
  if (!terra::same.crs(sim$SYU, sim$PlanningGrid)) {
    sim$SYU <- terra::project(
      sim$SYU,
      terra::crs(sim$PlanningGrid)
    )
  }
  
  # Unique internal ID for rasterization
  sim$SYU$SYU_ID <- seq_len(nrow(sim$SYU))
  
  # Create lookup table
  if (all(c(
    "PT",
    "FM_UNIT",
    "FM_UNIT_ID",
    "FM_UNIT_NA"
  ) %in% names(sim$SYU))) {
    
    # Default FMU-based SYUs
    sim$SYULookup <- data.table::data.table(
      SYU_ID = sim$SYU$SYU_ID,
      jurisdiction = sim$SYU$PT,
      FMU_ID = sim$SYU$FM_UNIT_ID,
      SYU_NAME = sim$SYU$FM_UNIT_NA,
      FMU_TYPE = sim$SYU$FM_UNIT
    )
    
  } else {
    
    # User-supplied SYUs
    sim$SYULookup <- data.table::data.table(
      SYU_ID = sim$SYU$SYU_ID
    )
  }
  
  syuVector <- sim$SYU
  
  sim$SYU <- terra::rasterize(
    syuVector,
    sim$PlanningGrid,
    field = "SYU_ID"
  )
  
  names(sim$SYU) <- "SYU"
  
  message(
    "✔ SYU ready. Units: ",
    nrow(sim$SYULookup)
  )
  
  invisible(sim)
}