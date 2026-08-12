#------------------------------------------------------------------------------
# Build Protected Areas
#
# Rasterizes CPCAD protected areas onto the PlanningGrid using IUCN category.
#------------------------------------------------------------------------------

buildProtectedAreas <- function(sim) {
  
  message("Building Protected Areas raster...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$CPCAD, "SpatVector")
  )
  
  cpcad <- sim$CPCAD
  
  #------------------------------------------------------------
  # Match CRS to PlanningGrid
  #------------------------------------------------------------
  
  if (!terra::same.crs(cpcad, sim$PlanningGrid)) {
    cpcad <- terra::project(
      cpcad,
      terra::crs(sim$PlanningGrid)
    )
  }
  
  #------------------------------------------------------------
  # Rasterize IUCN protected-area category
  #------------------------------------------------------------
  
  sim$protectedArea <- terra::rasterize(
    cpcad,
    sim$PlanningGrid,
    field = "IUCN_CAT",
    background = 0
  )
  
  names(sim$protectedArea) <- "protectedArea"
  
  message("✔ Protected Areas raster created.")
  
  sim
}