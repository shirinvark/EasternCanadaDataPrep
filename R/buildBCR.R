
#------------------------------------------------------------------------------
# Build Bird Conservation Regions (BCR)
#
# Rasterizes Bird Conservation Regions onto the PlanningGrid and creates
# a lookup table for downstream ecological analyses.
#------------------------------------------------------------------------------

buildBCR <- function(sim) {
  
  message("🔵 Building Bird Conservation Region map...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$BCR, "SpatVector")
  )
  
  planning <- sim$PlanningGrid
  bcr <- sim$BCR
  #------------------------------------------------------------
  # Match CRS to PlanningGrid
  #------------------------------------------------------------
  
  if (!terra::same.crs(bcr, planning)) {
    bcr <- terra::project(
      bcr,
      terra::crs(planning)
    )
  }
  #------------------------------------------------------------
  # Ensure each BCR has a unique numeric identifier
  #------------------------------------------------------------
  
  bcr$ID <- seq_len(nrow(bcr))
  
  #------------------------------------------------------------
  # Rasterize BCR polygons
  #------------------------------------------------------------
  
  sim$bcr <- terra::rasterize(
    bcr,
    planning,
    field = "ID"
  )
  
  #------------------------------------------------------------
  # Lookup table
  #------------------------------------------------------------
  
  sim$bcrLookup <- unique(
    terra::as.data.frame(
      bcr
    )[
      ,
      c(
        "ID",
        "bcr_label",
        "bcr_label_name"
      )
    ]
  )
  names(sim$bcr) <- "bcr"
  message(
    "✔ BCR raster created (",
    nrow(sim$bcrLookup),
    " BCRs)"
  )
  
  sim
  
}