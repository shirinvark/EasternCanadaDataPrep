#------------------------------------------------------------------------------
# Build Jurisdiction
#
# Assigns each PlanningGrid cell to its provincial/territorial jurisdiction
# for downstream policy-specific processing.
# sim$Jurisdiction      # SpatVector (input)
# 
# ↓
# 
# buildJurisdiction()
# 
# ↓
# 
# sim$jurisdictionMap   # SpatRaster (output)
#------------------------------------------------------------------------------
buildJurisdiction <- function(sim) {
  
  message("Building jurisdiction raster...")
  
  stopifnot(inherits(sim$PlanningGrid, "SpatRaster"))
  stopifnot(inherits(sim$Jurisdiction, "SpatVector"))
  
  # Project jurisdiction polygons if needed
  if (!terra::same.crs(sim$Jurisdiction, sim$PlanningGrid)) {
    
    sim$Jurisdiction <- terra::project(
      sim$Jurisdiction,
      terra::crs(sim$PlanningGrid)
    )
    
  }
  
  # Add integer ID for rasterization
  sim$Jurisdiction$ID <- seq_len(nrow(sim$Jurisdiction))
  
  # Lookup table
  sim$jurisdictionLookup <- data.table::data.table(
    ID = sim$Jurisdiction$ID,
    jurisdiction = sim$Jurisdiction$NAME_En,
    nation = sim$Jurisdiction$COUNTRY
  )
  
  # Rasterize
  sim$jurisdictionMap <- terra::rasterize(
    sim$Jurisdiction,
    sim$PlanningGrid,
    field = "ID"
  )
  
  names(sim$jurisdictionMap) <- "jurisdiction"
  
  # Attach factor levels
  levels(sim$jurisdictionMap) <-
    sim$jurisdictionLookup[
      ,
      .(
        ID,
        jurisdiction
      )
    ]
  
  message("✔ Jurisdiction raster ready.")
  
  sim
}