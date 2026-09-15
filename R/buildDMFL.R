#------------------------------------------------------------------------------
# Build DMFL
#
# Creates a binary Designated Managed Forest Land raster aligned to
# PlanningGrid.
#
# Current jurisdiction-specific rules:
#
# Ontario:
#   1 = inside Ontario DMFL polygons
#   0 = outside Ontario DMFL polygons
#
# Other jurisdictions:
#   1 = no DMFL restriction currently applied
#
# Additional jurisdiction-specific DMFL layers can be added later.
#------------------------------------------------------------------------------

buildDMFL <- function(sim) {
  
  message("Building DMFL raster...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$DMFL_ON, "SpatVector"),
    inherits(sim$jurisdictionMap, "SpatRaster")
  )
  
  # ---------------------------------------------------------
  # Start unrestricted
  #
  # Jurisdictions without a DMFL layer are not excluded.
  # ---------------------------------------------------------
  
  sim$DMFL <- terra::rast(sim$PlanningGrid)
  sim$DMFL[] <- 1
  names(sim$DMFL) <- "DMFL"
  
  # ---------------------------------------------------------
  # Read jurisdiction lookup
  # ---------------------------------------------------------
  
  jurLevels <- terra::levels(sim$jurisdictionMap)
  
  if (
    length(jurLevels) == 0 ||
    is.null(jurLevels[[1]]) ||
    nrow(jurLevels[[1]]) == 0
  ) {
    stop("Jurisdiction raster has no category lookup.")
  }
  
  jurLookup <- jurLevels[[1]]
  
  idField <- names(jurLookup)[1]
  nameField <- names(jurLookup)[2]
  
  ontarioID <- jurLookup[
    jurLookup[[nameField]] == "Ontario",
    idField
  ]
  
  # ---------------------------------------------------------
  # No Ontario in study area
  #
  # No DMFL restriction is currently applied to other
  # jurisdictions.
  # ---------------------------------------------------------
  
  if (length(ontarioID) == 0) {
    
    message(
      "Ontario not present in study area. ",
      "No DMFL restriction applied."
    )
    
    return(sim)
  }
  
  if (length(ontarioID) > 1) {
    stop("Ontario appears more than once in jurisdiction lookup.")
  }
  
  # ---------------------------------------------------------
  # Prepare Ontario DMFL polygons
  # ---------------------------------------------------------
  
  dmflON <- sim$DMFL_ON
  
  if (!terra::same.crs(dmflON, sim$PlanningGrid)) {
    
    dmflON <- terra::project(
      dmflON,
      terra::crs(sim$PlanningGrid)
    )
  }
  
  dmflON$DMFL <- 1
  
  dmflONRaster <- terra::rasterize(
    dmflON,
    sim$PlanningGrid,
    field = "DMFL",
    background = 0,
    touches = TRUE
  )
  
  # ---------------------------------------------------------
  # Apply DMFL restriction ONLY within Ontario
  # ---------------------------------------------------------
  
  sim$DMFL <- terra::ifel(
    sim$jurisdictionMap == ontarioID,
    dmflONRaster,
    1
  )
  
  names(sim$DMFL) <- "DMFL"
  
  message(
    "DMFL raster created. ",
    "Ontario DMFL restriction applied; ",
    "other jurisdictions currently unrestricted."
  )
  
  sim
}