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
    inherits(sim$jurisdiction, "SpatRaster")
  )
  
  # ---------------------------------------------------------
  # Start unrestricted
  #
  # Jurisdictions without a DMFL layer are not excluded.
  # ---------------------------------------------------------
  
  sim$DMFL <- terra::rast(sim$PlanningGrid)
  sim$DMFL[] <- 1
  
  # ---------------------------------------------------------
  # Identify Ontario in jurisdiction raster
  # ---------------------------------------------------------
  
  jurLevels <- terra::levels(sim$jurisdiction)
  
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
  
  if (length(ontarioID) != 1) {
    stop("Could not uniquely identify Ontario in jurisdiction raster.")
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
  
  # Rasterize Ontario polygons
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
    sim$jurisdiction == ontarioID,
    dmflONRaster,
    1
  )
  
  names(sim$DMFL) <- "DMFL"
  
  message(
    "✔ DMFL raster created. ",
    "Ontario DMFL restriction applied; ",
    "other jurisdictions currently unrestricted."
  )
  
  sim
}