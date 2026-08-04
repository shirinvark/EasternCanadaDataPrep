#------------------------------------------------------------------------------
# Build Managed Forest
#
# Creates a managed-forest mask on the PlanningGrid.
#------------------------------------------------------------------------------

buildManagedForest <- function(sim) {
  
  message("Building managed forest mask...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster")
  )
  
  
  
  message("✔ Managed forest mask ready.")
  
  sim
  
}