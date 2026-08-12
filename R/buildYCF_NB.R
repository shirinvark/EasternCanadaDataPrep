#------------------------------------------------------------------------------
# Build New Brunswick Yield Curve Family
#
# Loads the New Brunswick Yield Curve Family polygons, standardizes the
# attribute names, projects them to the PlanningGrid CRS, and crops them to
# the PlanningGrid extent.
#------------------------------------------------------------------------------
buildYCF_NB <- function(sim) {
  
  message("Building New Brunswick Yield Curve Family...")
  
  ycf <- sim$YCF_NB
  
  if (!terra::same.crs(ycf, sim$PlanningGrid)) {
    
    ycf <- terra::project(
      ycf,
      terra::crs(sim$PlanningGrid)
    )
    
  }
  
  ycf <- terra::crop(
    ycf,
    terra::ext(sim$PlanningGrid)
  )
  
  if (nrow(ycf) == 0) {
    return(ycf)
  }
  
  ycf$Province <- "NB"
  
  ycf <- ycf[, c("Province", "YCF")]
  
  ycf
}