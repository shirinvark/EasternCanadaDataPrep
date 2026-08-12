buildYCF_ON <- function(sim) {
  
  message("Building Ontario Yield Curve Family...")
  
  ycf <- sim$YCF_ON
  
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
  
  # No Ontario YCF polygons in the current study area
  if (nrow(ycf) == 0) {
    message("No Ontario Yield Curve Family polygons in study area.")
    return(ycf)
  }
  
  ycf$Province <- "ON"
  ycf$YCF <- toupper(ycf$SITEREGION)
  
  ycf <- ycf[, c("Province", "YCF")]
  
  ycf
  
}