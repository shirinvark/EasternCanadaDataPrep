buildYCF_NL <- function(sim) {
  
  message("Building Newfoundland Yield Curve Family...")
  
  ycf <- sim$YCF_NL
  
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
  
  ycf$Province <- "NL"
  
  ycf <- ycf[, c("Province", "YCF")]
  
  ycf
}