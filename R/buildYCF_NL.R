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
  
  if (nrow(ycf) == 0) {
    return(ycf)
  }
  
  ycf$Province <- "NL"
  
  ycf <- ycf[, c("Province", "YCF")]
  
  ycf
  message("NL features before crop: ", nrow(sim$YCF_NL))
  
  message("NL features after crop: ", nrow(ycf))
  ycf$Province <- "NL"
  
  ycf <- ycf[, c("Province", "YCF")]
  
  ycf
}