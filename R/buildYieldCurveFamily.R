#------------------------------------------------------------------------------
# Build Yield Curve Family
#
# Downloads or loads jurisdiction-specific Yield Curve Family layers,
# standardizes their attributes, merges them, and creates a raster layer
# aligned to the PlanningGrid.
#------------------------------------------------------------------------------
buildYieldCurveFamily <- function(sim) {
  
  message("Building Yield Curve Family raster...")
  
  on <- buildYCF_ON(sim)
  nl <- buildYCF_NL(sim)
  
  ycfList <- list()
  
  if (nrow(on) > 0) {
    ycfList$ON <- on
  }
  
  if (nrow(nl) > 0) {
    ycfList$NL <- nl
  }
  
  stopifnot(length(ycfList) > 0)
  length(ycfList)
  if (length(ycfList) == 1) {
    
    ycf <- ycfList[[1]]
    
  } else {
    
    ycf <- do.call(
      rbind,
      ycfList
    )
    
  }
  
  ycf$ID <- seq_len(nrow(ycf))
  
  sim$yieldCurveFamily <- terra::rasterize(
    ycf,
    sim$PlanningGrid,
    field = "ID"
  )
  
  names(sim$yieldCurveFamily) <- "yieldCurveFamily"
  
  levels(sim$yieldCurveFamily) <- data.frame(
    ID = ycf$ID,
    Province = ycf$Province,
    YCF = ycf$YCF
  )
  
  sim$yieldCurveLookup <- levels(
    sim$yieldCurveFamily
  )[[1]]
  
  sim
}