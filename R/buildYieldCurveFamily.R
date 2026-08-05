#------------------------------------------------------------------------------
# Build Yield Curve Family
#
# Downloads or loads jurisdiction-specific Yield Curve Family layers,
# standardizes their attributes, merges them, and creates a raster layer
# aligned to the PlanningGrid.
#------------------------------------------------------------------------------
buildYieldCurveFamily <- function(sim) {
  
  message("Building Yield Curve Family raster...")
  
  ycfList <- list(
    ON = buildYCF_ON(sim),
    NL = buildYCF_NL(sim)
  )
  
  stopifnot(length(ycfList) > 0)
  
  ycf <- do.call(
    terra::rbind,
    ycfList
  )
  
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