## =========================================================
## 0) CLEAN SESSION
## =========================================================
rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## =========================================================
## 1) SET PATHS
## =========================================================
setPaths(
  cachePath   = "E:/EasternCanadaDataPrep/cache",
  inputPath   = "E:/EasternCanadaDataPrep/inputs",
  outputPath  = "E:/EasternCanadaDataPrep/outputs",
  modulePath  = "E:/EasternCanadaDataPrep/modules",
  scratchPath = "E:/EasternCanadaDataPrep/scratch"
)

clearCache(getPaths()$cachePath)

## =========================================================
## 2) LOAD SUDBURY FMU
## =========================================================
sudbury <- sf::st_read(
  "E:/EasternCanadaDataPrep/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

sudbury <- sf::st_make_valid(sudbury)

## =========================================================
## 3) CREATE SMALL TEST PATCH (5 km x 5 km)
## =========================================================

cent <- sf::st_centroid(
  sf::st_union(sudbury)
)

xy <- sf::st_coordinates(cent)

xmin <- xy[1] - 2500
xmax <- xy[1] + 2500
ymin <- xy[2] - 2500
ymax <- xy[2] + 2500

small_ext <- terra::ext(
  xmin, xmax,
  ymin, ymax
)

small_poly <- terra::as.polygons(
  small_ext,
  crs = terra::crs(
    terra::vect(sudbury)
  )
)

small_poly <- terra::intersect(
  terra::vect(sudbury),
  small_poly
)

## =========================================================
## 4) DOWNLOAD MODULES
## =========================================================
getModule(
  modules = c(
    "shirinvark/EasternCanadaDataPrep",
    "shirinvark/RiparianBuffers"
  ),
  modulePath = getPaths()$modulePath,
  overwrite = FALSE
)

## =========================================================
## 5) INIT SIM
## =========================================================
sim <- simInit(
  
  times = list(
    start = 1,
    end   = 1
  ),
  
  modules = c(
    "EasternCanadaDataPrep",
    "RiparianBuffers"
  ),
  
  objects = list(
    studyArea = small_poly
  ),
  
  params = list(
    
    EasternCanadaDataPrep = list(
      .useCache = FALSE
    ),
    
    RiparianBuffers = list(
      hydroRaster_m = 25
    )
  ),
  
  paths = getPaths()
)

## =========================================================
## 6) RUN
## =========================================================
system.time({
  sim <- spades(sim)
})

## =========================================================
## 7) CHECKS
## =========================================================

cat("\n====================\n")
cat("PlanningGrid check\n")
cat("====================\n")

print(sim$PlanningGrid_250m)

print(
  unique(
    values(sim$PlanningGrid_250m)
  )
)

print(
  freq(sim$PlanningGrid_250m)
)

cat("\n====================\n")
cat("LandCover check\n")
cat("====================\n")

print(sim$LandCover_250m)

print(
  freq(sim$LandCover_250m)
)

cat("\n====================\n")
cat("Riparian check\n")
cat("====================\n")

print(
  summary(
    values(
      sim$Riparian$riparianFraction
    )
  )
)

plot(
  sim$PlanningGrid_250m,
  main = "PlanningGrid"
)

plot(
  sim$Riparian$riparianFraction,
  main = "Riparian Fraction"
)

message("✅ TEST FINISHED")