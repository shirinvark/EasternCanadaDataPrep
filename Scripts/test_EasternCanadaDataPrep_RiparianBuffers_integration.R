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
studyArea <- sf::st_transform(studyArea, terra::crs(lc))
## =========================================================
## 3) CREATE SMALL TEST PATCH INSIDE SUDBURY (10km x 10km)
## =========================================================

# گرفتن centroid
cent <- sf::st_centroid(sf::st_union(sudbury))
xy <- sf::st_coordinates(cent)

xmin <- xy[1] - 2500
xmax <- xy[1] + 2500
ymin <- xy[2] - 2500
ymax <- xy[2] + 2500

small_ext <- terra::ext(xmin, xmax, ymin, ymax)
small_poly <- terra::as.polygons(small_ext, crs = terra::crs(terra::vect(sudbury)))

# محدود کردن به داخل Sudbury
small_poly <- terra::intersect(
  terra::vect(sudbury),
  small_poly
)

## =========================================================
## 4) LOAD LandCover
## =========================================================
lc <- terra::rast(
  "E:/MODULES_TESTS/SCANFI_att_nfiLandCover_CanadaLCCclassCodes_S_2010_v1_1.tif"
)

## =========================================================
## 5) DOWNLOAD MODULES
## =========================================================
getModule(
  modules    = c(
    "shirinvark/EasternCanadaDataPrep",
    "shirinvark/RiparianBuffers"
  ),
  modulePath = getPaths()$modulePath,
  overwrite  = FALSE
)

## =========================================================
## 6) INIT SIM (SMALL PATCH)
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = c(
    "EasternCanadaDataPrep",
    "RiparianBuffers"
  ),
  objects = list(
    LandCover = lc,
    studyArea = small_poly
  ),
  params = list(
    EasternCanadaDataPrep = list(
      devMode = FALSE
    ),
    RiparianBuffers = list(
      hydroRaster_m = 25
    )
  ),
  paths = getPaths()
)

## =========================================================
## 7) RUN
## =========================================================
system.time({
  sim <- spades(sim)
})

## =========================================================
## 8) CHECK
## =========================================================
cat("\nPlanningGrid values:\n")
print(unique(values(sim$PlanningGrid_250m)))

plot(sim$PlanningGrid_250m,
     main = "Planning Grid – Sudbury small patch")

plot(sim$Riparian$riparianFraction,
     main = "Riparian fraction – Sudbury small patch")

message("✅ Small Sudbury patch: DataPrep + Riparian OK")