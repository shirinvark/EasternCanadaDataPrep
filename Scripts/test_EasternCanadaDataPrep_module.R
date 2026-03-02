## =========================================================
## 0) CLEAN SESSION
## =========================================================
rm(list = ls())
gc()

## =========================================================
## 1) LOAD PACKAGES
## =========================================================
library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## =========================================================
## 2) SET PATHS
## =========================================================
setPaths(
  cachePath   = "E:/EasternCanadaDataPrep/cache",
  inputPath   = "E:/EasternCanadaDataPrep/inputs",
  outputPath  = "E:/EasternCanadaDataPrep/outputs",
  modulePath  = "E:/EasternCanadaDataPrep/modules",
  scratchPath = "E:/EasternCanadaDataPrep/scratch"
)

print(getPaths())

## =========================================================
## 3) CLEAR CACHE
## =========================================================
clearCache(getPaths()$cachePath)

## =========================================================
## 4) LOAD STUDY AREA
## =========================================================
studyArea <- sf::st_read(
  "E:/EasternCanadaDataPrep/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

studyArea <- sf::st_make_valid(studyArea)

## =========================================================
## 5) GET DataPrep MODULE
## =========================================================
getModule(
  modules    = "shirinvark/EasternCanadaDataPrep",
  modulePath = getPaths()$modulePath,
  overwrite  = FALSE
)

## =========================================================
## 6) LOAD LandCover
## =========================================================
lc <- terra::rast(
  "E:/MODULES_TESTS/SCANFI_att_nfiLandCover_CanadaLCCclassCodes_S_2010_v1_1.tif"
)

## =========================================================
## ⚡ IMPORTANT: Crop LandCover to studyArea (correct CRS)
## =========================================================

# تبدیل studyArea به CRS لندکاور
studyArea_lcCRS <- sf::st_transform(
  studyArea,
  crs = terra::crs(lc)
)

# تبدیل به SpatVector
study_v <- terra::vect(studyArea_lcCRS)

# crop
lc <- terra::crop(lc, study_v)

gc()

# چک کن که کوچک شده
terra::ncell(lc)

## =========================================================
## 7) INIT SIM (ONLY DataPrep)
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaDataPrep",
  objects = list(
    LandCover = lc,
    studyArea = studyArea
  ),
  params = list(
    EasternCanadaDataPrep = list(
      devMode = TRUE
    )
  )
)

## =========================================================
## 8) RUN
## =========================================================
sim <- spades(sim)

## =========================================================
## 9) CHECK OUTPUTS
## =========================================================
ls(sim)

terra::ext(sim$PlanningGrid_250m)

plot(sim$PlanningGrid_250m, main = "Planning Grid (DEV MODE)")

sim$LegalConstraints