## =========================================================
## 0) CLEAN SESSION
## =========================================================
rm(list = ls())
gc()

## =========================================================
## 1) LOAD REQUIRED PACKAGES
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
## 3) LOAD STUDY AREA
## =========================================================
studyArea <- sf::st_read(
  "E:/EasternCanadaDataPrep/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

studyArea <- sf::st_make_valid(studyArea)

## =========================================================
## 4) DOWNLOAD MODULES
## =========================================================
getModule(
  modules    = c(
    "shirinvark/EasternCanadaDataPrep",
    "shirinvark/RiparianBuffers",
    "shirinvark/EasternCanadaLandbase"   # 👈 ماژول سوم اضافه شد
  ),
  modulePath = getPaths()$modulePath,
  overwrite  = FALSE
)

## =========================================================
## 5) LOAD LANDCOVER
## =========================================================
## =========================================================
## 5) LOAD & CROP LANDCOVER
## =========================================================

# مسیر فایل اصلی
landcoverPath <- "E:/MODULES_TESTS/SCANFI_att_nfiLandCover_CanadaLCCclassCodes_S_2010_v1_1.tif"

# تابع crop
cropLandCoverToStudyArea <- function(landcoverPath, studyArea) {
  
  message("Loading full LandCover raster...")
  lc_full <- terra::rast(landcoverPath)
  
  # هماهنگ کردن CRS
  if (!terra::same.crs(lc_full, terra::vect(studyArea))) {
    studyArea <- sf::st_transform(studyArea, terra::crs(lc_full))
  }
  
  message("Cropping to studyArea...")
  lc_crop <- terra::crop(lc_full, terra::vect(studyArea))
  
  message("Masking outside studyArea...")
  lc_mask <- terra::mask(lc_crop, terra::vect(studyArea))
  
  return(lc_mask)
}

# این خط مهمه 👇
lc <- cropLandCoverToStudyArea(landcoverPath, studyArea)

## CREATE TEMP STAND AGE MAP (DEV MODE)
standAgeMap <- terra::rast(lc)
standAgeMap[] <- sample(1:120, terra::ncell(standAgeMap), replace = TRUE)
## =========================================================
## 6) INITIALIZE SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = c(
    "EasternCanadaDataPrep",
    "RiparianBuffers",
    "EasternCanadaLandbase"   # 👈 اضافه شد
  ),
  objects = list(
    LandCover = lc,
    studyArea = studyArea
  ),
  params = list(
    RiparianBuffers = list(
      hydroRaster_m = 250
    )
  )
)

## ========================================================
## 7) RUN SIMULATION
## ========================================================
sim <- spades(sim)

## =========================================================
## 8) CHECK OUTPUTS
## =========================================================

ls(sim)

# forest base
plot(sim$forestBase, main = "Forest Base")

# protected mask
plot(sim$protectedMask, main = "Protected Mask")

# merchantable forest
plot(sim$merchantableForest, main = "Merchantable Forest")

# analysis units
plot(sim$analysisUnitMap, main = "Analysis Unit Map")

message("✅ All three modules ran successfully")