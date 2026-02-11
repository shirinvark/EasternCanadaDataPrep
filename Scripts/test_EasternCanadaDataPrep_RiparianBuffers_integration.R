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
## 3) LOAD STUDY AREA: SUDBURY FMU
## =========================================================
studyArea <- sf::st_read(
  "E:/EasternCanadaDataPrep/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

# اطمینان از valid geometry
studyArea <- sf::st_make_valid(studyArea)

# چک CRS (باید EPSG:5070 باشه)
sf::st_crs(studyArea)

## =========================================================
## 4) DOWNLOAD MODULES FROM GITHUB
## =========================================================
getModule(
  modules    = c(
    "shirinvark/EasternCanadaDataPrep",
    "shirinvark/RiparianBuffers"
  ),
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 5) INITIALIZE SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = c(
    "EasternCanadaDataPrep",
    "RiparianBuffers"
  ),
  objects = list(
    studyArea = studyArea
  ),
  params = list(
    RiparianBuffers = list(
      hydroRaster_m = 30
      # riparian policy از خود ماژول میاد
    )
  )
)

## ========================================================
## 6) RUN SIMULATION
## ========================================================
sim <- spades(sim)

## =========================================================
## 7) QUICK CHECKS
## =========================================================
ls(sim)

# Provinces (باید فقط ON باشه)
unique(sim$Provinces$jurisdiction)

# Planning raster (extent باید Sudbury FMU باشه)
sim$PlanningRaster

# Hydrology structure
names(sim$Hydrology)

# Riparian output
names(sim$Riparian)
plot(sim$Riparian$riparianFraction, main = "Riparian fraction – Sudbury FMU")

message("✅ DataPrep + RiparianBuffers pipeline ran successfully (Sudbury FMU)")

