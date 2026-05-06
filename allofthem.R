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
    #"shirinvark/RiparianBuffers",
   # "shirinvark/EasternCanadaLandbase"   # 👈 ماژول سوم اضافه شد
  ),
  modulePath = getPaths()$modulePath,
  overwrite  = FALSE
)

## =========================================================
## 6) INITIALIZE SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = c(
    "EasternCanadaDataPrep",
   # "RiparianBuffers",
    #"EasternCanadaLandbase"   # 👈 اضافه شد
  ),
  objects = list(
    studyArea = studyArea
  ),
 # params = list(
   # RiparianBuffers = list(
    #  hydroRaster_m = 250
    #)
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