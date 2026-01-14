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

## =========================================================
## 2) SET PATHS (AUTO-CREATED IF NOT EXIST)
## =========================================================
setPaths(
  cachePath   = "E:/EasternCanadaDataPrep/cache",
  inputPath   = "E:/EasternCanadaDataPrep/inputs",
  outputPath  = "E:/EasternCanadaDataPrep/outputs",
  modulePath  = "E:/EasternCanadaDataPrep/modules",
  scratchPath = "E:/EasternCanadaDataPrep/scratch"
)

## sanity check
print(getPaths())

## =========================================================
## 3) DOWNLOAD MODULE FROM GITHUB
## =========================================================
SpaDES.project::getModule(
  modules = "shirinvark/EasternCanadaDataPrep",
  modulePath = getPaths()$modulePath,
  overwrite = TRUE
)


## =========================================================
## 4) CHECK MODULE VISIBILITY
## =========================================================
stopifnot("EasternCanadaDataPrep" %in% modules())

## =========================================================
## 5) INITIALIZE SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaDataPrep"
)

## =========================================================
## 6) RUN MODULE
## =========================================================
sim <- spades(sim)

## =========================================================
## 7) POST-RUN CHECKS
## =========================================================
# objects created
ls(sim)

# planning raster
sim$PlanningRaster
terra::ncell(sim$PlanningRaster)
terra::res(sim$PlanningRaster)

# landbase table summary
summary(sim$LandbaseTable$effective_area / 10000)

# harvestable mask diagnostics
table(
  terra::values(
    sim$EasternCanadaLandbase$HarvestableMask
  )
)
