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
library(reproducible)
library(terra)
library(sf)

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

print(getPaths())

## =========================================================
## 3) DOWNLOAD MODULE FROM GITHUB
## =========================================================
SpaDES.project::getModule(
  modules    = "shirinvark/EasternCanadaDataPrep",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 4) INITIALIZE SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaDataPrep",
  params  = list(
    EasternCanadaDataPrep = list(
      .useCache = TRUE
    )
  )
)

## =========================================================
## 5) RUN SIMULATION
## =========================================================
sim <- spades(sim)
ls(sim)

## =========================================================
## 6) QUICK CHECKS
## =========================================================
names(sim)

# Provinces
sim$Provinces
unique(sim$Provinces$province_code)

# Planning raster
sim$PlanningRaster
terra::res(sim$PlanningRaster)

# Hydrology sanity
names(sim$Hydrology)
nrow(sim$Hydrology$streams)
nrow(sim$Hydrology$lakes)
