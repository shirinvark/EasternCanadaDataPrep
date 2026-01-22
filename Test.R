## =========================================================
## 0) CLEAN SESSION
## =========================================================
rm(list = ls())
gc()
.rs.restartR()

## =========================================================
## 1) LOAD REQUIRED PACKAGES
## =========================================================
library(SpaDES.core)
library(SpaDES.project)
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
## 4) INITIALIZE & RUN SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 0, end = 1),
  modules = "EasternCanadaDataPrep"
)

sim <- spades(sim)
names(sim)
sim$Provinces
unique(sim$Provinces$province_code)

