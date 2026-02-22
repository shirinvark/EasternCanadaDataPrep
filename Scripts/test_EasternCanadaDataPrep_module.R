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

print(getPaths())

## =========================================================
## 2) FORCE DOWNLOAD MODULE FROM GITHUB
## =========================================================
SpaDES.project::getModule(
  modules    = "shirinvark/EasternCanadaDataPrep",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 3) LOAD LandCover (MANDATORY in method 2)
## =========================================================
lc <- terra::rast(
  "E:/MODULES_TESTS/SCANFI_att_nfiLandCover_CanadaLCCclassCodes_S_2010_v1_1.tif"
)

## =========================================================
## 4) INIT + RUN SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaDataPrep",
  objects = list(
    LandCover = lc
  )
)

sim <- spades(sim)

## =========================================================
## 5) CHECK OUTPUTS
## =========================================================
names(sim)

## Provinces check (safe)
if ("Provinces" %in% names(sim)) {
  print(names(sim$Provinces))
  print(unique(sim$Provinces$jurisdiction))
} else {
  stop("❌ Provinces object was not created by EasternCanadaDataPrep")
}

## Planning grid check
if ("PlanningGrid_250m" %in% names(sim)) {
  plot(sim$PlanningGrid_250m)
}

## Legal mask check
if ("LegalConstraints" %in% names(sim)) {
  plot(sim$LegalConstraints$LegalHarvestMask_250m)
}

