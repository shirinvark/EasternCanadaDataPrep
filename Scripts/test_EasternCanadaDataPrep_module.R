rm(list = ls())
gc()
library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)
setPaths(
  cachePath   = "E:/EasternCanadaDataPrep/cache",
  inputPath   = "E:/EasternCanadaDataPrep/inputs",
  outputPath  = "E:/EasternCanadaDataPrep/outputs",
  modulePath  = "E:/EasternCanadaDataPrep/modules",
  scratchPath = "E:/EasternCanadaDataPrep/scratch"
)
unlink("E:/EasternCanadaDataPrep/cache/CPCAD", recursive = TRUE, force = TRUE)
unlink("E:/EasternCanadaDataPrep/cache/FMU", recursive = TRUE, force = TRUE)
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaDataPrep"
)

sim <- spades(sim)
names(sim)
names(sim$EasternCanadaLandbase)
unique(sim$Provinces$province_code)
