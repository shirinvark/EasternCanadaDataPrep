## ======== SET SpaDES PATHS (REQUIRED) ========
library(SpaDES.core)
library(reproducible)
library(terra)
library(sf)

options(
  spades.modulePath = "E:/EasternCanadaProject/modules",
  spades.inputPath  = "E:/EasternCanadaProject/inputs",
  spades.outputPath = "E:/EasternCanadaProject/outputs",
  spades.scratchPath = "E:/EasternCanadaProject/scratch",
  reproducible.cachePath = "E:/EasternCanadaProject/cache"
)

dir.create("E:/EasternCanadaProject/inputs", recursive = TRUE, showWarnings = FALSE)
dir.create("E:/EasternCanadaProject/outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("E:/EasternCanadaProject/cache",   recursive = TRUE, showWarnings = FALSE)
dir.create("E:/EasternCanadaProject/scratch", recursive = TRUE, showWarnings = FALSE)

sim <- simInit(
  times = list(start = 0, end = 1),
  modules = list("EasternCanadaDataPrep")
)
sim$paths
