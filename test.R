## ======== SET SpaDES PATHS (REQUIRED) ========
library(SpaDES.core)
library(reproducible)
library(terra)
library(sf)

setPaths(
  modulePath = "E:/EasternCanadaProject/modules",
  inputPath  = "E:/EasternCanadaProject/inputs",
  outputPath = "E:/EasternCanadaProject/outputs",
  scratchPath = "E:/EasternCanadaProject/scratch",
  cachePath = "E:/EasternCanadaProject/cache"
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
sim <- spades(sim)
hlb <- sim$EasternCanadaLandbase$Harvestable_LB

cols <- c("grey80", "darkgreen")   # 0 = grey, 1 = green

terra::plot(
  hlb,
  col = cols,
  main = "HLB — Harvestable Land Base"
)
