## =====================================================
## TEST FILE
## Purpose: Run EasternCanadaDataPrep module
## Output : sim with FMU, CPCAD, Hydrology
## Option : Build HLB (Temagami test)
## =====================================================

library(SpaDES.core)
library(reproducible)
library(terra)
library(sf)

## -------- SET PATHS --------
setPaths(
  modulePath  = "E:/EasternCanadaProject/modules",
  inputPath   = "E:/EasternCanadaProject/inputs",
  outputPath  = "E:/EasternCanadaProject/outputs",
  scratchPath = "E:/EasternCanadaProject/scratch",
  cachePath   = "E:/EasternCanadaProject/cache"
)

dir.create("E:/EasternCanadaProject/inputs",  recursive = TRUE, showWarnings = FALSE)
dir.create("E:/EasternCanadaProject/outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("E:/EasternCanadaProject/cache",   recursive = TRUE, showWarnings = FALSE)
dir.create("E:/EasternCanadaProject/scratch", recursive = TRUE, showWarnings = FALSE)

## -------- RUN MODULE --------
sim <- simInit(
  times = list(start = 0, end = 1),
  modules = "EasternCanadaDataPrep",
  params = list(
    EasternCanadaDataPrep = list(
      buildHLB  = TRUE,
      targetFMU = "Temagami Forest",  # 👈 
      hydroBuffer_m = 30
    )
  )
)


sim <- spades(sim)

## -------- QUICK CHECK --------
names(sim)
names(sim$Hydrology)
names(sim$Hydrology$buffered)
names(sim$EasternCanadaLandbase)

sim$EasternCanadaLandbase$HLB

message("✅ EasternCanadaDataPrep ran successfully (HLB enabled)")

list.files("E:/EasternCanadaProject/outputs/HLB")
hlb_disk <- terra::vect(
  "E:/EasternCanadaProject/outputs/HLB/HLB_Temagami_Forest.gpkg"
)
plot(hlb_disk)

