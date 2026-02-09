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
## 3) INIT + RUN SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaDataPrep"
)

sim <- spades(sim)

## =========================================================
## 4) CHECK OUTPUTS
## =========================================================
names(sim)

## اگر این وجود داره:
names(sim$EasternCanadaLandbase)

## Provinces رو امن چک کن
if ("Provinces" %in% names(sim)) {
  names(sim$Provinces)
  unique(sf::st_drop_geometry(sim$Provinces)[, 1])
} else {
  stop("❌ Provinces object was not created by EasternCanadaDataPrep")
}
library(sf)

prov <- rnaturalearth::ne_states(
  country = "Canada",
  returnclass = "sf"
)

east <- prov[prov$name_en %in% c(
  "Ontario",
  "Quebec",
  "New Brunswick",
  "Nova Scotia",
  "Prince Edward Island",
  "Newfoundland and Labrador"
), ]

unique(east$name_en)

