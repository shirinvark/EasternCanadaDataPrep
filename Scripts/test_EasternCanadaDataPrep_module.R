## =========================================================
## 0) CLEAN SESSION
## =========================================================
rm(list = ls())
gc()

## =========================================================
## 1) LOAD PACKAGES
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

## =========================================================
## 3) LOAD SMALL STUDY AREA
## =========================================================

studyArea <- sf::st_read(
  "D:/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

studyArea <- sf::st_make_valid(studyArea)

# dissolve polygons (faster masking/rasterization)
studyArea <- sf::st_union(studyArea)

# convert back to sf
studyArea <- sf::st_sf(
  id = 1,
  geometry = studyArea
)

# project to module CRS
studyArea <- sf::st_transform(
  studyArea,
  "ESRI:102001"
)

## =========================================================
## 4) OPTIONAL: REMOVE OLD CACHE
## =========================================================

unlink(
  file.path(getPaths()$cachePath, "*"),
  recursive = TRUE,
  force = TRUE
)

## =========================================================
## 5) GET MODULE
## =========================================================
# #getModule(
#   modules    = "shirinvark/EasternCanadaDataPrep",
#   modulePath = getPaths()$modulePath,
#   overwrite  = TRUE
# )

## =========================================================
## 6) INIT SIM
## =========================================================
sim <- simInit(
  times = list(start = 1, end = 1),
  
  modules = "EasternCanadaDataPrep",
  
  objects = list(
    studyArea = studyArea
  ),
  
  paths = getPaths()
)

## =========================================================
## 7) RUN MODULE
## =========================================================
system.time({
  
  sim <- spades(sim)
  
})

## =========================================================
## 8) CHECK OUTPUTS
## =========================================================

cat("\n============================\n")
cat("OUTPUT OBJECTS\n")
cat("============================\n")

print(names(sim))

cat("\n============================\n")
cat("PlanningGrid extent\n")
cat("============================\n")

print(terra::ext(sim$PlanningGrid_250m))

cat("\n============================\n")
cat("PlanningGrid ncell\n")
cat("============================\n")

print(terra::ncell(sim$PlanningGrid_250m))

cat("\n============================\n")
cat("LandCover unique values\n")
cat("============================\n")

print(
  unique(
    terra::values(sim$LandCover_250m)
  )
)

cat("\n============================\n")
cat("PlanningGrid unique values\n")
cat("============================\n")

print(
  unique(
    terra::values(sim$PlanningGrid_250m)
  )
)

## =========================================================
## 9) QUICK PLOTS
## =========================================================

plot(
  sim$PlanningGrid_250m,
  main = "PlanningGrid"
)

plot(
  sim$LandCover_250m,
  main = "LandCover 250m"
)

plot(
  sim$LegalConstraints$LegalHarvestMask_250m,
  main = "Legal Harvest Mask"
)