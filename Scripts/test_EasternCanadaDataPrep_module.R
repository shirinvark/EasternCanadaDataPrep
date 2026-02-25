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
## 3) LOAD SMALL STUDY AREA (SUDBURY FMU TEST)
## =========================================================
studyArea <- sf::st_read(
  "E:/EasternCanadaDataPrep/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

# اطمینان از valid geometry
studyArea <- sf::st_make_valid(studyArea)

# بسیار مهم: تبدیل به Canada Albers (ESRI:102001)
studyArea <- sf::st_transform(studyArea, 3978)
cat("Study Area CRS:\n")
print(sf::st_crs(studyArea))
print(studyArea)
print(st_bbox(studyArea))
print(st_crs(studyArea))
terraOptions(threads = 8)
## =========================================================
## 4) INIT SIMULATION (WITH SMALL STUDY AREA)
## =========================================================
sim <- simInit(
  times = list(start = 0, end = 1),
  modules = "EasternCanadaDataPrep",
  objects = list(
    studyArea = studyArea
  ),
  options = list(
    spades.checkpoint = FALSE,
    spades.save       = FALSE,
    spades.progress   = FALSE
  )
)
print(sim$LandCover)
print(sim$standAgeMap)
## =========================================================
## 5) RUN SIMULATION
## =========================================================
system.time(
  sim <- spades(sim)
)

## =========================================================
## 6) CHECK OUTPUTS
## =========================================================

cat("\nObjects in sim:\n")
print(names(sim))

## Planning Grid
if ("PlanningGrid_250m" %in% names(sim)) {
  plot(sim$PlanningGrid_250m, main = "Planning Grid 250m")
} else {
  stop("❌ PlanningGrid_250m was not created")
}

## Legal Mask
if ("LegalConstraints" %in% names(sim)) {
  plot(sim$LegalConstraints$LegalHarvestMask_250m,
       main = "Legal Harvest Mask 250m")
} else {
  stop("❌ LegalConstraints not created")
}

cat("\n✅ Small test run completed successfully.\n")