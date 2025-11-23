# EasternCanadaDataPrep

A reproducible SpaDES module for preparing landbase layers for Eastern Canada  
(ON, QC, NB, NS, PEI, NL).  
Authors: **Shirin Varkuhi** & **Tyler Rudolph**

---

## Overview

`EasternCanadaDataPrep` is an automated data-preparation module designed for Eastern Canada.  
It downloads, processes, and integrates:

- **LCC2020** — Land Cover of North America  
- **CPCAD 2024** — Protected and Conserved Areas  
- **FMU Canada** — Forest Management Units  

The module then builds:

- `forestMask` (forest / non-forest)  
- Removes protected areas  
- Applies FMU boundaries  
- Produces a final **landbase raster**  
- Saves all outputs to disk  

---

## Features

- Automated dataset downloading  
- Automatic study-area generation (Eastern provinces)  
- Forest mask creation  
- Protected-area removal (CPCAD)  
- FMU boundary masking  
- Saves outputs (GeoTIFF + GPKG)  
- Fully reproducible SpaDES workflow  

---

## Directory Structure

EasternCanadaProject/
├── modules/
│   └── EasternCanadaDataPrep/
│       └── EasternCanadaDataPrep.R
├── inputs/
├── output/
├── cache/
└── README.md

---

## How to Run

library(SpaDES.core)

pathsList <- list(
  modulePath = "E:/EasternCanadaProject/modules",
  inputPath  = "E:/EasternCanadaProject/inputs",
  outputPath = "E:/EasternCanadaProject/output",
  cachePath  = "E:/EasternCanadaProject/cache"
)

sim <- simInit(
  times = list(start = 0, end = 1),
  params = list(
    EasternCanadaDataPrep = list(useUserStudyArea = FALSE)
  ),
  modules = list("EasternCanadaDataPrep"),
  paths = pathsList
)

simOut <- spades(sim)

---

## Outputs Saved in `output/`

- landbase_final.tif  
- forestMask.tif  
- studyArea.gpkg  
- output_log.txt  

---

## Requirements

- SpaDES.core  
- SpaDES.tools  
- reproducible  
- terra  
- sf  
- rnaturalearth  
- httr2  

---

## Authors

- **Shirin Varkuhi** — Lead developer  
- **Tyler Rudolph** — Data workflow advisor  

---

## License
MIT License (or project-preferred)
