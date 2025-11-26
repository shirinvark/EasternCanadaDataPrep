# EasternCanadaDataPrep <img src="https://img.shields.io/badge/SpaDES-Module-blue?style=flat-square" align="right"/>

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)]()
[![R](https://img.shields.io/badge/R-%3E=4.2-blue.svg)]()
[![SpaDES.core](https://img.shields.io/badge/SpaDES.core-%3E=2.0-green.svg)]()
[![reproducible](https://img.shields.io/badge/reproducible-enabled-brightgreen.svg)]()

---

## 📌 Overview

**EasternCanadaDataPrep** is a fully automated, reproducible  
**SpaDES** module designed to prepare landbase layers for **Eastern Canada**:

- Ontario (ON)  
- Quebec (QC)  
- New Brunswick (NB)  
- Nova Scotia (NS)  
- Prince Edward Island (PEI)  
- Newfoundland and Labrador (NL)

It downloads, processes, and harmonizes:

- **LCC2020** — Land Cover of North America  
- **CPCAD 2024** — Canadian Protected & Conserved Areas  
- **FMU Canada** — Forest Management Units  

The module generates:

- `forestMask` — forest/non-forest  
- Protected-area removal mask  
- FMU boundary mask  
- Final **landbase raster**  
- `studyArea.gpkg`  
- Log file documenting outputs  

---

## 🚀 Features

- ✔ Automatic study-area generation (Eastern Canada)  
- ✔ Automated downloading with `reproducible::prepInputs()`  
- ✔ Forest mask using LCC2020  
- ✔ Filtering & exclusion of protected/private areas (CPCAD)  
- ✔ FMU masking  
- ✔ GeoTIFF & GPKG output support  
- ✔ Fully reproducible workflow  

---

## 📁 Project Structure

EasternCanadaProject/
├── modules/
│ └── EasternCanadaDataPrep/
│ ├── EasternCanadaDataPrep.R
│ ├── README.md
│ └── helpers.R (optional)
├── inputs/
├── output/
├── cache/
└── scratch/

yaml
Copy code

---

## 🧩 Installation

### Install required R packages:

```r
install.packages(c(
  "SpaDES.core", "SpaDES.tools", "reproducible",
  "terra", "sf", "rnaturalearth", "httr2", "dplyr", "fs"
))
▶ How to Run the Module
r
Copy code
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
📦 Outputs (saved to /output)
landbase_final.tif — final landbase raster

forestMask.tif — forest-only raster

studyArea.gpkg — Eastern Canada merged boundary

output_log.txt — run log

📚 Dependencies
Package	Purpose
SpaDES.core	module engine
reproducible	caching + downloading
terra	raster operations
sf	vector operations
rnaturalearth	boundary data
httr2	secure downloads
dplyr	filtering, processing

🔬 Citation
If you use this module in a publication:

rust
Copy code
Varkouhi, S. & Rudolph, T. (2025).
EasternCanadaDataPrep: A SpaDES module for automated landbase preparation
for Eastern Canada. https://github.com/<your_repo>
👩‍💻 Authors
Shirin Varkouhi — Lead Developer

Tyler Rudolph — Co-Developer

📜 License
This module is released under the MIT License.

yaml
Copy code
MIT License © 2025 Shirin Varkouhi & Tyler Rudolph
💛 Notes
This module is part of the larger
Harvest+ / Hanzlik AAC / LandR workflow for cross-border forest modeling.

Please report issues or improvement suggestions via GitHub Issues.

nginx
Copy code
Happy modeling! 🌲🔥