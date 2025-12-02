![made-with-Markdown](https://img.shields.io/badge/Made%20with-Markdown-1f425f.svg)

# EasternCanadaDataPrep

Prepare and standardize all spatial layers required for the Eastern Canada landbase.  
This SpaDES module downloads, processes, crops, masks, filters, and reprojects datasets such as protected areas (CPCAD), land cover (LCC2020), forest management units (FMUs), and study area boundaries.

This module is part of the **Eastern Canada Landbase Preparation Pipeline**, supporting downstream models including **LandR**, **AAC calculation**, **SimpleHarvest**, and **classification modules**.

---

## 🌎 Purpose

The goal of this module is to create a **clean, harmonized, and analysis-ready landbase** for Eastern Canada.  
It ensures all spatial datasets:

- share the same CRS (EPSG:5070 — NAD83 / Conus Albers)
- are correctly clipped to the study area  
- are masked, filtered, and cleaned  
- follow consistent naming and file formats  
- are ready for use in other SpaDES modules  

---

## 📦 Input Objects

The module accepts the following inputs (may be provided externally or created inside the module):

| Object      | Class            | Description                                   |
|-------------|------------------|-----------------------------------------------|
| `studyArea` | sf / SpatVector  | Polygon defining the modeling area           |
| `LCC2020`   | SpatRaster       | Land cover (30m)                              |
| `CPCAD`     | sf / SpatVector  | Protected areas                               |
| `FMU`       | sf / SpatVector  | Forest Management Units                       |

If not provided, `studyArea` can be auto-generated (default: ON, QC, NB, NS, PEI, NL).

---

## 🛠 What the module does

### ✔ Downloads (optional via `prepInputs()`)
- CPCAD 2024 (via Google Drive link)
- FMU boundaries (Canada-wide)

### ✔ Filters CPCAD  
Removes:
- STATUS: Proposed (3), Delisted (5)  
- PA_OECM_DF: Proposed (4), Delisted (5)  
- IUCN categories outside 1–7  

### ✔ Preprocesses FMUs  
- Clips to studyArea  
- Reprojects to EPSG:5070  

### ✔ Prepares land cover (if provided)  
- Crops, masks, reprojects  

### ✔ Builds unified landbase object  
Creates:

```r
sim$EasternCanadaLandbase <- list(
  LCC2020 = <raster>,
  CPCAD   = <sf>,
  FMU     = <sf>
)
A clean, consistent spatial bundle for downstream simulation modules.

📤 Output Object
Output Name	Type	Description
EasternCanadaLandbase	list	Harmonized list of processed spatial layers

📚 Documentation
Full module documentation is available in:

Copy code
EasternCanadaDataPrep.Rmd
This includes:

module summary

inputs/outputs tables

event descriptions

saving/plotting behavior

acknowledgment + citations

🧩 Related Modules
This module feeds into:

LandR Biomass (pixel-group initialization)

AAC Calculator (yield curves + harvestable landbase)

SimpleHarvest / block-based harvest

Forest classification / yield-curve assignment

🤝 Getting Help
For questions, issues, or feature requests:

➡ GitHub Issues:
https://github.com/shirinvark/EasternCanadaDataPrep/issues

➡ SpaDES Help (Zulip):
https://spades.zulipchat.com

➡ SpaDES Documentation:
https://spades-core.predictiveecology.org

© Author
Shirin Varkouhi (Université Laval)
Contact: shirin.varkuhi@gmail.com
