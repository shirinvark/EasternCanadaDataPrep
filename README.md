![made-with-Markdown](https://img.shields.io/badge/Made%20with-Markdown-1f425f.svg)

# EasternCanadaDataPrep

Prepare and standardize all spatial layers required for the Eastern Canada landbase.  
This SpaDES module downloads, processes, crops, masks, filters, and reprojects datasets such as protected areas (CPCAD), land cover (LandCover, supplied upstream), forest management units (FMUs), and study area boundaries.

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
| `LandCover` | SpatRaster | Land cover raster (provided by upstream module) |
| `CPCAD`     | sf / SpatVector  | Protected areas                               |
| `FMU`       | sf / SpatVector  | Forest Management Units                       |

If not provided, `studyArea` can be auto-generated (default: ON, QC, NB, NS, PEI, NL).

---

## 🔌 External Interfaces

### LandCover (required upstream input)

EasternCanadaDataPrep does **not** generate land cover data.  
Instead, it expects a categorical **LandCover** raster to be supplied
by an upstream module (e.g. LandR, FireSense, or a dedicated LandCover module).

**Interface contract:**

- Object name: `sim$LandCover`
- Class: `terra::SpatRaster`
- Resolution: finer than the planning resolution (default 250 m)
- Values: categorical land-cover classes (e.g. forest / non-forest)
- CRS: must be projectable to the FMU and study area CRS

**Scope and limitations:**

This module:
- uses LandCover only to construct a 250 m planning raster (majority rule)
- does **not** reclassify land-cover types
- does **not** interpret forest type, merchantability, or age

All semantic interpretation of land cover is intentionally deferred
to downstream classification and AAC modules.

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

Creates a unified landbase object including:

```r
sim$EasternCanadaLandbase <- list(
  PlanningRaster = <SpatRaster>,
  LandbaseTable  = <data.frame>,
  CPCAD          = <sf>,
  FMU            = <sf>
)
```
A clean, consistent spatial bundle for downstream simulation modules.

## 📤 Output Objects

| Output Name              | Type | Description                                  |
|--------------------------|------|----------------------------------------------|
| `EasternCanadaLandbase`  | list | Harmonized landbase layers (planning raster, landbase table, FMU, CPCAD) |


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
