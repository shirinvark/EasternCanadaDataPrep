![made-with-Markdown](https://img.shields.io/badge/Made%20with-Markdown-1f425f.svg)

# EasternCanadaDataPrep

**EasternCanadaDataPrep** is a SpaDES module that prepares and harmonizes
core spatial inputs for Eastern Canada, defining the **planning framework**
used by downstream landbase, hydrology, biomass, and harvest modules.

This module focuses exclusively on **spatial data preparation and legal availability**.
It intentionally avoids ecological interpretation, forest classification,
or policy-specific constraints that are applied later in the modeling pipeline.

---

## 🌎 Purpose

The purpose of this module is to answer a single, well-defined question:

> **Where are spatial planning units defined, and which areas are legally
> available for harvesting, before applying ecological, hydrological,
> or management-specific rules?**

To achieve this, the module:

- Defines a consistent study area for Eastern Canada  
- Harmonizes all spatial layers to a common projection  
- Prepares administrative units (FMUs)  
- Applies legal constraints (protected and conserved areas)  
- Constructs a coarse-resolution planning grid  
- Prepares hydrology inputs for downstream processing  

This module intentionally **does not**:

- Interpret land cover or forest type  
- Apply silvicultural or management rules  
- Apply hydrological buffering or riparian constraints  
- Estimate biomass, age, or productivity  
- Construct harvest blocks or schedules  

---

## 🧭 Spatial Reference System

All spatial outputs are harmonized to:

**Canada Albers Equal Area Conic**  
**ESRI:102001**

---

## 📦 Input Objects

The module expects or generates the following input objects:

| Object      | Class               | Description |
|------------|---------------------|-------------|
| `studyArea` | `sf` / `SpatVector` | Polygon defining the modeling extent |
| `FMU`       | `sf` / `SpatVector` | Forest Management Unit boundaries |
| `CPCAD`     | `sf` / `SpatVector` | Protected and conserved areas |
| `Hydrology` | `list`              | Raw hydrology inputs (flowlines) |

If not provided by the user, all inputs are **automatically created or downloaded**
inside `.inputObjects()`.

---

## 🛠 What the Module Does

### ✔ Study Area

If no study area is provided, the module automatically constructs a default
Eastern Canada study area including:

- Ontario  
- Québec  
- New Brunswick  
- Nova Scotia  
- Prince Edward Island  
- Newfoundland and Labrador  

---

### ✔ Forest Management Units (FMUs)

FMUs are:

- Downloaded at the national scale  
- Cropped and masked to the study area  
- Reprojected to ESRI:102001  
- Rasterized to the planning resolution  

FMUs define the **administrative units** used by downstream accounting
and harvest-related modules.

---

### ✔ Protected and Conserved Areas (CPCAD)

Protected areas are derived from the **Canadian Protected and Conserved Areas Database (CPCAD)**.

Processing includes:

- Cropping and masking to the study area  
- Reprojection to ESRI:102001  
- Policy-level filtering only (no ecological interpretation)  

Filtered CPCAD areas represent **legal exclusions** from harvesting.

---

### ✔ Hydrology – Raw Inputs

Hydrology inputs are prepared as **raw vector flowlines** derived from
**HydroRIVERS**.

This module:

- Downloads and crops HydroRIVERS flowlines  
- Reprojects them to the project CRS  
- Stores them for downstream use  

No buffering, rasterization, or riparian constraints are applied at this stage.

Hydrological buffering and riparian influence are intentionally handled
in downstream modules once jurisdiction- and policy-specific rules
are defined.

---

## 🧮 Planning Grid and Legal Availability

### Planning Raster

The planning raster is a **coarse-resolution grid (default: 250 m)** defining
the spatial units used by downstream models.

It is constructed from:

- Study area extent  
- A fixed target resolution  
- Harmonized spatial reference system  

No land-cover, forest-type, or biomass information is used at this stage.

---

### Harvestable Mask

A binary harvestable mask is created where:

- Cells fall inside an FMU, **and**  
- Cells are **not** inside protected or conserved areas  

This mask represents **legal availability only** and does not include
ecological or hydrological constraints.

---

## 📤 Output Objects

| Output Name | Type | Description |
|------------|------|-------------|
| `PlanningRaster` | `SpatRaster` | Coarse-resolution planning grid |
| `EasternCanadaLandbase` | `list` | Planning grid and legal availability masks |

The main landbase object has the form:

```r
sim$EasternCanadaLandbase <- list(
  PlanningRaster  = <SpatRaster>,
  FMU_raster      = <SpatRaster>,
  HarvestableMask = <SpatRaster>
)
