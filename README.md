![made-with-Markdown](https://img.shields.io/badge/Made%20with-Markdown-1f425f.svg)

# EasternCanadaDataPrep

**EasternCanadaDataPrep** is a SpaDES module that constructs a **raster-based landbase accounting framework** for Eastern Canada, defining where harvesting is **legally and physically possible**, independent of forest composition or management decisions.

This module prepares and harmonizes core spatial constraints—Forest Management Units (FMUs), protected and conserved areas (CPCAD), and hydrology-derived riparian influence—and produces a coarse-resolution planning raster and a cell-based landbase accounting table.

The outputs are designed to be consumed directly by downstream modules such as **LandR**, **AAC calculation**, and harvest allocation or scheduling modules.

---

## 🌎 Purpose

The purpose of this module is to answer a single, well-defined question:

> **Where is harvesting legally and physically allowed, and how much effective area is available, before applying any forest-type, age, or management rules?**

To achieve this, the module:

- Defines a consistent study area for Eastern Canada  
- Harmonizes all spatial layers to a common projection  
- Applies legal constraints (protected and conserved areas)  
- Applies physical constraints (riparian influence)  
- Produces a raster-based planning framework and a transparent accounting table  

This module intentionally **does not**:
- Interpret land cover or forest type  
- Apply silvicultural or management rules  
- Classify forest attributes  
- Construct vector-based harvest blocks  

---

## 🧭 Spatial Reference System

All spatial outputs are harmonized to:

**Canada Albers Equal Area Conic**  
**ESRI:102001**

---

## 📦 Input Objects

The module expects or generates the following input objects:

| Object       | Class              | Description |
|--------------|--------------------|-------------|
| `studyArea`  | `sf` / `SpatVector` | Polygon defining the modeling extent |
| `FMU`        | `sf` / `SpatVector` | Forest Management Unit boundaries |
| `CPCAD`      | `sf` / `SpatVector` | Protected and conserved areas |
| `Hydrology`  | `list`              | Hydrology-derived riparian fraction raster |

If not provided by the user, all inputs are **automatically created or downloaded** inside `.inputObjects()`.

---

## 🛠 What the module does

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
- Downloaded (Canada-wide)
- Cropped and masked to the study area
- Reprojected to ESRI:102001
- Rasterized to the planning resolution

FMUs define the **administrative units** for landbase accounting.

---

### ✔ Protected and Conserved Areas (CPCAD)

Protected areas are derived from the **Canadian Protected and Conserved Areas Database (CPCAD)**.

Processing includes:
- Cropping and masking to the study area
- Reprojection to ESRI:102001
- Policy-level filtering (no ecological interpretation)

Filtered CPCAD areas represent **legal exclusions** from harvesting.

---

### ✔ Hydrology – Riparian Influence

Hydrology is represented as a **wall-to-wall raster of riparian influence**, not as raw vector hydrology.

Processing workflow:
1. HydroRIVERS flowlines are downloaded and cropped
2. A uniform buffer is applied (`riparianBuffer_m`)
3. Buffered streams are rasterized to a coarse template (`hydroRaster_m`)
4. A **riparian fraction raster** is produced

Each cell value represents the **fraction of the cell area influenced by riparian buffers**, with values in `[0, 1]`.

This representation is:
- Scalable
- Memory-efficient
- Directly compatible with raster-based models

---

## 🧮 Planning Raster and Landbase Accounting

### Planning Raster

The planning raster is a **coarse-resolution (default: 250 m)** raster defining
the spatial units used for accounting and downstream modeling.

It is constructed from:
- Study area extent
- FMU geometry
- A fixed target resolution

No land-cover or forest information is used at this stage.

---

### Harvestable Mask

A binary harvestable mask is created where:

- Cells fall inside an FMU, **and**
- Cells are **not** inside protected areas

This mask represents **legal availability only**.

---

### Landbase Accounting Table

A transparent, cell-based accounting table is constructed for all harvestable cells.

For each planning cell, the table records:

- FMU identifier  
- Cell area (m²)  
- Riparian fraction (0–1)  
- Effective harvestable area after riparian constraint  

This table enables reproducible, auditable area calculations without repeated raster processing.

---

## 📤 Output Objects

| Output Name | Type | Description |
|------------|------|-------------|
| `PlanningRaster` | `SpatRaster` | Coarse-resolution planning raster |
| `LandbaseTable` | `data.frame` | Cell-based landbase accounting table |
| `EasternCanadaLandbase` | `list` | Structured container of all landbase products |

The main landbase object has the form:

```r
sim$EasternCanadaLandbase <- list(
  PlanningRaster   = <SpatRaster>,
  FMU_raster       = <SpatRaster>,
  HarvestableMask  = <SpatRaster>,
  RiparianFraction = <SpatRaster>,
  LandbaseTable    = <data.frame>
)



🔗 Relationship to Other Modules

EasternCanadaDataPrep provides foundational spatial constraints for:

LandR forest dynamics and biomass modules

Allowable Annual Cut (AAC) calculation modules

Harvest scheduling and allocation modules

Forest classification and yield-curve assignment modules

This module must be run before any forest dynamics or harvest simulation.

🤝 Getting Help

For issues, feature requests, or questions:

GitHub Issues:
https://github.com/shirinvark/EasternCanadaDataPrep/issues

SpaDES Documentation:
https://spades-core.predictiveecology.org

© Author
Shirin Varkouhi
Université Laval
📧 shirin.varkuhi@gmail.com