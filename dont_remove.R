# Packages
# -----------------------------------------------------
library(reproducible)
library(terra)
library(sf)
library(fs)

# -----------------------------------------------------
# Paths
# -----------------------------------------------------
project_path <- "E:/EasternCanadaProject"
inputs <- file.path(project_path, "inputs")
fs::dir_create(inputs)

# -----------------------------------------------------
# Study area
# -----------------------------------------------------
ext <- terra::ext(-1e6, 1e6, -1e6, 1e6)
r0 <- terra::rast(ext, crs = "EPSG:5070", resolution = 10000)
studyArea <- terra::as.polygons(r0)[1]

message("✔ Study area ready.")

# =====================================================================
# 1) LCC 2020
# =====================================================================
message("▶ LCC2020... (using local ZIP if exists)")


lcc_dir <- file.path(inputs, "LCC2020")
fs::dir_create(lcc_dir)

local_zip_lcc <- file.path(lcc_dir, "land_cover_2020v2_30m_tif.zip")

# ✔ اگر ZIP را دستی گذاشتی → دانلود نمی‌کند
out <- reproducible::prepInputs(
  url = "https://www.cec.org/files/atlas_layers/1_terrestrial_ecosystems/1_01_0_land_cover_2020_30m/land_cover_2020v2_30m_tif.zip",
  destinationPath = lcc_dir,
  targetFile = "NA_NALCMS_landcover_2020v2_30m/NA_NALCMS_landcover_2020v2_30m.tif",
  archive = "land_cover_2020v2_30m_tif.zip",
  cropTo = studyArea,
  maskTo = studyArea,
  projectTo = studyArea
)


message("✔ LCC2020 ready.")

# =====================================================================
# 2) CPCAD 2024
# =====================================================================
message("▶ CPCAD 2024... (using local ZIP if exists)")

Require::Require("httr2")

cpcad_dir <- file.path(project_path, "inputs", "CPCAD")
dir.create(cpcad_dir, recursive = TRUE, showWarnings = FALSE)

cpcad <- reproducible::prepInputs(
  url = "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fprotectrestore%2Fcanadian-protected-conserved-areas-database%2FDatabases%2FProtectedConservedArea_2024.zip",
  destinationPath = cpcad_dir,
  archive = "ProtectedConservedArea_2024.zip",
  targetFile = "ProtectedConservedArea_2024.gdb",
  fun = "sf::st_read",
  layer = "ProtectedConservedArea_2024",
  cropTo = studyArea,
  maskTo = studyArea,
  projectTo = studyArea
)


message("✔ CPCAD ready.")

# =====================================================================
# 3) FMU Canada (Google Drive)
# =====================================================================
message("▶ FMU GPKG... (using local file if exists)")

fmu_dir <- file.path(inputs, "FMU")
fs::dir_create(fmu_dir)

local_fmu <- file.path(fmu_dir, "Forest_Management_Units_CA.gpkg")

Require::Require("httr2")

FMU <- reproducible::prepInputs(
  url = "https://drive.google.com/uc?export=download&id=1qp4TRgFArANp1YNEoOpeuwLlM-khf4v1",
  destinationPath = fmu_dir,
  targetFile = "Forest_Management_Units_CA.gpkg",
  archive = NULL,
  cropTo = studyArea,
  maskTo = studyArea,
  projectTo = studyArea
)


message("✔ FMU ready.")

# =====================================================================
# Summary
# =====================================================================
message("\n======================")
message("🎉 ALL DONE (prepInputs + local fallback) ")
message("======================")

print(out)
print(cpcad)
print(FMU)
