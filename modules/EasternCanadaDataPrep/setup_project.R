# ================================================================
#   EasternCanadaProject — CLEAN & CORRECT PROJECT SETUP
#   GitHub-ready SpaDES project structure
#   Author: Shirin Varkouhi
# ================================================================

# -----------------------------
# 1) Install core dependencies
# -----------------------------
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

remotes::install_github("PredictiveEcology/Require", force = TRUE)
remotes::install_github("PredictiveEcology/reproducible", force = TRUE)
remotes::install_github("PredictiveEcology/SpaDES.core", force = TRUE)

# -----------------------------
# 2) Define project root
# -----------------------------
project_path <- "E:/EasternCanadaProject"
dir.create(project_path, showWarnings = FALSE, recursive = TRUE)
setwd(project_path)

# -----------------------------
# 3) Create standard folder structure
# -----------------------------
folders <- c(
  "modules",       # SpaDES modules
  "inputs",        # raw/downloaded data
  "cache",         # reproducible cache
  "outputs",       # processed rasters, tables
  "scratch"        # temporary workspace
)

for (f in folders) {
  dir.create(file.path(project_path, f), recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# 4) Prepare empty module structure (no overwriting!)
# -----------------------------
module_name <- "EasternCanadaDataPrep"
module_dir <- file.path(project_path, "modules", module_name)

dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(module_dir, "R"), showWarnings = FALSE)
dir.create(file.path(module_dir, "data"), showWarnings = FALSE)

# NOTE: We DO NOT create empty module files because the real module already exists.
# This script only prepares clean folders.

# -----------------------------
# 5) Create GitHub-friendly .gitignore
# -----------------------------
gitignore_content <- '
# ---------------------
# RStudio files
# ---------------------
.Rproj.user/
.Rhistory
.RData
.DS_Store

# ---------------------
# SpaDES outputs & caches
# ---------------------
cache/
outputs/
scratch/

# ---------------------
# Large geospatial data (do NOT commit)
# ---------------------
inputs/
*.zip
*.tif
*.tiff
*.gpkg
*.gdb
*.shp
*.shx
*.dbf
*.prj
*.xml
*.aux
*.sbn
*.sbx
'

writeLines(gitignore_content, file.path(project_path, ".gitignore"))

# -----------------------------
# DONE
# -----------------------------
cat("✔ PROJECT STRUCTURE CREATED SUCCESSFULLY.\n")
print(list.dirs(project_path, recursive = TRUE))
