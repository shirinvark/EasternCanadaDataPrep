# ================================================================
#   EasternCanadaProject — CLEAN & CORRECT PROJECT SETUP
#   Makes a GitHub-friendly SpaDES project structure
#   Author: Shirin Varkuhi
# ================================================================

# -----------------------------
# 1) Project root
# -----------------------------
project_path <- "E:/EasternCanadaProject"
dir.create(project_path, showWarnings = FALSE, recursive = TRUE)
setwd(project_path)

# -----------------------------
# 2) Create correct folders
# -----------------------------
folders <- c(
  "modules",       # SpaDES modules
  "inputs",        # downloaded data, raw data, gdb, zip...
  "cache",         # reproducible cache
  "output",        # final rasters, tables, maps
  "scratch"        # temporary files
)

for (f in folders) {
  dir.create(file.path(project_path, f), recursive = TRUE, showWarnings = FALSE)
}

# -----------------------------
# 3) Create module folder structure
# -----------------------------
module_name <- "EasternCanadaDataPrep"

module_dir <- file.path(project_path, "modules", module_name)
dir.create(module_dir, showWarnings = FALSE, recursive = TRUE)

dir.create(file.path(module_dir, "R"), showWarnings = FALSE)
dir.create(file.path(module_dir, "data"), showWarnings = FALSE)

# Empty module files (to be filled later)
file.create(file.path(module_dir, "DESCRIPTION"))
file.create(file.path(module_dir, "R", paste0(module_name, ".R")))
file.create(file.path(module_dir, "R", "helpers.R"))

# -----------------------------
# 4) Create .gitignore
# -----------------------------
gitignore_content <- '
# RStudio
.Rproj.user/
.Rhistory
.RData
.DS_Store

# Project structure
cache/
inputs/
output/
scratch/

# Common large files
*.zip
*.tif
*.gpkg
*.gdb
*.shp
*.shx
*.dbf
'

writeLines(gitignore_content, file.path(project_path, ".gitignore"))

# -----------------------------
# DONE
# -----------------------------
cat("✔ PROJECT STRUCTURE CREATED SUCCESSFULLY.\n")
print(list.dirs(project_path, recursive = TRUE))
