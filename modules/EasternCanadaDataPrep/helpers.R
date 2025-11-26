# ============================================================
# helpers.R  — Utility functions for EasternCanadaDataPrep
# ============================================================

#' Make sure a directory exists
ensureDir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  return(path)
}

#' Check if a file exists; return TRUE/FALSE
fileExists <- function(path) {
  return(file.exists(path))
}

#' Simple standardized message formatting
msg <- function(...) {
  message(paste0("   ▶ ", ...))
}

#' Convert sf polygon to raster template
sfToRaster <- function(sfObj, template) {
  terra::rasterize(sfObj, template, field = 1)
}

#' Apply mask but handle missing layers safely
safeMask <- function(r, mask, maskvalues = 1) {
  if (is.null(mask)) return(r)
  return(terra::mask(r, mask, maskvalues = maskvalues))
}
