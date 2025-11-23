#===============================================================
# EasternCanadaDataPrep Module
# Authors: Shirin Varkuhi & Tyler Rudolph
# Description: Prepare spatial layers for Eastern Canada
#===============================================================

defineModule(sim, list(
  name = "EasternCanadaDataPrep",
  description = "Prepare spatial layers for Eastern Canada",
  authors = c(
    person("Shirin", "Varkuhi",
           email = "shirin.varkuhi@gmail.com",
           role = c("aut", "cre")),
    person("Tyler", "Rudolph",
           email = "tyler.rudolph@nrcan-rncan.gc.ca",
           role = c("aut"))
  ),
  version = list(major = 0, minor = 1),
  
  parameters = bindrows(
    defineParameter("useUserStudyArea", "logical", FALSE,
                    "If TRUE, use user-provided studyArea"),
    defineParameter("defaultStudyArea", "character", "EasternCanada",
                    "Default study area if user does not provide one")
  ),
  
  inputs = data.frame(),
  outputs = data.frame()
))

##########################################################################################################
doEvent.EasternCanadaDataPrep <- function(sim, eventTime, eventType) {
  switch(eventType,
         
         "init" = {
           message("🔵 EasternCanadaDataPrep initialized.")
           
           ## -------------------------------
           ## Create or use studyArea
           ## -------------------------------
           if (!param(sim)$EasternCanadaDataPrep$useUserStudyArea) {
             message("🔷 Creating real Eastern Canada studyArea…")
             
             library(sf)
             library(dplyr)
             library(rnaturalearth)
             
             can <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf")
             
             east <- can %>% 
               dplyr::filter(name_en %in% c("Ontario",
                                            "Quebec",
                                            "New Brunswick",
                                            "Nova Scotia",
                                            "Prince Edward Island",
                                            "Newfoundland and Labrador"))
             
             east_union <- sf::st_union(east)
             east_5070 <- sf::st_transform(east_union, 5070)
             sim$studyArea <- east_5070
           } else {
             message("🔷 Using user-provided studyArea")
           }
           
           message("🟩 StudyArea summary:")
           print(sf::st_bbox(sim$studyArea))
           message("   CRS: ", terra::crs(sim$studyArea))
           
           ## Schedule events
           sim <- scheduleEvent(sim, time(sim), "EasternCanadaDataPrep", "downloadData")
           sim <- scheduleEvent(sim, time(sim), "EasternCanadaDataPrep", "loadInputs")
           sim <- scheduleEvent(sim, time(sim), "EasternCanadaDataPrep", "buildLandbase")
           sim <- scheduleEvent(sim, time(sim), "EasternCanadaDataPrep", "saveOutputs")
         },
         
         "downloadData" = {
           sim <- downloadLCC(sim)
           sim <- downloadCPCAD(sim)
           sim <- downloadFMU(sim)
           message("✔ All downloads completed.")
         },
         
         "loadInputs" = {
           sim <- loadInputs(sim)
         },
         
         "buildLandbase" = {
           sim <- buildLandbase(sim)   # فقط فراخوانی
         },
         
         "saveOutputs" = {
           sim <- saveOutputs(sim)
         }
         
  ) # end switch
  
  return(invisible(sim))
}

#########################################################################################################
downloadLCC <- function(sim) {
  
  # Packages (no change)
  library(reproducible)
  library(terra)
  library(sf)
  library(fs)
  
  message("▶ LCC2020... (using local ZIP if exists)")
  
  project_path <- sim@paths$inputPath
  inputs <- project_path
  fs::dir_create(inputs)
  
  studyArea <- sim$studyArea
  
  lcc_dir <- file.path(inputs, "LCC2020")
  fs::dir_create(lcc_dir)
  
  local_zip_lcc <- file.path(lcc_dir, "land_cover_2020v2_30m_tif.zip")
  
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
  
  return(sim)
}
################################################################################################################
downloadCPCAD <- function(sim) {
  
  # Packages
  library(reproducible)
  library(terra)
  library(sf)
  library(httr2)
  library(fs)
  
  message("▶ CPCAD 2024... (using local ZIP if exists)")
  
  project_path <- sim@paths$inputPath
  cpcad_dir <- file.path(project_path, "CPCAD")
  dir.create(cpcad_dir, recursive = TRUE, showWarnings = FALSE)
  
  studyArea <- sim$studyArea
  
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
  
  return(sim)
}
################################################################################################################
downloadFMU <- function(sim) {
  
  library(reproducible)
  library(terra)
  library(sf)
  library(httr2)
  library(fs)
  
  message("▶ FMU GPKG... (using local file if exists)")
  
  studyArea <- sim$studyArea
  
  project_path <- sim@paths$inputPath
  fmu_dir <- file.path(project_path, "FMU")
  fs::dir_create(fmu_dir)
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
  
  return(sim)
}
#############################################################################################################
loadInputs <- function(sim) {
  message("▶ Loading inputs...")
  
  library(terra)
  library(fs)
  library(sf)
  
  project_path <- sim@paths$inputPath
  
  ## 1) LCC2020
  lcc_file <- file.path(project_path, "LCC2020", 
                        "NA_NALCMS_landcover_2020v2_30m/NA_NALCMS_landcover_2020v2_30m.tif")
  
  if (file.exists(lcc_file)) {
    message("   ✔ LCC2020 found. Loading...")
    sim$LCC2020 <- terra::rast(lcc_file)
  } else {
    message("   ❗ LCC2020 NOT found.")
  }
  
  ## 2) CPCAD 2024
  cpcad_gdb <- file.path(project_path, "CPCAD", "ProtectedConservedArea_2024.gdb")
  
  if (dir.exists(cpcad_gdb)) {
    message("   ✔ CPCAD found. Loading...")
    sim$CPCAD <- sf::st_read(cpcad_gdb, layer = "ProtectedConservedArea_2024", quiet = TRUE)
  } else {
    message("   ❗ CPCAD NOT found.")
  }
  
  ## 3) FMU
  fmu_file <- file.path(project_path, "FMU", "Forest_Management_Units_CA.gpkg")
  
  if (file.exists(fmu_file)) {
    message("   ✔ FMU found. Loading...")
    sim$FMU <- sf::st_read(fmu_file, quiet = TRUE)
  } else {
    message("   ❗ FMU NOT found.")
  }
  
  return(sim)
}
#############################################################################################################
buildLandbase <- function(sim) {
  message("▶ buildLandbase(): Building landbase (Phase 1 + Phase 2 + Phase 3)...")
  
  library(terra)
  library(sf)
  
  ## ===========================================================================
  ## PHASE 1 — Forest Mask از LCC2020
  ## ===========================================================================
  if (is.null(sim$LCC2020)) {
    stop("❗ LCC2020 not loaded. Cannot build landbase.")
  }
  
  lcc <- sim$LCC2020
  
  forest_classes <- c(1, 2, 3)  # Needleleaf, Broadleaf, Mixed
  
  forest_mask <- lcc
  forest_mask[!values(lcc) %in% forest_classes] <- NA
  forest_mask[values(lcc) %in% forest_classes] <- 1
  
  message("   ✔ Forest mask created.")
  
  ## ===========================================================================
  ## PHASE 2 — حذف Protected Areas
  ## ===========================================================================
  if (!is.null(sim$CPCAD)) {
    message("   ▶ Removing Protected Areas...")
    
    pa_raster <- terra::rasterize(sim$CPCAD, forest_mask, field = 1)
    
    forest_mask <- terra::mask(forest_mask, pa_raster,
                               maskvalues = 1,
                               updatevalue = NA)
    
    message("   ✔ Protected areas removed.")
  } else {
    message("   ❗ CPCAD not found → skipping PA removal.")
  }
  
  ## ===========================================================================
  ## PHASE 3 — اعمال FMU Mask
  ## ===========================================================================
  if (!is.null(sim$FMU)) {
    message("   ▶ Applying FMU mask...")
    
    fmu_raster <- terra::rasterize(sim$FMU, forest_mask, field = 1)
    
    forest_mask <- terra::mask(forest_mask, fmu_raster,
                               maskvalues = NA)
    
    message("   ✔ FMU mask applied.")
  } else {
    message("   ❗ FMU not found → skipping FMU masking.")
  }
  
  ## ===========================================================================
  ## ذخیره خروجی نهایی
  ## ===========================================================================
  sim$landbase <- forest_mask
  sim$forestMask <- forest_mask
  
  message("✔ Landbase (Phase 1+2+3) completed.")
  return(sim)
}


#############################################################################################################
saveOutputs <- function(sim) {
  message("▶ saveOutputs(): Saving final outputs...")
  
  library(terra)
  library(sf)
  library(fs)
  
  outDir <- sim@paths$outputPath
  fs::dir_create(outDir)
  
  ## ------------------------------------------------------------
  ## 1) Save landbase raster
  ## ------------------------------------------------------------
  if (!is.null(sim$landbase)) {
    landbase_file <- file.path(outDir, "landbase_final.tif")
    terra::writeRaster(sim$landbase, landbase_file, overwrite = TRUE)
    message("   ✔ landbase_final.tif saved.")
  } else {
    message("   ❗ landbase not found, skipping.")
  }
  
  ## ------------------------------------------------------------
  ## 2) Save forestMask raster
  ## ------------------------------------------------------------
  if (!is.null(sim$forestMask)) {
    fm_file <- file.path(outDir, "forestMask.tif")
    terra::writeRaster(sim$forestMask, fm_file, overwrite = TRUE)
    message("   ✔ forestMask.tif saved.")
  } else {
    message("   ❗ forestMask not found, skipping.")
  }
  
  ## ------------------------------------------------------------
  ## 3) Save studyArea shapefile/geopackage
  ## ------------------------------------------------------------
  if (!is.null(sim$studyArea)) {
    study_file <- file.path(outDir, "studyArea.gpkg")
    sf::st_write(sim$studyArea, study_file, delete_dsn = TRUE, quiet = TRUE)
    message("   ✔ studyArea.gpkg saved.")
  } else {
    message("   ❗ studyArea missing, skipping.")
  }
  
  ## ------------------------------------------------------------
  ## 4) Write a simple log file
  ## ------------------------------------------------------------
  logFile <- file.path(outDir, "output_log.txt")
  writeLines(c(
    paste0("EasternCanadaDataPrep outputs saved on ", Sys.time()),
    "Files:",
    " - landbase_final.tif",
    " - forestMask.tif",
    " - studyArea.gpkg"
  ), logFile)
  
  message("✔ All outputs saved.")
  return(sim)
}


