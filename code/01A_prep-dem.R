# ============================================================
# MAKE A DEM THAT COVERS THE (BUFFERED) STUDY AREA
# ============================================================

source("code/00_config.R")

##----------------##------------------##----------------

###########################################################################

#### RAW DEM ####  

  # Read in DEM tiles
  dem_files <- list.files(
    "data/raw/environment/dem",
    pattern = "\\.tif$",
    full.names = TRUE
  )
  
  dem_tiles <- lapply(dem_files, rast)
  
  # Stitch tiles together
  dem <- do.call(merge, dem_tiles)
  
  # Read in study area outline
  study_area_outline <- st_read("data/processed/arch-studyarea.shp")
  
  # Convert to terra vector
  study_area <- vect(study_area_outline)
  
  # Make CRS match DEM
  study_area <- project(study_area, crs(dem))
  
  # Crop to study-area extent, then mask to exact boundary
  dem_crop <- crop(dem, study_area)
  dem_final <- mask(dem_crop, study_area)
  
  # Save
  writeRaster(
    dem_final,
    "data/processed/environment/dem_latlong.tif",
    overwrite = TRUE
  )
 
  # convert to utms 
  dem_utm <- terra::project(dem_final, utm, method = "bilinear")
  
  # Save
  writeRaster(
    dem_utm,
    "data/processed/environment/dem_utms.tif",
    overwrite = TRUE
  )
  
  # aggregate to 30mx30m (from 10m) to speed computation
  dem30 <- terra::aggregate(dem_final, fact = 3, fun = mean)
  
  # Save
  writeRaster(
    dem30,
    "data/processed/environment/dem30.tif",
    overwrite = TRUE
  )
  
 


  
  
