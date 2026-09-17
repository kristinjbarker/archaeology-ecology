# ============================================================
# Build unified environmental raster stack
# Geometry defined by project area polygon
# Resolution defined by finest raster
# Outputs aligned, masked GeoTIFF
# ============================================================

source("code/00_config.R")


###########################################################################

# read in the DEM
dem <- rast("data/processed/environment/dem30.tif")

# set path to GRASS (if error, check version number)
grass_dir <- file.path(osgeo4w_root, "apps/grass/grass85") 

# Core OSGeo4W env vars (normally set by OSGeo4W.bat / shell)
Sys.setenv(OSGEO4W_ROOT = osgeo4w_root)
Sys.setenv(GISBASE      = grass_dir)
Sys.setenv(GRASS_PYTHON = file.path(osgeo4w_root, "bin/python3.exe"))
Sys.setenv(PYTHONHOME   = file.path(osgeo4w_root, "apps/Python312"))  # match installed version
Sys.setenv(GDAL_DATA    = file.path(osgeo4w_root, "apps/gdal/share/gdal"))
Sys.setenv(PROJ_LIB     = file.path(osgeo4w_root, "share/proj"))

# Prepend all the relevant bin/lib folders to PATH so DLLs resolve
old_path <- Sys.getenv("PATH")
new_path <- paste(
  file.path(osgeo4w_root, "bin"),
  file.path(grass_dir, "bin"),
  file.path(grass_dir, "scripts"),
  file.path(grass_dir, "lib"),
  file.path(osgeo4w_root, "apps/Python312"),
  file.path(osgeo4w_root, "apps/Python312/Scripts"),
  old_path,
  sep = ";"
)
Sys.setenv(PATH = new_path)
  

# start GRASS
initGRASS(
  gisBase = grass_dir,
  home = tempdir(),
  gisDbase = tempdir(),
  location = "grass_location",
  mapset = "PERMANENT",
  SG = dem,
  override = TRUE
)

# # run this if the above didn't work
# unlink(file.path(tempdir(), "grass_location"), recursive = TRUE)

# identify the dem
execGRASS(
  "r.in.gdal",
  flags = "o",
  parameters = list(
    input = "data/processed/environment/dem30.tif",
    output = "dem_processed"
  )
)


#### CALCULATE RADIATION FROM DEM #### 


    ## winter ##
    
        winter_days = c(335, 1, 32)  # Dec 1, Jan 1, Feb 1
        
        winter_rad <- lapply(winter_days, function(day) {
          
          out_file <- tempfile(fileext = ".tif")
          
          execGRASS(
            "r.sun",
            flags = "overwrite",
            parameters = list(
              elevation = "dem_processed",
              glob_rad = paste0("winter_", day),
              day = day,
              step = 1
            )
          )
          
          execGRASS(
              "r.out.gdal",
              flags = "overwrite",
              parameters = list(
                input = paste0("winter_", day),
                output = out_file,
                format = "GTiff"
              )
            )
          
          rast(out_file)
        })
        
        winter_stack <- rast(winter_rad)
        winter <- mean(winter_stack, na.rm = TRUE)
        
        writeRaster(
          winter_stack,
          "data/processed/environment/winter_rad_stack.tif",
          overwrite = TRUE
        )
        
        writeRaster(
          winter,
          "data/processed/environment/solar_winter.tif",
          overwrite = TRUE
        )



    ## summer ##

    summer_days = c(166, 196, 227)  # Jun 15, Jul 15, Aug 15

    summer_rad <- lapply(summer_days, function(day) {
      
      out_file <- tempfile(fileext = ".tif")
      
      execGRASS(
        "r.sun",
        flags = "overwrite",
        parameters = list(
          elevation = "dem_processed",
          glob_rad = paste0("summer_", day),
          day = day,
          step = 1
        )
      )
      
      execGRASS(
        "r.out.gdal",
        flags = "overwrite",
        parameters = list(
          input = paste0("summer_", day),
          output = out_file,
          format = "GTiff"
        )
      )
      
      rast(out_file)
    })
    
    summer_stack <- rast(summer_rad)
    summer <- mean(summer_stack, na.rm = TRUE)
    
    writeRaster(
      summer_stack,
      "data/processed/environment/summer_rad_stack.tif",
      overwrite = TRUE
    )
    
    writeRaster(
      summer,
      "data/processed/environment/solar_summer.tif",
      overwrite = TRUE
    )
    