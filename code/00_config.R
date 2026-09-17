
#### USER-SPECIFIC INFO (FILE PATHS ETC) ####

    # GRASS software location, for calculating solar radiation
    osgeo4w_root <- "C:/Users/Kristin/AppData/Local/Programs/OSGeo4W"


#### PACKAGES ####

    ## (list needed; install missing; load all)
    packages <- c(
      "here", # id root folder
      "lubridate", # datetimes
      "cowplot", # muti-panel plotting
      "sf", # tidy spatdat
      "stringr", # cleaning names etc
      "skimr",  # wrangling data
      "terra", # raster data
      "fs", # something spatial
      "spatialEco", # circular tpi
      "rgrass", # solar radiation
      "tidyverse") # life
    ipak <- function(pkg){
      new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
      if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
      sapply(pkg, require, character.only = TRUE)
    }    
    ipak(packages) ; rm(ipak, packages)

    
#### DATA INFO ####

    # spatial projections  
    ll <- "EPSG:4326"   # WGS 84 geographic (lon/lat, degrees)
    utm <- "EPSG:3742"   # NAD83(HARN) / UTM zone 12N (meters)
    

    # projections of data
    crs_arch <- ll
    crs_ung <- utm


#### COMMON UNIT CONVERSIONS ####

    km2mi <- function(kms) {
      return(kms*0.6213712)
    }
    
    mi2km <- function(mis) {
      return(mis/0.6213712)
    }
    
    
#### FUNCTIONS ####
