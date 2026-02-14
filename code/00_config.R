#### PACKAGES ####

    ## (list needed; install missing; load all)
    packages <- c(
      "lubridate", # datetimes
      "cowplot", # muti-panel plotting
      "sf", # tidy spatdat
      "stringr", # cleaning names etc
      "skimr",  # wrangling data
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
    crs_ungulates <- 32157 # NAD83 / Wyoming West Central



#### COMMON UNIT CONVERSIONS ####

    km2mi <- function(kms) {
      return(kms*0.6213712)
    }
    
    mi2km <- function(mis) {
      return(mis/0.6213712)
    }
    
    
    
#### FUNCTIONS ####
