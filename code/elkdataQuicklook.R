library(tidyverse)
library(sf)

setwd("C:\\Users\\Kristin\\Downloads")
raw <- read.csv("animals.csv")
herds <- unique(raw$herd)
herds
herds2 <- unique(raw$commonHerd)
herds2
rawLocs <- read.csv("gps.csv")
prelimLocs <- rawLocs %>%
  left_join(raw, by = c("gps_sensors_code" = "animals_code")) %>%
  filter(commonHerd == "Clarks Fork" | 
                       commonHerd == "Clarks Fork or Cody" |
                       commonHerd == "Cody" |
                       commonHerd == "Wiggins Fork")
rm(rawLocs); gc()
area <- st_read("C:\Users\Kristin\Dropbox\Documents\Collaborations\Archaeology\HU10_SNF_Rasterized")
spLocs <- st_as_sf(prelimLocs)



##### restart w gabes lovely code

# packages
library(data.table)
library(tidyverse)
library(sf)
library(lubridate)

# wd
setwd("C:\\Users\\Kristin\\Dropbox\\Documents\\Collaborations\\Archaeology")

# data
sensorsAnimals <- fread("sensorsAnimals.csv")
animals <- fread("animals.csv")
gps <- fread("gps.csv")
#sensors <- fread("sensors.csv")



#taking relevant columns from sensorsAnimals and animals
sensAniCols <- sensorsAnimals %>% dplyr::select(gps_sensors_animals_id, animals_code, gps_sensors_code)
aniCols <- animals %>% dplyr::select(animals_code, herd, commonHerd, coreGYE, Fed)

#merging by animals_code, should be one row per deployment
infoCols <- merge(sensAniCols, aniCols)

#taking relevant columns from gps data
gpsCols <- gps %>% dplyr::select(gps_sensors_code, acquisition_time, latitude, longitude)

#merging by gps_sensors_code, should be one row per gps fix
gpsAnimalsData <- merge(infoCols, gpsCols, by = "gps_sensors_code", allow.cartesian=TRUE)

# store column names to add back to filtered data later
columns <- colnames(gpsAnimalsData)

###KJB pull relevant elk and lcocs to save time/memory
gpsAnimalsData <- filter(gpsAnimalsData,
                         commonHerd == "Clarks Fork" | 
                         commonHerd == "Clarks Fork or Cody" |
                         commonHerd == "Cody" |
                         commonHerd == "Wiggins Fork") %>%
  filter(!is.na(longitude)) %>%
  filter(longitude < -100) 

# save poor old laptop
rm(gps); gc()


#now ensuring that each individuals data is between the proper start/end date
filterToDeployment <- function(id) {
  
  #gets movement data
  indivData <- gpsAnimalsData %>% filter(gps_sensors_animals_id == id)

  #deployment start and end dates
  start <- filter(sensorsAnimals, gps_sensors_animals_id == id)$start_time
  end <- filter(sensorsAnimals, gps_sensors_animals_id == id)$end_time
  
  #filters to within appropriate interval, handles the various time formats
  if (end != "") {
    return(indivData %>% filter(parse_date_time(acquisition_time, orders = c("mdy_HM", "mdy", "ymd_HMS", "ymd")) >= parse_date_time(start, orders = c("mdy_HM", "mdy", "ymd_HMS", "ymd"))) %>% 
             filter(parse_date_time(acquisition_time, orders = c("mdy_HM", "mdy", "ymd_HMS", "ymd")) <= parse_date_time(end, c("mdy_HM", "mdy", "ymd_HMS", "ymd"))))
  } else {
    return(indivData %>% filter(parse_date_time(acquisition_time, orders = c("mdy_HM", "mdy", "ymd_HMS", "ymd")) >= parse_date_time(start, c("mdy_HM", "mdy", "ymd_HMS", "ymd"))))
  }
}

#may take a while, on my machine it took about 5 minutes
gpsAnimalsDataFiltered <- map_dfr(sensorsAnimals$gps_sensors_animals_id, filterToDeployment)
beepr::beep()

# save poor old laptop
rm(gpsAnimalsData); gc()

# make df for transition to sf
datPrelim <- as.data.frame(gpsAnimalsDataFiltered)
colnames(datPrelim) <- columns

# remove old locs, presumably from vhf collars
datPrelim <- filter(datPrelim, year(acquisition_time) > 2000)
head(datPrelim)

# run if memory gets bad again
# rm(gpsAnimalsDataFiltered); gc()

# projections
ll <- st_crs("+init=epsg:4326") # WGS 84
utm <- st_crs("+init=epsg:3742") # NAD83(HARN)/UTMzone12N 
aea <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")


# study area for archaeology
studyarea <- st_read("./HU10_SNF_Rasterized/HU10_SNF_Rasterized.shp")

# make locs spatial 
locsSf <- st_as_sf(datPrelim, 
                   coords = c("longitude", "latitude"),
                   crs = ll)

# match study area crs from paul's shp
locsSf <- st_transform(locsSf, crs = st_crs(studyarea))

# extract locs within study area
locsCrop <- st_filter(locsSf, studyarea)

# export
st_write(locsCrop, "elklocs_all.shp", delete_layer = TRUE)

