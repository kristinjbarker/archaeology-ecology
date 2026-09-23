source("00_config.R")

# create shp of raw inventory data
rawArch <- read.csv("data/raw/archaeology/inventory-latePrehistoric/arch-inventory-data.csv")
spArch <- st_as_sf(rawArch, coords = c("EAST", "NORTH"), crs = ll)
st_write(spArch, "data/processed/arch-inventory-data.shp", append = TRUE)

# 
