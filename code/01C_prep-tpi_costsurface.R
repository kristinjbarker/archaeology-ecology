# ============================================================
# TPI and cost surface
# ============================================================

source("code/00_config.R")


###########################################################################

# read in the DEM
dem <- rast("data/processed/environment/dem_utms.tif")

# define the three spatial extents (circle diameters, in m)
w100 <- focalMat(dem, d = 100, type = "circle")
mean100 <- focal(dem, 
                 w = w100, 
                 fun = "sum", 
                 na.rm = TRUE,
                 filename = "data/intermediate/mean-elev-100.tif",
                 overwrite = TRUE)
tpi100 <- dem - mean100


w250 <- focalMat(dem, d = 250, type = "circle")
w500 <- focalMat(dem, d = 500, type = "circle")

# calculate tpi at 100m scale
tpi100 <- focal(
  dem,
  w = w100,
  fun = function(x) x[ceiling(length(x) / 2)] - mean(x, na.rm = TRUE),
  na.rm = TRUE
)








tpi100 <- focal(
  dem,
  w = w100,
  fun = function(x) x[ceiling(length(x) / 2)] - mean(x),
  na.rm = TRUE,
  filename = "data/processed/environment/tpi_100m.tif",
  overwrite = TRUE
)

tpi250 <- focal(
  dem,
  w = w250,
  fun = function(x) x[ceiling(length(x) / 2)] - mean(x, na.rm = TRUE),
  na.rm = FALSE,
  filename = "data/processed/environment/tpi_250m.tif",
  overwrite = TRUE
)

tpi500 <- focal(
  dem,
  w = w500,
  fun = function(x) x[ceiling(length(x) / 2)] - mean(x, na.rm = TRUE),
  na.rm = FALSE,
  filename = "data/processed/environment/tpi_500m.tif",
  overwrite = TRUE
)
  # f.	rel500 [relative elevation 500 m]
  # g.	rel250
  # h.	rel100
  # i.	north
  # j.	east
  # k.	cost surface (see descrip)

  
  
  
###########################################################################

#### STORE ####  
  





##----------------##------------------##----------------

####### YLOH below is misc code to use however needed

# ----------------------------
# Paths (R Project relative)
# ----------------------------

raw_dir  <- "data/raw/environment"
out_file <- "data/processed/environment_stack.tif"

# ----------------------------
# Identify subfolders
# ----------------------------

layer_dirs  <- dir_ls(raw_dir, type = "directory")
layer_names <- path_file(layer_dirs)

if (length(layer_dirs) == 0) {
  stop("No subdirectories found in data/raw/environment/")
}

# ----------------------------
# Separate project area folder
# ----------------------------

proj_index <- which(tolower(layer_names) == "project area")

if (length(proj_index) != 1) {
  stop("There must be exactly one folder named 'project area'")
}

proj_dir <- layer_dirs[proj_index]
covar_dirs <- layer_dirs[-proj_index]
covar_names <- layer_names[-proj_index]

# ----------------------------
# Read project area polygon
# ----------------------------

proj_files <- dir_ls(proj_dir, glob = "*.shp")

if (length(proj_files) != 1) {
  stop("Project area folder must contain exactly one shapefile")
}

project_area <- vect(proj_files)

# Dissolve to single geometry (removes internal boundaries)
project_area <- aggregate(project_area)

proj_crs <- crs(project_area)

# ----------------------------
# Read covariate rasters
# ----------------------------

rasters <- map(covar_dirs, rast)
names(rasters) <- covar_names

# ----------------------------
# Reproject rasters to project CRS
# ----------------------------

rasters <- map(rasters, function(r) {
  if (crs(r) != proj_crs) {
    project(r, proj_crs)
  } else {
    r
  }
})

# ----------------------------
# Determine finest resolution
# ----------------------------

resolutions <- map_dbl(rasters, function(r) prod(res(r)))
finest_index <- which.min(resolutions)

template <- rasters[[finest_index]]

# ----------------------------
# Align resolution + origin
# ----------------------------

rasters <- map(rasters, function(r) {
  
  if (!compareGeom(r, template, stopOnError = FALSE, crs = TRUE)) {
    
    method <- if (is.factor(r)) "near" else "bilinear"
    
    r <- resample(r, template, method = method)
  }
  
  r
})

# ----------------------------
# Crop + mask to project area
# ----------------------------

rasters <- map(rasters, function(r) {
  r <- crop(r, project_area)
  r <- mask(r, project_area)
  r
})

# ----------------------------
# Combine into stack
# ----------------------------

env_stack <- rast(rasters)

# ----------------------------
# Write multi-layer GeoTIFF
# ----------------------------

writeRaster(
  env_stack,
  filename = out_file,
  overwrite = TRUE
)

message("Environmental stack built successfully.")

env_stack