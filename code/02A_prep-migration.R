### GOAL: create rasters representing ungulate migration from the USGS migration lines

source("code/00_config.R")

# where to find raw data
wildlife_dir <- "data/raw/wildlife"

#  where to store output
out_dir <- "data/processed/wildlife"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


###########################################################################

#### MIGRATION LINES ####

  # find all USGS migration line files
  route_files <- list.files(
    path       = wildlife_dir,
    pattern    = "(Routes|Lines).*\\.shp$",  # contains "Routes", ends in ".shp"
    recursive  = TRUE,               # search all subfolders, any depth
    full.names = TRUE,
    ignore.case = FALSE              # set TRUE if you want to catch "routes" too
  )
  
  # list species names 
  species <- route_files |>
    str_remove(paste0("^", wildlife_dir, "/")) |>
    str_extract("^[^/]+")
  
  # read in each migration file and map to correct species
  route_list <- map(
    route_files, 
    ~ st_read(.x, quiet = TRUE) |>
      st_transform(st_crs(utm)))
  names(route_list) <- species
  
  # combine shps for each species
  routes_by_species <- split(route_list, species) |>
    map(~ bind_rows(.x))
  
  
###########################################################################

#### MIGRATION AREA POLYGONS ####  
  
  
  # buffer each line by 500m & merge per spp (Merkle et al 2024)
  species_migration_areas <- map2(
    routes_by_species,
    names(routes_by_species),
    ~ st_sf(
      species = .y,
      geometry = st_union(st_buffer(.x, 500))
    )
  ) |>
    bind_rows()
 

  # export shps of migration area per species
  for (sp in names(species_migration_areas)) {
    
    # Clean species name for filenames
    sp_name <- gsub("[^A-Za-z0-9]+", "_", sp)
    
    # --- Migration area polygon ---
    st_write(
      species_migration_areas[species_migration_areas$species == sp, ],
      file.path(out_dir, paste0(sp_name, "_migration_area.shp")),
      delete_layer = TRUE,
      quiet = TRUE
    )
  } 

  
  
###########################################################################

#### MIGRATION AREA RASTERS #### 

  ## prep ##
  
    # use existing dem file for grid template
    template <- rast("data/processed/environment/dem_study_area.tif")
    
    # Convert species polygons to terra
    migration_vect <- vect(species_migration_areas)
    
    # match CRS of dem
    migration_vect <- project(migration_vect, crs(template))
    
    
  ## create and store rasters ##

    # binary in/out of migration area
    
    for (sp in unique(migration_vect$species)) {
      
      # clean up species name
      sp_name <- gsub("[^A-Za-z0-9]+", "_", sp)
      
      # for this species,
      poly <- migration_vect[migration_vect$species == sp, ]
      
      # rasterize mig area polygon into present/absent
      r_binary <- rasterize(
        poly,
        template,
        field = 1,
        background = 0
      )
      
      # specify export info
      binary_file <- file.path(
        out_dir,
        paste0(sp_name, "_migration_area_binary.tif")
      )
      
      # export
      writeRaster(
        r_binary,
        binary_file,
        overwrite = TRUE,
        datatype = "INT1U"
      )
      
      # Remove polygon and binary raster from memory
      rm(poly, r_binary)
      gc()
    }
      
    
