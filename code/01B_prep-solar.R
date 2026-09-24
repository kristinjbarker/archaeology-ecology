# 01B: potential daily irradiation on the actual terrain surface.
# Source this file, then explicitly call prepare_solar_covariates().
# No processing occurs on source(). Inputs are pinned to the reviewed 01A run.
# Units: Wh/m2/day; seasonal products are means of three representative days,
# not seasonal totals or estimates of actual weather-dependent radiation.

prepare_solar_covariates <- function(project_dir = getwd(),
    dem_dir = file.path(project_dir, "data/processed/environment/dem30_20260923_182040"),
    output_dir = NULL, step = 1, nprocs = 1L,
    osgeo4w_root = "C:/Users/Kristin/AppData/Local/Programs/OSGeo4W") {
  for (pkg in c("terra", "rgrass"))
    if (!requireNamespace(pkg, quietly=TRUE)) stop("Required package: ", pkg)
  stopifnot(length(step)==1, is.finite(step), step>0,
            length(nprocs)==1, is.finite(nprocs), nprocs>=1, nprocs==as.integer(nprocs))
  if (nzchar(Sys.getenv("GISRC"))) stop("Run in a fresh R session without active GRASS.")
  project_dir <- normalizePath(project_dir, winslash="/", mustWork=TRUE)
  dem_dir <- normalizePath(dem_dir, winslash="/", mustWork=TRUE)
  terrain_file <- file.path(dem_dir, "dem_solar_30m_epsg3742_mean_10km.tif")
  terrain <- terra::rast(terrain_file)
  analysis <- terra::rast(file.path(dem_dir, "dem_analysis_30m_epsg3742_mean.tif"))
  eligible <- terra::rast(file.path(dem_dir, "analysis_valid_elevation_mask_30m.tif"))
  same_grid <- function(r, reference) {
    if (!terra::compareGeom(r, reference, lyrs=FALSE, stopOnError=FALSE) ||
        !terra::same.crs(r, "EPSG:3742") || any(abs(terra::res(r)-30)>1e-9) ||
        any(abs(as.vector(terra::ext(r))-as.vector(terra::ext(reference)))>1e-7) ||
        any(abs(terra::origin(r))>1e-7)) stop("Exact 30 m grid check failed.")
  }
  same_grid(terrain, terrain); same_grid(analysis, analysis); same_grid(eligible, analysis)
  te <- unname(as.vector(terra::ext(terrain)))
  ae <- unname(as.vector(terra::ext(analysis)))
  if (any(c(ae[1]-te[1], te[2]-ae[2], ae[3]-te[3], te[4]-ae[4]) != 10020))
    stop("Expected canonical grid nested 10,020 m inside solar terrain.")

  # Fresh output directory protects old products and previous runs.
  if (is.null(output_dir)) output_dir <- file.path(project_dir,
    "data/processed/environment", paste0("solar_terrain30_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (file.exists(output_dir)) stop("Refusing to reuse output directory: ", output_dir)
  if (!dir.create(output_dir, recursive=TRUE)) stop("Cannot create output directory.")
  output_dir <- normalizePath(output_dir, winslash="/", mustWork=TRUE)
  for (sub in c("grassdb", "grass_home", "intermediate"))
    dir.create(file.path(output_dir, sub))
  old_temp <- terra::terraOptions(print=FALSE)$tempdir
  terra::terraOptions(tempdir=file.path(output_dir, "intermediate"))
  on.exit(terra::terraOptions(tempdir=old_temp), add=TRUE)
  writeLines(c(paste("DEM directory:", dem_dir), paste("Time step (hours):", step),
    paste("Threads:", nprocs), "Winter days: 335,1,32; summer days: 166,196,227",
    "Linke=3; albedo=0.2; distance_step=1; solar_constant=1367; shadows enabled.",
    "Terrain-surface clear-sky daily irradiation; Wh/m2/day.",
    "Three-day means require all three days; final outputs masked to elevation-eligible polygon."),
    file.path(output_dir, "solar_settings.txt"))

  # Same OSGeo4W initialization that worked in the benchmark.
  grass_dir <- file.path(osgeo4w_root, "apps/grass/grass85")
  python_home <- file.path(osgeo4w_root, "apps/Python312")
  if (!dir.exists(grass_dir) || !dir.exists(python_home)) stop("Check OSGeo4W paths.")
  env_names <- c("OSGEO4W_ROOT","GISBASE","GRASS_PYTHON","PYTHONHOME","GDAL_DATA",
    "PROJ_LIB","PATH","GISRC","GIS_LOCK","GRASS_REGION","WIND_OVERRIDE","OMP_NUM_THREADS")
  old_env <- Sys.getenv(env_names, unset=NA_character_)
  on.exit({
    for (key in env_names) {
      if (is.na(old_env[[key]])) Sys.unsetenv(key)
      else do.call(Sys.setenv, setNames(list(old_env[[key]]), key))
    }
  }, add=TRUE)
  Sys.unsetenv(c("GRASS_REGION","WIND_OVERRIDE"))
  Sys.setenv(OSGEO4W_ROOT=osgeo4w_root, GISBASE=grass_dir,
    GRASS_PYTHON=file.path(osgeo4w_root,"bin/python3.exe"), PYTHONHOME=python_home,
    GDAL_DATA=file.path(osgeo4w_root,"apps/gdal/share/gdal"),
    PROJ_LIB=file.path(osgeo4w_root,"share/proj"), OMP_NUM_THREADS=nprocs)
  Sys.setenv(PATH=paste(c(file.path(osgeo4w_root,"bin"),
    file.path(grass_dir,c("bin","scripts","lib")), python_home,
    file.path(python_home,"Scripts"), old_env[["PATH"]]), collapse=.Platform$path.sep))
  rgrass::initGRASS(gisBase=grass_dir, home=file.path(output_dir,"grass_home"),
    gisDbase=file.path(output_dir,"grassdb"), location="solar_terrain",
    mapset="PERMANENT", SG=terrain, override=TRUE)
  grass <- function(module, parameters=list(), flags=NULL, intern=FALSE) {
    result <- rgrass::execGRASS(module, parameters=parameters, flags=flags, intern=intern)
    status <- attr(result,"status")
    if (is.null(status) && is.numeric(result) && length(result)==1) status <- result
    if (!is.null(status) && status!=0) stop(module, " failed: ", status)
    result
  }
  # Fix initGRASS projection metadata, then recover exact bounds from imported DEM.
  grass("g.proj", list(epsg=3742), flags="c")
  grass("r.in.gdal", list(input=terrain_file, output="dem_terrain")) # no CRS override
  check_region <- function() {
    lines <- grass("g.region", flags="g", intern=TRUE)
    kv <- strsplit(lines[grepl("=",lines,fixed=TRUE)], "=", fixed=TRUE)
    values <- setNames(vapply(kv, function(x) as.numeric(x[2]), numeric(1)),
                       vapply(kv, function(x) x[1], character(1)))
    expected <- c(w=te[1], e=te[2], s=te[3], n=te[4], ewres=30, nsres=30,
                  rows=nrow(terrain), cols=ncol(terrain))
    if (anyNA(values[names(expected)]) || any(abs(values[names(expected)]-expected)>1e-7))
      stop("GRASS computational region differs from imported terrain grid.")
  }
  grass("g.region", list(raster="dem_terrain"))
  check_region()

  # Native GRASS degrees: aspect counterclockwise from east, flat=0.
  # No -n aspect conversion; no -e edge/NULL interpolation.
  grass("r.slope.aspect", list(elevation="dem_terrain", slope="terrain_slope",
    aspect="terrain_aspect", format="degrees", precision="FCELL", zscale=1, nprocs=nprocs))
  check_region()
  # Retain the full terrain for shadows. Do not apply an analysis GRASS MASK.
  # NA terrain is not filled; distant missing terrain can still bias shadows.
  days <- list(winter=c(335L,1L,32L), summer=c(166L,196L,227L))
  save_final <- function(r, filename) {
    same_grid(r, analysis)
    path <- file.path(output_dir, filename)
    if (file.exists(path)) stop("Refusing to overwrite: ", path)
    saved <- terra::writeRaster(r, path, overwrite=FALSE, datatype="FLT4S",
      gdal=c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER"))
    same_grid(terra::rast(path), analysis)
    saved
  }
  for (season in names(days)) {
    daily <- list()
    for (day in days[[season]]) {
      map <- sprintf("%s_d%03d", season, day)
      grass("g.region", list(raster="dem_terrain"))
      check_region()
      cat("Calculating", season, "day", day, "; step =", step, "hours\n")
      elapsed <- system.time(grass("r.sun", list(
        elevation="dem_terrain", slope="terrain_slope", aspect="terrain_aspect",
        glob_rad=map, day=day, step=step, linke_value=3, albedo_value=0.2,
        distance_step=1, solar_constant=1367, nprocs=nprocs)))[["elapsed"]]
      # No -p: terrain shadowing ON. No time parameter: daily integrated energy.
      cat("r.sun elapsed seconds:", elapsed, "\n")
      check_region()
      full_file <- file.path(output_dir,"intermediate",paste0(map,"_solar_extent.tif"))
      if (file.exists(full_file)) stop("Refusing to overwrite: ", full_file)
      grass("r.out.gdal", list(input=map, output=full_file, format="GTiff", type="Float32",
        nodata=-999999, createopt=c("COMPRESS=LZW","TILED=YES","BIGTIFF=IF_SAFER")),
        flags="c") # omit color table: continuous Float32 data
      full <- terra::rast(full_file)
      same_grid(full, terrain)
      cropped <- terra::crop(full, terra::ext(analysis), snap="near")
      same_grid(cropped, analysis) # check, never repair by resampling
      daily[[length(daily)+1L]] <- terra::mask(cropped, eligible, maskvalues=0)
    }
    stack <- terra::rast(daily)
    names(stack) <- sprintf("%s_day%03d", season, days[[season]])
    save_final(stack, paste0(season,"_rad_stack_terrain30.tif"))
    # Require the same three dates everywhere instead of averaging whichever exist.
    mean_rad <- mean(stack, na.rm=FALSE)
    save_final(mean_rad, paste0("solar_",season,"_terrain30.tif"))
    valid <- terra::ifel(is.na(mean_rad),0,1)
    save_final(valid, paste0("solar_",season,"_valid_mask_30m.tif"))
    cat(season, "valid analysis cells:",
        terra::global(valid,"sum",na.rm=TRUE)[1,1], "\n")
  }
  cat("Solar covariates written to:", output_dir, "\n")
  invisible(output_dir)
}
