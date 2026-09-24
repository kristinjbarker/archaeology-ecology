# 01A: prepare 30 m terrain; source this file, then call prepare_analysis_dems().
# Synthetic method validation is separate: validate_transformed_footprints.R.
# No solar/TPI processing here; old DEM products are never overwritten.

prepare_analysis_dems <- function(project_dir = getwd(), output_dir = NULL) {
  for (pkg in c("terra", "sf"))
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Required package: ", pkg)
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  crs30 <- "EPSG:3742"
  source_res <- 1 / 10800
  full_support <- 1 - 1e-6 # numerical tolerance, not a scientific acceptance threshold
  options_gdal <- c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")

  # 1. Discover inputs in reproducible order and check basic compatibility.
  files <- sort(list.files(file.path(project_dir, "data/raw/environment/dem"),
    pattern = "\\.tif(f)?$", full.names = TRUE, ignore.case = TRUE), method = "radix")
  if (!length(files)) stop("No source DEM TIFFs found.")
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  cat("Source DEM files:\n", paste(files, collapse = "\n"), "\n")
  if (any(!grepl("^USGS_13_n[0-9]{2}w[0-9]{3}_[0-9]{8}\\.tiff?$",
      basename(files), ignore.case = TRUE))) stop("Unexpected source filename; review the list.")
  tile_ids <- sub("_[0-9]{8}\\.tiff?$", "", tolower(basename(files)))
  if (anyDuplicated(tile_ids)) stop("Duplicate tile editions; select one edition per tile.")
  tiles <- lapply(files, terra::rast)
  for (i in seq_along(tiles)) {
    r <- tiles[[i]]
    if (terra::nlyr(r) != 1 || !terra::same.crs(r, "EPSG:4269") ||
        any(abs(terra::res(r) - source_res) > 1e-9))
      stop("Expected single-band NAD83 1/3-arc-second source: ", files[i])
  }
  # Inputs must have correct NoData metadata and elevations in metres with a
  # consistent vertical datum. No unit conversion or void filling is performed.

  # 2. Project the study polygon; snap its bounding box outward to the 30 m grid.
  rectangle <- function(e, crs) {
    e <- unname(e) # prevent names such as xmin.xmin in st_bbox()
    sf::st_as_sfc(sf::st_bbox(c(xmin=e[1], ymin=e[3], xmax=e[2], ymax=e[4]),
                             crs = sf::st_crs(crs)))
  }
  boundary <- sf::st_read(file.path(project_dir, "data/processed/arch-studyarea.shp"),
                          quiet = TRUE)
  if (!nrow(boundary) || is.na(sf::st_crs(boundary)) ||
      any(sf::st_is_empty(boundary)) || !all(sf::st_is_valid(boundary)))
    stop("Study boundary is empty, invalid, or lacks a CRS.")
  g <- sf::st_geometry(boundary)
  if (sf::st_is_longlat(g)) {
    original_crs <- sf::st_crs(g)
    # Densify straight lon/lat edges before projection, not great-circle arcs.
    g <- sf::st_set_crs(sf::st_segmentize(sf::st_set_crs(g, NA),
                                          dfMaxLength = 0.001), original_crs)
  }
  boundary30 <- sf::st_transform(g, crs30)
  b <- sf::st_bbox(boundary30)
  ae <- 30 * c(floor(b[["xmin"]]/30), ceiling(b[["xmax"]]/30),
               floor(b[["ymin"]]/30), ceiling(b[["ymax"]]/30))
  se <- ae + c(-10020, 10020, -10020, 10020)
  analysis_grid <- terra::rast(terra::ext(ae), resolution = 30, crs = crs30)
  solar_grid <- terra::rast(terra::ext(se), resolution = 30, crs = crs30)
  cat("Analysis extent (xmin,xmax,ymin,ymax):", ae, "\nSolar extent:", se, "\n")

  # 3. Use a fresh directory. Nothing is written to old DEM filenames.
  if (is.null(output_dir)) output_dir <- file.path(project_dir,
    "data/processed/environment", paste0("dem30_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (file.exists(output_dir)) stop("Refusing to reuse output directory: ", output_dir)
  if (!dir.create(output_dir, recursive = TRUE)) stop("Cannot create output directory.")
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  work <- file.path(output_dir, "intermediate")
  dir.create(work)
  old_temp <- terra::terraOptions(print = FALSE)$tempdir
  terra::terraOptions(tempdir = work)
  on.exit(terra::terraOptions(tempdir = old_temp), add = TRUE)
  writeLines(files, file.path(output_dir, "source_files.txt"))
  save_raster <- function(r, name, type = "FLT4S") {
    path <- file.path(output_dir, name)
    if (file.exists(path)) stop("Refusing to overwrite: ", path)
    terra::writeRaster(r, path, overwrite = FALSE, datatype = type, gdal = options_gdal)
  }
  average_to_solar <- function(source, destination) {
    if (file.exists(destination)) stop("Refusing to overwrite: ", destination)
    sf::gdal_utils("warp", source, destination, options = c(
      "-t_srs", crs30, "-te", as.character(se[c(1,3,2,4)]),
      "-ts", as.character(ncol(solar_grid)), as.character(nrow(solar_grid)),
      "-r", "average", "-et", "0", "-ovr", "NONE", "-ot", "Float32",
      "-dstnodata", "-999999", "-wo", "NUM_THREADS=1",
      "-co", "COMPRESS=LZW", "-co", "TILED=YES", "-co", "BIGTIFF=IF_SAFER"))
    terra::rast(destination)
  }

  # 4. Virtual geographic mosaic -> one direct average reprojection.
  # Last valid source in sorted order wins in tile overlaps. Tiny source-header
  # precision differences are standardized; there is no bilinear intermediate.
  vrt <- file.path(work, "source_mosaic.vrt")
  sf::gdal_utils("buildvrt", files, vrt, options = c("-strict", "-resolution", "user",
    "-tr", format(source_res, digits=17), format(source_res, digits=17),
    "-r", "nearest", "-vrtnodata", "-999999"))
  solar <- average_to_solar(vrt,
    file.path(output_dir, "dem_solar_30m_epsg3742_mean_10km.tif"))
  analysis <- save_raster(terra::crop(solar, terra::ext(analysis_grid), snap="near"),
                          "dem_analysis_30m_epsg3742_mean.tif")

  # 5. Measure support, including missing tiles AND areas outside the mosaic.
  # Pad the geographic validity raster beyond the entire solar target before
  # averaging. Otherwise GDAL can normalize only over the source overlap and
  # incorrectly report full support for a partly uncovered target cell.
  envelope <- sf::st_transform(sf::st_segmentize(
    rectangle(se + c(-120,120,-120,120), crs30), dfMaxLength=100), "EPSG:4269")
  source_extent <- terra::ext(terra::vect(envelope))
  source_window <- terra::crop(terra::rast(vrt), source_extent, snap="out")
  source_window <- terra::extend(source_window, source_extent, snap="out")
  validity <- terra::ifel(is.na(source_window), 0, 1) # missing terrain is numeric 0
  validity_file <- file.path(work, "source_validity.tif")
  terra::writeRaster(validity, validity_file, overwrite=FALSE,
                     datatype="INT1U", gdal=options_gdal)
  fraction <- average_to_solar(validity_file,
    file.path(output_dir, "dem_solar_30m_valid_fraction.tif"))
  if (terra::global(is.na(fraction) | fraction < -1e-6 | fraction > 1+1e-6,
                    "sum", na.rm=TRUE)[1,1] > 0)
    stop("Invalid coverage diagnostic; check validity raster extent/encoding.")
  valid <- save_raster(terra::ifel(is.na(solar), 0,
    terra::ifel(is.finite(solar) & fraction >= full_support, 1, 0)),
    "valid_terrain_solar_30m.tif", "INT1U")
  valid_analysis <- save_raster(terra::crop(valid, terra::ext(analysis_grid)),
                                "valid_terrain_analysis_30m.tif", "INT1U")
  # Partial-cell elevation means are retained, but are excluded by valid masks.
  # No missing elevation is filled, and no polygon mask is applied to terrain.
  analysis_mask <- save_raster(terra::rasterize(terra::vect(boundary30), analysis_grid,
    field=1, background=0, touches=TRUE), "analysis_mask_30m.tif", "INT1U")
  eligible <- save_raster(analysis_mask * valid_analysis,
                          "analysis_valid_elevation_mask_30m.tif", "INT1U")
  # This is cell-level elevation eligibility only. 01C must also require complete
  # neighborhoods for 100/250/500 m TPI radii and account for raster edges.
  # Solar mask does not certify a complete distant horizon around each cell.

  # 6. Check geometry/nesting and equality of the SAVED overlapping values.
  check_grid <- function(r, expected) {
    if (!terra::compareGeom(r, expected, stopOnError=FALSE) ||
        !terra::same.crs(r, crs30) || any(abs(terra::res(r)-30) > 1e-9) ||
        any(abs(as.vector(terra::ext(r))-as.vector(terra::ext(expected))) > 1e-7) ||
        any(abs(terra::origin(r)) > 1e-7)) stop("Output grid validation failed.")
  }
  for (r in list(solar, fraction, valid)) check_grid(r, solar_grid)
  for (r in list(analysis, valid_analysis, analysis_mask, eligible)) check_grid(r, analysis_grid)
  if (any(c(ae[1]-se[1], se[2]-ae[2], ae[3]-se[3], se[4]-ae[4]) != 10020))
    stop("Analysis/solar nesting validation failed.")
  overlap <- terra::crop(solar, terra::ext(analysis))
  differences <- is.na(analysis) != is.na(overlap) |
    terra::ifel(is.na(analysis) | is.na(overlap), FALSE, analysis != overlap)
  if (terra::global(differences, "sum", na.rm=TRUE)[1,1] != 0)
    stop("Saved analysis/solar elevations differ.")

  # 7. Concise coverage counts/area: informational, never a complete-envelope gate.
  count <- function(r) terra::global(r, "sum", na.rm=TRUE)[1,1]
  af <- terra::crop(fraction, terra::ext(analysis_grid))
  coverage <- data.frame(
    domain=c("solar rectangle", "analysis polygon"),
    total_cells=c(terra::ncell(solar), count(analysis_mask)),
    missing_elevation_cells=c(count(is.na(solar)), count(analysis_mask & is.na(analysis))),
    partial_support_cells=c(count(!is.na(solar) & fraction < full_support),
                            count(analysis_mask & !is.na(analysis) & af < full_support)),
    full_support_cells=c(count(valid), count(eligible)))
  coverage$missing_cell_area_km2 <- coverage$missing_elevation_cells * 900 / 1e6
  coverage$partial_cell_area_km2 <- coverage$partial_support_cells * 900 / 1e6
  print(coverage, row.names=FALSE)
  write.csv(coverage, file.path(output_dir, "coverage_summary.csv"), row.names=FALSE)
  cat("DEM products written to:", output_dir,
      "\nUse analysis_valid_elevation_mask_30m.tif plus later neighborhood checks for inference.\n")
  invisible(output_dir)
}
