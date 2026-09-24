# Temporary synthetic validation extracted from 01A; no real DEMs are read.
# Uses the previously inspected analysis extent to preserve identical sites.
# No scientific acceptance thresholds are applied.
run_footprint_validation <- function(output_dir = NULL) {
  for (pkg in c("terra", "sf")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Required package: ", pkg)
  }
  target_crs <- "EPSG:3742"
  geo_res <- 1 / 10800
  nodata <- -999999
  ae <- c(498570, 701610, 4785540, 5004060)
  if (is.null(output_dir)) output_dir <- file.path("benchmark-results",
    paste0("transformed_footprint_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  if (file.exists(output_dir)) stop("Output directory already exists.")
  dir.create(output_dir, recursive = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  work <- file.path(output_dir, "synthetic_rasters")
  dir.create(work)
  writeLines(c(capture.output(sessionInfo()), capture.output(sf::sf_extSoftVersion())),
    file.path(output_dir, "software_versions.txt"))
  # Strip existing extent names to avoid xmin.xmin etc. in st_bbox().
  # This correction is confined to this temporary script; 01A is unchanged.
  rectangle <- function(e, crs) {
    e <- unname(e)
    sf::st_as_sfc(sf::st_bbox(
      c(xmin = e[1], ymin = e[3], xmax = e[2], ymax = e[4]), crs = sf::st_crs(crs)))
  }
  densify_lonlat <- function(g, step = 0.001) {
    original_crs <- sf::st_crs(g)
    # Preserve straight coordinate-plane shapefile edges, not great-circle arcs.
    g <- sf::st_set_crs(g, NA)
    g <- sf::st_segmentize(g, dfMaxLength = step)
    sf::st_set_crs(g, original_crs)
  }
  template <- function(e) terra::rast(terra::ext(e), resolution = 30, crs = target_crs)
  save_raster <- function(r, path, datatype = "FLT4S") {
    if (file.exists(path)) stop("Refusing to overwrite: ", path)
    terra::writeRaster(r, path, overwrite = FALSE, datatype = datatype,
      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER"))
  }
  warp_average <- function(src, dst, grid) {
    if (file.exists(dst)) stop("Refusing to overwrite: ", dst)
    e <- as.vector(terra::ext(grid))
    opts <- c("-t_srs", target_crs,
      "-te", format(e[c(1, 3, 2, 4)], scientific = FALSE, trim = TRUE),
      "-ts", as.character(ncol(grid)), as.character(nrow(grid)),
      "-r", "average", "-et", "0", "-ovr", "NONE", "-ot", "Float32",
      "-dstnodata", as.character(nodata), "-wm", "256", "-wo", "NUM_THREADS=1",
      "-co", "COMPRESS=LZW", "-co", "TILED=YES", "-co", "BIGTIFF=IF_SAFER")
    sf::gdal_utils("warp", src, dst, options = opts, quiet = FALSE)
    terra::rast(dst)
  }

  # Independent projected polygon-overlap reference at SW, centre, NE sites.
  # 4x4 target cells per site; source matches native NAD83 resolution/lattice.
  # Patterned relief spans 100 m; plane has gradients 0.5 and 0.3 m/m.
  # NA case independently checks renormalized mean and valid fraction.
  # Report measured errors only; no scientific acceptance criteria.
  # These small tests cannot establish errors for all real terrain.
  sites <- rbind(c(ae[1] + 300, ae[3] + 300),
    c(mean(ae[1:2]), mean(ae[3:4])), c(ae[2] - 300, ae[4] - 300))
  rows <- list()
  for (s in seq_len(nrow(sites))) {
    xy <- floor(sites[s, ] / 30) * 30
    grid <- template(c(xy[1], xy[1] + 120, xy[2], xy[2] + 120))
    sb <- sf::st_bbox(sf::st_transform(sf::st_segmentize(
      rectangle(as.vector(terra::ext(grid)) + c(-100, 100, -100, 100), target_crs),
      dfMaxLength = 10), "EPSG:4269"))
    e <- c(floor(sb[["xmin"]] / geo_res), ceiling(sb[["xmax"]] / geo_res),
           floor(sb[["ymin"]] / geo_res), ceiling(sb[["ymax"]] / geo_res)) * geo_res
    src <- terra::rast(terra::ext(e), resolution = geo_res, crs = "EPSG:4269")
    # Explicit IDs stored in cells guarantee reference weights match raster order.
    ids <- src; terra::values(ids) <- seq_len(terra::ncell(ids)); names(ids) <- "sid"
    sp <- sf::st_as_sf(terra::as.polygons(ids, aggregate = FALSE))
    sf::st_geometry(sp) <- densify_lonlat(sf::st_geometry(sp), geo_res / 4)
    sp <- sf::st_transform(sp, target_crs)
    tids <- grid; terra::values(tids) <- seq_len(terra::ncell(tids)); names(tids) <- "tid"
    tp <- sf::st_as_sf(terra::as.polygons(tids, aggregate = FALSE))
    pieces <- suppressWarnings(sf::st_intersection(sp, tp))
    weights <- as.numeric(sf::st_area(pieces))
    centres <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(
      data.frame(terra::xyFromCell(src, seq_len(terra::ncell(src)))),
      coords = c("x", "y"), crs = 4269), target_crs))
    rc <- terra::rowColFromCell(src, seq_len(terra::ncell(src)))
    pattern <- 1000 + 100 * ((rc[, 1] + 2 * rc[, 2]) %% 5) / 4
    for (case in c("slope", "pattern", "pattern_na")) {
      z <- if (case == "slope") 1000 + 0.5 * (centres[, 1] - xy[1]) +
        0.3 * (centres[, 2] - xy[2]) else pattern
      if (case == "pattern_na") z[(rc[, 1] + rc[, 2]) %% 4 == 0] <- NA_real_
      terra::values(src) <- z
      prefix <- file.path(work, paste0("test_", s, "_", case))
      save_raster(src, paste0(prefix, "_source.tif"), "FLT8S")
      got <- terra::values(warp_average(paste0(prefix, "_source.tif"),
        paste0(prefix, "_mean.tif"), grid), mat = FALSE)
      valid <- src; terra::values(valid) <- as.integer(!is.na(z))
      save_raster(valid, paste0(prefix, "_valid.tif"), "INT1U")
      got_fraction <- terra::values(warp_average(paste0(prefix, "_valid.tif"),
        paste0(prefix, "_fraction.tif"), grid), mat = FALSE)
      expected <- fraction <- numeric(terra::ncell(grid))
      for (j in seq_along(expected)) {
        take <- pieces$tid == j
        zz <- z[pieces$sid[take]]; ww <- weights[take]
        if (abs(sum(ww) - 900) > 1e-3) stop("Synthetic source does not fully cover target.")
        ok <- !is.na(zz)
        expected[j] <- sum(zz[ok] * ww[ok]) / sum(ww[ok])
        fraction[j] <- sum(ww[ok]) / 900
      }
      rows[[length(rows) + 1L]] <- data.frame(site = s, case = case,
        cell = seq_along(expected), x = terra::xyFromCell(grid, seq_along(expected))[, 1],
        y = terra::xyFromCell(grid, seq_along(expected))[, 2],
        expected = expected, actual = got, error = got - expected,
        expected_fraction = fraction, actual_fraction = got_fraction,
        fraction_error = got_fraction - fraction)
    }
  }
  diagnostics <- do.call(rbind, rows)
  write.csv(diagnostics, file.path(output_dir, "transformed_footprint_validation.csv"), row.names = FALSE)
  checks <- do.call(rbind, lapply(split(diagnostics, interaction(diagnostics$site, diagnostics$case)),
    function(d) data.frame(site = d$site[1], case = d$case[1],
      mae_m = mean(abs(d$error)), max_error_m = max(abs(d$error)),
      fraction_mae = mean(abs(d$fraction_error)), bias_m = mean(d$error),
      max_fraction_error = max(abs(d$fraction_error)))))
  print(checks)
  write.csv(checks, file.path(output_dir, "transformed_footprint_summary.csv"), row.names = FALSE)

  if (any(!is.finite(diagnostics$error)) || any(!is.finite(diagnostics$fraction_error))) {
    stop("Nonfinite diagnostic result; inspect outputs.")
  }
  cat("RESULT DIRECTORY:", output_dir, "\n")
  invisible(list(summary = checks, cells = diagnostics, output_dir = output_dir))
}
