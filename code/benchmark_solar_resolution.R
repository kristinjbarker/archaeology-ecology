# Temporary decision-making benchmark; NOT a numbered production script.
# Sourcing this file only defines a function. Nothing runs until explicitly called:
# source("code/benchmark_solar_resolution.R")
# run_solar_resolution_benchmark(project_dir = getwd())
# Run in a fresh R session, with terra and rgrass already installed.
# Does not source 00_config.R, install packages, or overwrite production data.
#
# Site: EPSG:3742 center (615000, 4860000), approximately
# 43.88414 N, 109.56842 W. Read-only 60 m-spaced reconnaissance found:
# core elevations ~2829--3539 m, all four aspect quadrants, and both negative
# and positive local elevation contrasts (valley/ridge terrain). Sampled core
# and 3 km buffer were entirely valid. Full validity is checked when run.
# The 3 km buffer is an EXPERIMENTAL terrain context, not a production extent
# or a demonstrated sufficient horizon distance. Distant mountains may matter.
#
# Three branches: native ~8.49 m; bilinear 30 m; footprint-mean 30 m.
# Derive slope/aspect independently on EACH branch's own DEM grid.
# Fine radiation is averaged by horizontal/map overlap area, not slope-surface
# area. This is mean tilted-surface irradiation over a map footprint, NOT
# total intercepted energy or a true-surface-area-weighted mean.
#
# References:
# https://grass.osgeo.org/grass-stable/manuals/r.slope.aspect.html
# https://grass.osgeo.org/grass-stable/manuals/r.sun.html
# https://rspatial.github.io/terra/reference/resample.html
# https://gdal.org/en/stable/programs/gdalwarp.html

run_solar_resolution_benchmark <- function(
    project_dir = getwd(), output_dir = NULL,
    osgeo4w_root = "C:/Users/Kristin/AppData/Local/Programs/OSGeo4W",
    nprocs = 1L) {
  for (pkg in c("terra", "rgrass")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install first: ", pkg)
  }
  if (nzchar(Sys.getenv("GISRC"))) {
    stop("Use a fresh R session without an active GRASS session.")
  }
  stopifnot(length(nprocs) == 1L, is.finite(nprocs), nprocs >= 1,
            nprocs == as.integer(nprocs))
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  source_file <- file.path(project_dir, "data/processed/environment/dem_utms.tif")
  grass_dir <- file.path(osgeo4w_root, "apps/grass/grass85")
  python_home <- file.path(osgeo4w_root, "apps/Python312")
  if (!file.exists(source_file)) stop("Missing source DEM: ", source_file)
  if (!dir.exists(grass_dir) || !dir.exists(python_home)) {
    stop("Review OSGeo4W/GRASS/Python paths before running.")
  }

  # ONE explicit 30 m grid: boundaries are multiples of 30 m in EPSG:3742.
  # Core = 80 x 80 cells (2.4 km); domain = 280 x 280 cells (8.4 km).
  core_extent <- terra::ext(613800, 616200, 4858800, 4861200)
  domain_extent <- terra::ext(610800, 619200, 4855800, 4864200)
  target30 <- terra::rast(domain_extent, nrows = 280, ncols = 280,
                          crs = "EPSG:3742")
  core30 <- terra::rast(core_extent, nrows = 80, ncols = 80,
                        crs = "EPSG:3742")
  stopifnot(all(terra::res(target30) == 30), all(terra::origin(target30) == 0))
  days <- c(jan01 = 1L, jun15 = 166L)
  kinds <- c("global", "beam", "sunhours")
  min_coverage <- 1 - 1e-6
  src <- terra::rast(source_file)
  if (terra::nlyr(src) != 1L || !terra::same.crs(src, target30)) {
    stop("Source must be a single-band EPSG:3742 DEM.")
  }
  if (any(abs(terra::res(src) - 8.49192367241446) > 1e-6)) {
    stop("Source resolution differs from the inspected ~8.49 m DEM.")
  }

  # A fresh, dedicated output directory is REQUIRED; no existing directory
  # can be reused. All rasters, logs and GRASS maps are confined to this run.
  if (is.null(output_dir)) {
    output_dir <- file.path(project_dir, "benchmark-results",
      paste0("solar_resolution_", format(Sys.time(), "%Y%m%d_%H%M%S"),
             "_", Sys.getpid()))
  }
  if (file.exists(output_dir)) stop("Output path already exists: ", output_dir)
  if (!dir.create(output_dir, recursive = TRUE)) stop("Cannot create output directory.")
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  for (sub in c("dem", "terrain", "solar_native", "solar_30m", "core",
                "differences", "logs", "grassdb", "grass_home", "terra_tmp")) {
    dir.create(file.path(output_dir, sub))
  }
  out <- function(sub, name) file.path(output_dir, sub, name)
  raster_options <- list(datatype = "FLT4S", gdal = c("COMPRESS=LZW", "TILED=YES"))
  save_raster <- function(x, path) {
    terra::writeRaster(x, path, overwrite = FALSE,
                       datatype = raster_options$datatype, gdal = raster_options$gdal)
  }
  old_options <- options(digits = 17)
  old_terra <- terra::terraOptions(print = FALSE)
  on.exit(options(old_options), add = TRUE)
  on.exit(terra::terraOptions(tempdir = old_terra$tempdir), add = TRUE)
  terra::terraOptions(tempdir = out("terra_tmp", ""))

  # Record elapsed wall time, including separate GRASS initialization,
  # preparation, terrain derivatives, daily model, exports and comparisons.
  timings <- data.frame()
  timed <- function(branch, stage, expr, day = NA_integer_) {
    start <- proc.time()[["elapsed"]]
    started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
    ok <- FALSE
    on.exit({
      timings <<- rbind(timings, data.frame(branch = branch, stage = stage,
        day = day, started = started, elapsed_seconds = proc.time()[["elapsed"]] - start,
        success = ok, nprocs = nprocs))
      utils::write.csv(timings, out("logs", "runtimes.csv"), row.names = FALSE)
    })
    value <- force(expr)
    ok <- TRUE
    value
  }
  grids <- data.frame()
  record_grid <- function(x, label) {
    e <- as.vector(terra::ext(x)); r <- terra::res(x); o <- terra::origin(x)
    grids <<- rbind(grids, data.frame(label = label, xmin = e[1], xmax = e[2],
      ymin = e[3], ymax = e[4], xres = r[1], yres = r[2],
      xorigin = o[1], yorigin = o[2], nrow = nrow(x), ncol = ncol(x),
      ncell = terra::ncell(x)))
    utils::write.csv(grids, out("logs", "grid_geometry.csv"), row.names = FALSE)
  }
  same_grid <- function(x, y) {
    if (!terra::same.crs(x, y) || nrow(x) != nrow(y) || ncol(x) != ncol(y) ||
        any(abs(as.vector(terra::ext(x)) - as.vector(terra::ext(y))) > 1e-6) ||
        any(abs(terra::res(x) - terra::res(y)) > 1e-8)) {
      stop("Grid mismatch: inspect logs/grid_geometry.csv; do not silently resample.")
    }
    invisible(TRUE)
  }
  full_valid <- function(x, label) {
    if (any(!is.finite(terra::values(x, mat = FALSE)))) {
      stop(label, " has missing/nonfinite elevations. Review the site/mask before modeling.")
    }
  }
  record_grid(target30, "target30")
  record_grid(core30, "core30")
  writeLines(c(
    "Potential clear-sky daily irradiation on DEM-derived terrain surfaces.",
    paste("Input:", source_file), paste("terra:", utils::packageVersion("terra")),
    paste("rgrass:", utils::packageVersion("rgrass")),
    "EPSG:3742; horizontal and assumed elevation units: meters; zscale=1.",
    "Core: xmin=613800 xmax=616200 ymin=4858800 ymax=4861200.",
    "Domain: xmin=610800 xmax=619200 ymin=4855800 ymax=4864200.",
    "Fine crop snaps outward to its original grid (less than one extra cell per edge).",
    "3 km terrain buffer is provisional: NO horizon/buffer convergence test is performed.",
    "Days=1,166; step=0.5 hours; distance_step=1; Linke=3; albedo=0.2.",
    paste("GRASS threads:", nprocs),
    "Slope/aspect: r.slope.aspect, degrees, FCELL, default GRASS aspect, no edge flag.",
    "Shadowing retained; no precomputed horizon maps; no cloud corrections.",
    "Fine-to-30m means use map-footprint overlap areas, NOT terrain surface-area weights.",
    "Common core mask requires finite results in EVERY branch/day/output and full fine coverage.",
    "One timing per operation; no warm-cache repetitions; no measured peak RAM.",
    "Fine outputs are a comparator, not ground truth. Outputs are not seasonal totals."
  ), file.path(output_dir, "RUN_NOTES.txt"))
  on.exit(writeLines(capture.output(utils::sessionInfo()),
                    out("logs", "sessionInfo.txt")), add = TRUE)

  # Keep fine pixels unchanged. A separate 60 m source apron supports
  # interpolation at the 30 m domain boundary; it is not an analysis buffer.
  fine <- timed("fine", "crop_write_dem", {
    z <- terra::crop(src, domain_extent, snap = "out")
    full_valid(z, "Fine benchmark domain")
    save_raster(z, out("dem", "fine.tif"))
  })
  apron <- terra::ext(610740, 619260, 4855740, 4864260)
  source30 <- timed("shared", "read_resampling_apron", {
    z <- terra::crop(src, apron, snap = "out")
    full_valid(z, "Resampling source apron")
    z
  })
  dems <- list(fine = fine)
  for (branch in c("bilinear30", "mean30")) {
    method <- if (branch == "bilinear30") "bilinear" else "average"
    # terra uses GDAL warping here. "average" weights contributing source
    # cells by overlap area, including partial cells at noninteger ratios.
    # This is NOT aggregate(fact=...) and NOT an unweighted focal mean.
    dems[[branch]] <- timed(branch, "resample_write_dem", {
      z <- terra::resample(source30, target30, method = method, threads = FALSE,
        filename = out("dem", paste0(branch, ".tif")), overwrite = FALSE,
        datatype = raster_options$datatype, gdal = raster_options$gdal)
      same_grid(z, target30)
      full_valid(z, branch)
      z
    })
  }
  same_grid(dems$bilinear30, dems$mean30)
  for (branch in names(dems)) record_grid(dems[[branch]], paste0("dem_", branch))

  # Environment setup follows the existing OSGeo4W installation, but uses a
  # new GRASS database/location. Restore environment variables upon return.
  env_names <- c("OSGEO4W_ROOT", "GISBASE", "GRASS_PYTHON", "PYTHONHOME",
                 "GDAL_DATA", "PROJ_LIB", "PATH", "GISRC", "GIS_LOCK",
                 "GRASS_REGION", "WIND_OVERRIDE", "OMP_NUM_THREADS")
  old_env <- Sys.getenv(env_names, unset = NA_character_)
  on.exit({
    for (key in env_names) {
      if (is.na(old_env[[key]])) Sys.unsetenv(key)
      else do.call(Sys.setenv, setNames(list(old_env[[key]]), key))
    }
  }, add = TRUE)
  Sys.unsetenv(c("GRASS_REGION", "WIND_OVERRIDE"))
  Sys.setenv(OSGEO4W_ROOT = osgeo4w_root, GISBASE = grass_dir,
    GRASS_PYTHON = file.path(osgeo4w_root, "bin/python3.exe"),
    PYTHONHOME = python_home, GDAL_DATA = file.path(osgeo4w_root, "apps/gdal/share/gdal"),
    PROJ_LIB = file.path(osgeo4w_root, "share/proj"), OMP_NUM_THREADS = nprocs)
  Sys.setenv(PATH = paste(c(file.path(osgeo4w_root, "bin"),
    file.path(grass_dir, c("bin", "scripts", "lib")), python_home,
    file.path(python_home, "Scripts"), old_env[["PATH"]]), collapse = .Platform$path.sep))
  timed("shared", "initialize_grass", rgrass::initGRASS(
    gisBase = grass_dir, home = out("grass_home", ""),
    gisDbase = out("grassdb", ""), location = "solar_benchmark",
    mapset = "PERMANENT", SG = target30, override = TRUE))
  grass <- function(module, parameters = list(), flags = character(), intern = FALSE) {
    cat(paste(module, paste(flags, collapse = ","),
      paste(names(parameters), unlist(parameters), sep = "=", collapse = " ")),
      "\n", file = out("logs", "grass_commands.txt"), append = TRUE)
    ans <- rgrass::execGRASS(module, parameters = parameters,
                            flags = if (length(flags)) flags else NULL, intern = intern)
    status <- attr(ans, "status")
    if (is.null(status) && is.numeric(ans) && length(ans) == 1L) status <- ans
    if (!is.null(status) && status != 0) stop(module, " failed with status ", status)
    ans
  }
  # Synchronize region projection metadata with the EPSG:3742 project CRS.
  timed("shared", "synchronize_grass_crs", grass("g.proj",
    flags = "c", parameters = list(epsg = 3742)))
  writeLines(grass("g.version", flags = "g", intern = TRUE),
             out("logs", "grass_version.txt"))
  cores <- list()
  coverages <- list()
  slopes <- list()
  aspects <- list()
  for (branch in names(dems)) {
    dem_name <- paste0("dem_", branch)
    slope_name <- paste0("slope_", branch)
    aspect_name <- paste0("aspect_", branch)
    timed(branch, "import_set_region", {
      # Do not override projection checks. Fix region from the imported map,
      # rather than relying on initGRASS's formatted initial coordinates.
      grass("r.in.gdal", parameters = list(input = out("dem", paste0(branch, ".tif")),
                                            output = dem_name))
      grass("g.region", parameters = list(raster = dem_name))
      writeLines(grass("g.region", flags = "g", intern = TRUE),
                 out("logs", paste0("region_", branch, ".txt")))
    })
    timed(branch, "derive_slope_aspect", grass("r.slope.aspect", parameters = list(
      elevation = dem_name, slope = slope_name, aspect = aspect_name,
      format = "degrees", precision = "FCELL", zscale = 1, nprocs = nprocs)))
    for (quantity in c("slope", "aspect")) {
      path <- out("terrain", paste0(branch, "_", quantity, ".tif"))
      timed(branch, paste0("export_", quantity), grass("r.out.gdal", parameters = list(
        input = paste0(quantity, "_", branch), output = path,
        format = "GTiff", type = "Float32")))
      r <- terra::rast(path)
      record_grid(r, paste(branch, quantity, sep = "_"))
      same_grid(r, dems[[branch]])
      if (quantity == "slope") slopes[[branch]] <- r else aspects[[branch]] <- r
    }
    for (day in unname(days)) {
      prefix <- paste0(branch, "_d", sprintf("%03d", day))
      maps <- setNames(paste0(prefix, "_", kinds), kinds)
      timed(branch, "r_sun", grass("r.sun", parameters = list(
        elevation = dem_name, slope = slope_name, aspect = aspect_name,
        day = day, step = 0.5, distance_step = 1,
        linke_value = 3, albedo_value = 0.2, nprocs = nprocs,
        glob_rad = maps[["global"]], beam_rad = maps[["beam"]],
        insol_time = maps[["sunhours"]])), day = day)
      # No -p flag: terrain shadows remain ON. Slope/aspect are explicit.
      for (kind in kinds) {
        key <- paste(prefix, kind, sep = "_")
        native_path <- out("solar_native", paste0(key, ".tif"))
        timed(branch, paste0("export_", kind), grass("r.out.gdal", parameters = list(
          input = maps[[kind]], output = native_path, format = "GTiff",
          type = "Float32")), day = day)
        r <- terra::rast(native_path)
        record_grid(r, key)
        same_grid(r, dems[[branch]])
        timed(branch, paste0("transfer_core_", kind), {
          if (branch == "fine") {
            fine_extent <- as.vector(terra::ext(r))
            target_extent <- as.vector(terra::ext(target30))
            if (fine_extent[1] > target_extent[1] ||
                fine_extent[2] < target_extent[2] ||
                fine_extent[3] > target_extent[3] ||
                fine_extent[4] < target_extent[4]) {
              stop("Fine radiation raster extent does not fully contain the target 30 m grid: ",
                   key, ". Averaging and valid-coverage calculation cannot proceed.")
            }
            # Average native radiation directly onto the EXACT common grid.
            on30 <- terra::resample(r, target30, method = "average", threads = FALSE,
              filename = out("solar_30m", paste0(key, ".tif")),
              overwrite = FALSE, datatype = raster_options$datatype,
              gdal = raster_options$gdal)
            # Zeros for invalid pixels must remain valid during this average:
            # otherwise GDAL would renormalize and hide incomplete coverage.
            coverage <- terra::resample(terra::ifel(is.na(r), 0, 1), target30,
                                        method = "average", threads = FALSE)
            coverages[[key]] <- terra::crop(coverage, core_extent, snap = "near")
          } else {
            on30 <- r
          }
          same_grid(on30, target30)
          cores[[key]] <- terra::crop(on30, core_extent, snap = "near")
          same_grid(cores[[key]], core30)
        }, day = day)
      }
    }
  }

  timed("shared", "common_mask_and_diagnostics", {
    # One shared evaluation set across all branches, both days and all kinds.
    v <- lapply(cores, terra::values, mat = FALSE)
    common <- Reduce(`&`, lapply(v, is.finite))
    for (key in names(coverages)) {
      cv <- terra::values(coverages[[key]], mat = FALSE)
      common <- common & is.finite(cv) & cv >= min_coverage
      save_raster(coverages[[key]], out("core", paste0(key, "_valid_fraction.tif")))
    }
    if (!any(common)) stop("No common valid evaluation cells.")
    mask <- terra::setValues(terra::rast(core30), ifelse(common, 1, NA_real_))
    save_raster(mask, out("core", "common_valid_mask.tif"))
    utils::write.csv(data.frame(total_core_cells = length(common),
      common_cells = sum(common), excluded_cells = sum(!common),
      common_fraction = mean(common)), out("logs", "coverage_summary.csv"), row.names = FALSE)
    for (key in names(cores)) {
      vals <- v[[key]]; vals[!common] <- NA_real_
      cores[[key]] <- terra::setValues(terra::rast(core30), vals)
      save_raster(cores[[key]], out("core", paste0(key, ".tif")))
    }
    # Use ONE terrain classification for all pairwise comparisons.
    # Do not average aspect angles. Classification uses mean30's own terrain.
    sl <- terra::values(terra::crop(slopes$mean30, core_extent), mat = FALSE)
    asp <- terra::values(terra::crop(aspects$mean30, core_extent), mat = FALSE)
    slope_class <- as.character(cut(sl, c(0, 5, 15, 30, Inf), right = FALSE,
                                    labels = c("0-5", "5-15", "15-30", "30+")))
    bearing <- (450 - asp) %% 360
    aspect_class <- c("N", "E", "S", "W")[floor(((bearing + 45) %% 360) / 90) + 1]
    aspect_class[is.finite(sl) & sl < 2] <- "flat_lt2deg"
    coordinates <- terra::xyFromCell(core30, seq_len(terra::ncell(core30)))
    cell_table <- data.frame(x = coordinates[, 1], y = coordinates[, 2],
      common = common, slope_mean30 = sl, aspect_grass_mean30 = asp,
      slope_class = slope_class, aspect_class = aspect_class)
    for (key in names(cores)) cell_table[[key]] <- terra::values(cores[[key]], mat = FALSE)
    utils::write.csv(cell_table, file.path(output_dir, "core_cell_values.csv"), row.names = FALSE)

    pairs <- list(c("fine", "bilinear30"), c("fine", "mean30"), c("mean30", "bilinear30"))
    summary_rows <- list()
    differences <- list()
    for (day in unname(days)) for (kind in kinds) for (pair in pairs) {
      key <- function(branch) paste0(branch, "_d", sprintf("%03d", day), "_", kind)
      reference <- terra::values(cores[[key(pair[1])]], mat = FALSE)
      candidate <- terra::values(cores[[key(pair[2])]], mat = FALSE)
      delta <- candidate - reference
      label <- paste0(pair[2], "_minus_", pair[1], "_d", sprintf("%03d", day), "_", kind)
      differences[[label]] <- terra::setValues(terra::rast(core30), delta)
      save_raster(differences[[label]], out("differences", paste0(label, ".tif")))
      groups <- list(all = rep(TRUE, length(common)))
      for (g in unique(stats::na.omit(slope_class))) groups[[paste0("slope_", g)]] <- slope_class == g
      for (g in unique(stats::na.omit(aspect_class))) groups[[paste0("aspect_", g)]] <- aspect_class == g
      for (group in names(groups)) {
        keep <- common & !is.na(groups[[group]]) & groups[[group]]
        if (!any(keep)) next
        a <- reference[keep]; b <- candidate[keep]; d <- b - a
        threshold <- if (kind == "sunhours") 1 else 100
        relative <- a >= threshold
        can_cor <- length(a) > 1L && stats::sd(a) > 0 && stats::sd(b) > 0
        summary_rows[[length(summary_rows) + 1L]] <- data.frame(
          reference = pair[1], candidate = pair[2], day = day, quantity = kind,
          units = if (kind == "sunhours") "hours/day" else "Wh/m2/day",
          group = group, n = length(a), reference_mean = mean(a), candidate_mean = mean(b),
          bias_candidate_minus_reference = mean(d), MAE = mean(abs(d)),
          RMSE = sqrt(mean(d^2)), median_abs_difference = stats::median(abs(d)),
          p95_abs_difference = unname(stats::quantile(abs(d), 0.95)),
          pearson = if (can_cor) stats::cor(a, b) else NA_real_,
          spearman = if (can_cor) stats::cor(a, b, method = "spearman") else NA_real_,
          relative_reference_cutoff = threshold, n_relative = sum(relative),
          median_abs_percent_difference = if (any(relative))
            stats::median(100 * abs(d[relative]) / a[relative]) else NA_real_)
      }
    }
    utils::write.csv(do.call(rbind, summary_rows),
                     file.path(output_dir, "comparison_statistics.csv"), row.names = FALSE)
    make_plots <- function() {
      grDevices::pdf(file.path(output_dir, "global_radiation_comparison.pdf"), width = 12, height = 8)
      on.exit(grDevices::dev.off())
      for (day in unname(days)) {
        graphics::par(mfrow = c(2, 3), mar = c(3, 3, 3, 4))
        keys <- paste0(names(dems), "_d", sprintf("%03d", day), "_global")
        zlim <- range(unlist(lapply(cores[keys], terra::values)), na.rm = TRUE)
        for (key in keys) terra::plot(cores[[key]], range = zlim, main = key,
                                      col = grDevices::hcl.colors(50, "YlOrRd"))
        labels <- vapply(pairs, function(p) paste0(p[2], "_minus_", p[1],
          "_d", sprintf("%03d", day), "_global"), character(1))
        limit <- max(abs(unlist(lapply(differences[labels], terra::values))), na.rm = TRUE)
        if (limit == 0) limit <- 1
        for (label in labels) terra::plot(differences[[label]], range = c(-limit, limit),
          main = label, col = grDevices::hcl.colors(51, "Blue-Red 3"))
      }
    }
    make_plots()
  })
  message("Benchmark complete. Outputs: ", output_dir)
  invisible(output_dir)
}
