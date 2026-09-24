# Standalone synthetic-domain timestep benchmark; no production files modified.
.libPaths(c("C:/Users/Kristin/AppData/Local/R/win-library/4.6", .libPaths()))
library(terra)
library(rgrass)
run <- function() {
  old <- "benchmark-results/solar_resolution_20260923_173102_16448"
  out <- file.path("benchmark-results", paste0("solar_timestep_",format(Sys.time(),"%Y%m%d_%H%M%S")))
  stopifnot(!file.exists(out), !nzchar(Sys.getenv("GISRC")))
  dir.create(out,recursive=TRUE)
  out <- normalizePath(out,winslash="/")
  dir.create(file.path(out,"grassdb"));dir.create(file.path(out,"grass_home"))
  dem <- rast(file.path(old,"dem/mean30.tif"))
  stopifnot(all(res(dem)==30),nrow(dem)==280,ncol(dem)==280,
    all(as.vector(ext(dem))==c(610800,619200,4855800,4864200)))
  root <- "C:/Users/Kristin/AppData/Local/Programs/OSGeo4W"
  grass_dir <- file.path(root,"apps/grass/grass85")
  python_home <- file.path(root,"apps/Python312")
  Sys.unsetenv(c("GRASS_REGION","WIND_OVERRIDE"))
  Sys.setenv(OSGEO4W_ROOT=root,GISBASE=grass_dir,
    GRASS_PYTHON=file.path(root,"bin/python3.exe"),PYTHONHOME=python_home,
    GDAL_DATA=file.path(root,"apps/gdal/share/gdal"),
    PROJ_LIB=file.path(root,"share/proj"),OMP_NUM_THREADS=1)
  Sys.setenv(PATH=paste(c(file.path(root,"bin"),file.path(grass_dir,c("bin","scripts","lib")),
    python_home,file.path(python_home,"Scripts"),Sys.getenv("PATH")),collapse=.Platform$path.sep))
  initGRASS(gisBase=grass_dir,home=file.path(out,"grass_home"),gisDbase=file.path(out,"grassdb"),
    location="step_test",mapset="PERMANENT",SG=dem,override=TRUE)
  grass <- function(module,p=list(),flags=NULL,intern=FALSE) {
    x <- execGRASS(module,parameters=p,flags=flags,intern=intern)
    status <- attr(x,"status")
    if(is.null(status)&&is.numeric(x)&&length(x)==1) status <- x
    if(!is.null(status)&&status!=0) stop(module," failed")
    x
  }
  grass("g.proj",list(epsg=3742),flags="c")
  inputs <- c(dem=file.path(old,"dem/mean30.tif"),slope=file.path(old,"terrain/mean30_slope.tif"),
              aspect=file.path(old,"terrain/mean30_aspect.tif"))
  for(n in names(inputs)) {
    stopifnot(compareGeom(rast(inputs[[n]]),dem))
    grass("r.in.gdal",list(input=normalizePath(inputs[[n]],winslash="/"),output=n))
  }
  grass("g.region",list(raster="dem"))
  writeLines(grass("g.region",flags="g",intern=TRUE),file.path(out,"region.txt"))
  writeLines(c(capture.output(sessionInfo()),grass("g.version",flags="g",intern=TRUE)),
    file.path(out,"versions.txt"))
  writeLines(c(inputs,"domain=610800,619200,4855800,4864200",
    "core=613800,616200,4858800,4861200",
    "Both steps: nprocs=1, Linke=3, albedo=0.2, distance_step=1, solar_constant=1367; terrain shadows ON.",
    "Imported original benchmark FCELL slope/aspect; global, beam and sunhours computed as in original benchmark.",
    "Relative differences exclude comparator <=0; no scientific thresholds applied."),
    file.path(out,"settings.txt"))
  stats <- list(); times <- list()
  for(day in c(1L,166L)) {
    surfaces <- list()
    for(step in c(1,0.5)) {
      key <- paste0("d",day,"_s",if(step==1)"1" else "05")
      elapsed <- system.time(grass("r.sun",list(elevation="dem",slope="slope",aspect="aspect",
        glob_rad=key,beam_rad=paste0(key,"_beam"),insol_time=paste0(key,"_hours"),
        day=day,step=step,distance_step=1,linke_value=3,albedo_value=0.2,
        solar_constant=1367,nprocs=1)))[["elapsed"]]
      times[[length(times)+1L]] <- data.frame(day=day,step_hours=step,elapsed_seconds=elapsed)
      path <- file.path(out,paste0(key,"_global.tif"))
      grass("r.out.gdal",list(input=key,output=path,format="GTiff",type="Float32",
        createopt=c("COMPRESS=LZW","TILED=YES")),flags="c")
      z <- rast(path);stopifnot(compareGeom(z,dem))
      surfaces[[as.character(step)]] <- crop(z,ext(613800,616200,4858800,4861200))
    }
    a <- values(surfaces[["1"]],mat=FALSE);b <- values(surfaces[["0.5"]],mat=FALSE)
    ok <- is.finite(a)&is.finite(b);stopifnot(sum(ok)==6400)
    delta <- a[ok]-b[ok];ad <- abs(delta);positive <- b[ok]>0
    stats[[length(stats)+1L]] <- data.frame(day=day,n=sum(ok),relative_n=sum(positive),
      mean_signed_Wh_m2_day=mean(delta),mae_Wh_m2_day=mean(ad),
      median_absolute_relative_percent=100*median(ad[positive]/abs(b[ok][positive])),
      p95_absolute_Wh_m2_day=unname(quantile(ad,.95)),
      max_absolute_Wh_m2_day=max(ad))
    writeRaster(surfaces[["1"]]-surfaces[["0.5"]],
      file.path(out,paste0("d",day,"_core_difference_1_minus_05.tif")),overwrite=FALSE)
  }
  summary <- do.call(rbind,stats);runtime <- do.call(rbind,times)
  write.csv(summary,file.path(out,"comparison.csv"),row.names=FALSE)
  write.csv(runtime,file.path(out,"runtimes.csv"),row.names=FALSE)
  print(summary,digits=10);print(runtime);cat("OUTPUT:",out,"\n")
}
run()
