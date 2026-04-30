# ------------------------------------------------------------
# R script: Reproject GPW rasters to EPSG:9377 and
#           Mosaic + Reproject WorldPop yearly tiles to EPSG:9377
# Notes:
# - Uses terra for raster IO and reprojection.
# - WorldPop: groups tiles by year via regex, mosaics per year with sprc+mosaic.
# - GPW: reprojects each file and appends _9377 suffix.
# - Reprojection method set to "bilinear" (continuous surfaces).
#   Use "near" if you need to preserve integer pixel values strictly.
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(terra)
  library(stringr)
  library(tools)
})

# ----------------------------
# 0) Paths and CRS
# ----------------------------
dir_gpw      <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/02_population/output/gpw"
dir_worldpop <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/02_population/output/worldpop"

crs_target    <- "EPSG:9377"
reproj_method <- "bilinear"  # change to "near" if you prefer nearest-neighbor

if (!dir.exists(dir_gpw)) dir.create(dir_gpw, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_worldpop)) dir.create(dir_worldpop, recursive = TRUE, showWarnings = FALSE)

# Optional: set a fast temp directory for big raster ops
# terraOptions(tempdir = "C:/temp_terra")

# Optional: write options (compression)
write_opts <- c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES")

# ----------------------------
# 1) GPW: reproject each raster and save with _9377 suffix
# ----------------------------
gpw_files <- list.files(dir_gpw, pattern = "\\.(tif|tiff)$", full.names = TRUE, ignore.case = TRUE)
cat("GPW: found", length(gpw_files), "rasters.\n")

for (f in gpw_files) {
  cat("Reprojecting GPW ->", basename(f), "\n")
  r <- rast(f)
  out_name <- file.path(dirname(f), paste0(file_path_sans_ext(basename(f)), "_9377.tif"))
  
  # If already projected, terra will still project (safe). Overwrite explicitly.
  r9377 <- project(r, crs_target, method = reproj_method)
  writeRaster(r9377, out_name, overwrite = TRUE, gdal = write_opts)
  cat("  > Saved:", basename(out_name), "\n")
}


# ----------------------------
# 2) WorldPop: mosaic tiles by year, then reproject to 9377
#    Example file names:
#    WorldPop_Population_2017-0000000000-0000000000.tif
#    WorldPop_Population_2017-0000000000-0000032768.tif
# ----------------------------

wp_files <- list.files(dir_worldpop, pattern = "\\.(tif|tiff)$", full.names = TRUE, ignore.case = TRUE)
cat("WorldPop: found", length(wp_files), "tiles.\n")

if (length(wp_files) > 0) {
  # Extract year from filename (robust to "_" or "-" separators)
  get_year <- function(x) {
    m <- stringr::str_match(basename(x), "WorldPop[_-]Population[_-](\\d{4})")
    if (is.na(m[1, 2])) NA_integer_ else as.integer(m[1, 2])
  }
  
  years_vec <- vapply(wp_files, get_year, integer(1))
  tbl <- data.frame(file = wp_files, year = years_vec, stringsAsFactors = FALSE)
  tbl <- subset(tbl, !is.na(year))
  
  yrs <- sort(unique(tbl$year))
  cat("WorldPop: years detected ->", paste(yrs, collapse = ", "), "\n")
  
  for (yy in yrs) {
    cat("Processing WorldPop year:", yy, "\n")
    files_y <- tbl$file[tbl$year == yy]
    if (length(files_y) == 0) next
    
    # Read tiles as SpatRaster list
    rs <- lapply(files_y, function(p) {
      cat("  - reading:", basename(p), "\n")
      terra::rast(p)
    })
    
    # Mosaic tiles WITHOUT custom fun (handles non-overlapping tiles well)
    # 'merge' uses the first non-NA across rasters, ideal for tiling patterns
    r_mosaic <- do.call(terra::merge, rs)
    
    # Reproject to EPSG:9377
    r_mosaic_9377 <- terra::project(r_mosaic, crs_target, method = reproj_method)
    
    # Output name (save in same folder)
    out_name <- file.path(dir_worldpop, sprintf("worldpop_population_%d_9377.tif", yy))
    if (file.exists(out_name)) {
      cat("  > Exists, overwriting:", basename(out_name), "\n")
    }
    
    terra::writeRaster(r_mosaic_9377, out_name, overwrite = TRUE, gdal = write_opts)
    cat("  > Saved:", basename(out_name), "\n")
  }
} else {
  cat("WorldPop: no tiles found. Nothing to process.\n")
}
