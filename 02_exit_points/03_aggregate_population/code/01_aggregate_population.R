# ------------------------------------------------------------
# R script: 01_aggregate_population - Sum population per Thiessen polygon (per year)
# - Inputs: Thiessen polygons per year (EPSG:9377) and yearly population rasters (GPW, WorldPop)
# - Output: One shapefile per year with added columns pop_gpw and pop_worldpop
# - Extraction: terra::extract with exact=TRUE (area-weighted cell fractions)
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(tools)
})

# ----------------------------
# 0) Paths and constants
# ----------------------------
dir_thiessen_in <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/01_seizures/output"

dir_gpw   <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/02_population/output/gpw"
dir_wp    <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/02_population/output/worldpop"

dir_out   <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/03_aggregate_population/output"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

crs_target_epsg <- 9377
years <- 2017:2022

# Optional: write options for GeoTIFF (not used here, but kept for reference)
# write_opts <- c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES")

# ----------------------------
# Helpers
# ----------------------------

# Cleanly overwrite an ESRI Shapefile set
remove_existing_shp <- function(path_with_ext) {
  base_no_ext <- tools::file_path_sans_ext(path_with_ext)
  exts <- c(".shp",".shx",".dbf",".prj",".cpg",".qmd",".qix",".sbn",".sbx")
  files <- paste0(base_no_ext, exts)
  files <- files[file.exists(files)]
  if (length(files)) invisible(file.remove(files))
}

# Force 2D (drop Z/M)
force_2d <- function(x) sf::st_zm(x, drop = TRUE, what = "ZM")

# Safe read of a raster by full path; returns SpatRaster or stops
read_rast_safe <- function(p) {
  if (!file.exists(p)) stop(sprintf("Raster not found: %s", p))
  terra::rast(p)
}

# Sum raster values inside polygons with area-weighted fractions
sum_raster_by_polygons <- function(r, sfx) {
  # terra::extract expects same CRS; reproject polygons if needed
  if (!is.na(terra::crs(r))) {
    sfx <- if (!is.na(sf::st_crs(sfx)) && sf::st_crs(sfx)$epsg != terra::crs(r, proj=TRUE)) {
      sf::st_transform(sfx, terra::crs(r, proj=TRUE))
    } else sfx
  }
  # exact=TRUE uses partial cell coverage weights
  vals <- terra::extract(r, terra::vect(sfx), fun = sum, na.rm = TRUE, exact = FALSE)
  # terra::extract returns a data.frame with first column 'ID' and second the summary
  vals[[2]]
}

# ----------------------------
# 1) Process per year
# ----------------------------
for (yy in years) {
  message(sprintf("Processing year: %d", yy))
  
  # 1.1 Read Thiessen polygons (input)
  thi_path_in <- file.path(dir_thiessen_in, sprintf("thiessen_%d.shp", yy))
  if (!file.exists(thi_path_in)) {
    warning(sprintf("Thiessen file not found, skipping: %s", thi_path_in))
    next
  }
  thi <- sf::st_read(thi_path_in, quiet = TRUE)
  if (is.na(sf::st_crs(thi))) stop("Thiessen has no CRS defined.")
  if (sf::st_crs(thi)$epsg != crs_target_epsg) thi <- sf::st_transform(thi, crs_target_epsg)
  thi <- force_2d(thi)
  
  # 1.2 Read rasters for the same year
  gpw_path <- file.path(dir_gpw, sprintf("GPW_Population_%d_9377.tif", yy))
  wp_path  <- file.path(dir_wp,  sprintf("worldpop_population_%d_9377.tif", yy))
  
  gpw_r <- read_rast_safe(gpw_path)
  wp_r  <- read_rast_safe(wp_path)
  
  # 1.3 Ensure rasters have a defined CRS and match target
  if (is.na(terra::crs(gpw_r))) stop(sprintf("Raster CRS undefined: %s", gpw_path))
  if (is.na(terra::crs(wp_r)))  stop(sprintf("Raster CRS undefined: %s", wp_path))
  
  # 1.4 Aggregate (sum) population per polygon (area-weighted)
  pop_gpw <- sum_raster_by_polygons(gpw_r, thi)
  pop_wp  <- sum_raster_by_polygons(wp_r,  thi)
  
  # 1.5 Attach results to polygons
  thi_out <- thi %>%
    mutate(
      pop_gpw      = as.numeric(pop_gpw),
      pop_worldpop = as.numeric(pop_wp)
    )
  
  # 1.6 Save output shapefile
  out_path <- file.path(dir_out, sprintf("thiessen_%d_pop.shp", yy))
  remove_existing_shp(out_path)
  sf::st_write(thi_out, out_path, driver = "ESRI Shapefile", quiet = TRUE)
  
  message(sprintf("  > Saved: %s (features: %d)", basename(out_path), nrow(thi_out)))
}

message("Done. Thiessen polygons with GPW and WorldPop population saved in 03_aggregate_population/output.")
