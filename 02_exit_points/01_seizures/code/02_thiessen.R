# ------------------------------------------------------------
# R script: 02_thiessen - Voronoi (Thiessen) polygons per year
# - Replicates your working flow
# - Uses a user-provided extent polygon to bound cells
# - Preserves ID_SEIZURE, cultivo, year
# - Keeps everything in EPSG:9377
# - Ensures 2D geometries to avoid POLYGONZ shapefile errors
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  # lwgeom is optional but helps with st_make_valid if needed
  suppressWarnings(require(lwgeom, quietly = TRUE, warn.conflicts = FALSE))
})

# =========================
# 0) Base paths and CRS
# =========================
base_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/01_seizures"
in_dir   <- file.path(base_dir, "output")   # where seiz_YYYY.shp live
out_dir  <- in_dir

# Extent polygon (limit area)
extent_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/00_area/extent_area.shp"

# Target CRS (national unique origin)
crs_target_epsg <- 9377

# Years to process
years <- 2017:2022

# =========================
# Helpers
# =========================

# Cleanly overwrite a shapefile set
remove_existing_shp <- function(path_with_ext) {
  base_no_ext <- tools::file_path_sans_ext(path_with_ext)
  exts <- c(".shp",".shx",".dbf",".prj",".cpg",".qmd",".qix",".sbn",".sbx")
  files <- paste0(base_no_ext, exts)
  files <- files[file.exists(files)]
  if (length(files)) invisible(file.remove(files))
}

# Force 2D (drop Z/M) safely
force_2d <- function(x) {
  x %>% st_zm(drop = TRUE, what = "ZM")
}

# Make geometries valid if lwgeom is available; otherwise use buffer(0) fallback
make_valid_safe <- function(x) {
  if ("lwgeom" %in% .packages()) {
    st_make_valid(x)
  } else {
    suppressWarnings(st_buffer(x, 0))
  }
}

# =========================
# 1) Read and prepare extent (2D in EPSG:9377)
# =========================
extent <- st_read(extent_path, quiet = TRUE)
if (is.na(st_crs(extent))) stop("Extent polygon has no CRS defined. Please define it before running.")
if (st_crs(extent)$epsg != crs_target_epsg) {
  extent <- st_transform(extent, crs_target_epsg)
}
extent <- force_2d(extent)
extent_union <- st_union(extent)
# Numeric-stable envelope for st_voronoi
envelope <- st_as_sfc(st_bbox(extent_union))

# =========================
# 2) Build Thiessen per year
# =========================
for (anio in years) {
  cat("Processing year:", anio, "\n")
  
  in_pts <- file.path(in_dir, sprintf("seiz_%d.shp", anio))
  if (!file.exists(in_pts)) {
    cat("  > Skipped, not found:", basename(in_pts), "\n")
    next
  }
  
  # Read and ensure CRS 9377 and 2D points
  pts <- st_read(in_pts, quiet = TRUE)
  if (is.na(st_crs(pts))) stop("Input points have no CRS.")
  if (st_crs(pts)$epsg != crs_target_epsg) {
    pts <- st_transform(pts, crs_target_epsg)
  }
  pts <- force_2d(pts)
  pts <- st_cast(pts, "POINT", warn = FALSE)
  
  # Keep only required fields
  req <- c("ID_SEIZURE","cultivo","year")
  miss <- setdiff(req, names(pts))
  if (length(miss)) stop(paste("Missing required fields:", paste(miss, collapse=", ")))
  pts <- pts %>% select(all_of(req), geometry)
  
  # Build MULTIPOINT from geometry only
  mp <- st_union(st_geometry(pts))
  mp <- st_cast(mp, "MULTIPOINT", warn = FALSE)
  
  # Voronoi over the envelope, then clip to exact extent polygon
  v <- st_voronoi(mp, envelope = envelope)
  vor_sfc <- st_collection_extract(st_sfc(v, crs = st_crs(pts)), "POLYGON")
  
  # Intersect with extent; ensure valid 2D polygons
  vor_clip <- st_intersection(st_sf(geometry = vor_sfc), st_sf(geometry = extent_union))
  if (nrow(vor_clip) == 0) {
    cat("  > No Voronoi polygons after clipping. Skipping year:", anio, "\n")
    next
  }
  
  vor_clip <- make_valid_safe(vor_clip)
  vor_clip <- force_2d(vor_clip)
  # Homogenize type to MULTIPOLYGON for shapefile robustness
  vor_clip <- st_cast(vor_clip, "MULTIPOLYGON", warn = FALSE)
  
  # Attribute transfer: nearest point (1-to-1)
  nn <- st_nearest_feature(vor_clip, pts)
  thiessen <- st_sf(
    ID_SEIZURE = pts$ID_SEIZURE[nn],
    cultivo    = pts$cultivo[nn],
    year       = pts$year[nn],
    geometry   = st_geometry(vor_clip),
    crs        = st_crs(pts)
  )
  
  # Final safety: 2D and MULTIPOLYGON
  thiessen <- make_valid_safe(thiessen)
  thiessen <- force_2d(thiessen)
  thiessen <- st_cast(thiessen, "MULTIPOLYGON", warn = FALSE)
  
  # Export
  out_path <- file.path(out_dir, sprintf("thiessen_%d.shp", anio))
  remove_existing_shp(out_path)
  st_write(thiessen, out_path, driver = "ESRI Shapefile", quiet = TRUE)
  
  cat("  > Exported:", basename(out_path), "features:", nrow(thiessen), "\n")
}

cat("Done. Thiessen layers saved in the same output folder (EPSG:9377, 2D, MULTIPOLYGON).\n")
