# ------------------------------------------------------------
# R script: 03_compare_CENSO_2018
# - Aggregate GPW and WorldPop population (2018) by municipality
#   using CNP 2018 polygons
# - Compare both sources against official census population (j_Poblacio)
# - Outputs:
#   * Shapefile with added columns (gpw18_pop, wp18_pop)
#   * CSV with per-municipality comparison
#   * Console summary stats (MAE, RMSE, Bias)
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

# Municipalities shapefile (CNP 2018)
path_mpios <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/03_aggregate_population/input/MPIOS_limpio_CNP2018.shp"

# Raster folders
dir_gpw <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/02_population/output/gpw"
dir_wp  <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/02_population/output/worldpop"

# Base output folder and subfolder for this step
base_out <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/03_aggregate_population/output"
dir_out  <- file.path(base_out, "06_compare_CENSO_2018")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Target CRS and census year
crs_target_epsg <- 9377
year_censo <- 2018

# Expected census population field in municipalities shapefile
censo_field <- "j_Poblacio"

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

# Sum raster values inside polygons
sum_raster_by_polygons <- function(r, sfx) {
  # Ensure polygons and raster share CRS
  if (!is.na(terra::crs(r))) {
    sfx <- if (!is.na(sf::st_crs(sfx)) && sf::st_crs(sfx)$epsg != terra::crs(r, proj=TRUE)) {
      sf::st_transform(sfx, terra::crs(r, proj=TRUE))
    } else sfx
  }
  vals <- terra::extract(r, terra::vect(sfx), fun = sum, na.rm = TRUE, exact = FALSE)
  vals[[2]]
}

# ----------------------------
# 1) Read municipalities (CNP 2018)
# ----------------------------
mpios <- sf::st_read(path_mpios, quiet = TRUE)

if (is.na(sf::st_crs(mpios))) stop("Municipalities shapefile has no CRS defined.")
if (sf::st_crs(mpios)$epsg != crs_target_epsg) {
  mpios <- sf::st_transform(mpios, crs_target_epsg)
}
mpios <- force_2d(mpios)

if (!(censo_field %in% names(mpios))) {
  stop(sprintf("Field '%s' (official census population) not found in municipalities layer.", censo_field))
}

# ----------------------------
# 2) Read GPW and WorldPop rasters for 2018
# ----------------------------
gpw_path <- file.path(dir_gpw, sprintf("GPW_Population_%d_9377.tif", year_censo))
wp_path  <- file.path(dir_wp,  sprintf("worldpop_population_%d_9377.tif", year_censo))

gpw_r <- read_rast_safe(gpw_path)
wp_r  <- read_rast_safe(wp_path)

if (is.na(terra::crs(gpw_r))) stop(sprintf("Raster CRS undefined: %s", gpw_path))
if (is.na(terra::crs(wp_r)))  stop(sprintf("Raster CRS undefined: %s", wp_path))

# ----------------------------
# 3) Aggregate population by municipality
# ----------------------------
cat("Aggregating GPW population by municipality...\n")
gpw_pop <- sum_raster_by_polygons(gpw_r, mpios)

cat("Aggregating WorldPop population by municipality...\n")
wp_pop  <- sum_raster_by_polygons(wp_r, mpios)

# Attach new columns (short names for shapefile compatibility)
mpios_out <- mpios %>%
  mutate(
    gpw18_pop = as.numeric(gpw_pop),
    wp18_pop  = as.numeric(wp_pop)
  )




###################################################################################

# ----------------------------
# 4) Compare against census 2018 (j_Poblacio)
# ----------------------------
df_comp <- mpios_out %>%
  st_drop_geometry() %>%
  filter(!is.na(.data[[censo_field]])) %>%
  mutate(
    censo = as.numeric(.data[[censo_field]])
  )

# ----------------------------
# 5) Save outputs in 06_compare_CENSO_2018
# ----------------------------

# Shapefile with aggregated populations
out_shp <- file.path(dir_out, "06_compare_CENSO_2018.shp")
remove_existing_shp(out_shp)
sf::st_write(mpios_out, out_shp, driver = "ESRI Shapefile", quiet = TRUE)
cat("\nOutput shapefile saved at:\n", out_shp, "\n")

# CSV with per-municipality comparison
out_csv <- file.path(dir_out, "06_compare_CENSO_2018_per_municipio.csv")
write.csv(df_comp, out_csv, row.names = FALSE)
cat("Per-municipality comparison saved at:\n", out_csv, "\n")

# ----------------------------
# 6) Correlation against census (GPW and WorldPop)
# ----------------------------

# Keep only valid, positive values for correlation
df_metrics <- df_comp %>%
  filter(!is.na(censo), censo > 0,
         !is.na(gpw18_pop), gpw18_pop >= 0,
         !is.na(wp18_pop),  wp18_pop  >= 0)

# Correlations
r_gpw <- cor(df_metrics$censo, df_metrics$gpw18_pop, use = "complete.obs")
r_wp  <- cor(df_metrics$censo, df_metrics$wp18_pop,  use = "complete.obs")

cat("\n===== Correlation with Census 2018 (j_Poblacio) =====\n")
cat(sprintf("GPW 2018     - Correlation (r): %.3f\n", r_gpw))
cat(sprintf("WorldPop 2018 - Correlation (r): %.3f\n", r_wp))
cat("Note: values closer to 1 indicate stronger linear association with census.\n")

# ----------------------------
# 7) Scatterplots with correlation printed
# ----------------------------

suppressPackageStartupMessages({
  library(ggplot2)
})

plot_comparison <- function(data, x, y, title, out_file) {
  # Compute correlation for this pair
  r_val <- cor(data[[x]], data[[y]], use = "complete.obs")
  subtitle_text <- sprintf("Correlation (r) = %.3f", r_val)
  
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(alpha = 0.6, color = "#4A7EBB") +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(
      title    = title,
      subtitle = subtitle_text,
      x = "Census 2018 population (DANE)",
      y = "Estimated population"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(out_file, plot = p, width = 6, height = 5, dpi = 300)
  cat("Saved plot:", out_file, "\n")
}

# GPW vs Census
plot_comparison(
  data    = df_metrics,
  x       = "censo",
  y       = "gpw18_pop",
  title   = "GPW 2018 vs Census 2018 (DANE)",
  out_file = file.path(dir_out, "scatter_GPW_vs_Censo2018.png")
)

# WorldPop vs Census
plot_comparison(
  data    = df_metrics,
  x       = "censo",
  y       = "wp18_pop",
  title   = "WorldPop 2018 vs Census 2018 (DANE)",
  out_file = file.path(dir_out, "scatter_WorldPop_vs_Censo2018.png")
)

cat("\nBoth plots saved in:\n", dir_out, "\n")
