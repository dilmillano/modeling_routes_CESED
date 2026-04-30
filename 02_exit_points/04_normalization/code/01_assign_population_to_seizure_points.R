# ------------------------------------------------------------
# R script: 01_assign_population_to_seizure_points
# - Join Thiessen-level population (GPW and WorldPop) back to
#   seizure points by ID (ID_SEIZ in Thiessen, ID_SEIZURE in points)
# - Inputs:
#   * Thiessen polygons with population: thiessen_YYYY_pop.shp
#   * Seizure points: seiz_YYYY.shp
# - Output:
#   * Seizure points with added columns:
#       - pop_gpw
#       - pp_wrld
#     saved as seiz_YYYY_pop.shp
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tools)
})

# ----------------------------
# 0) Paths and constants
# ----------------------------

# Thiessen polygons with population (per year)
dir_thiessen_pop <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/03_aggregate_population/output"

# Original seizure points (per year)
dir_seiz_in <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/01_seizures/output"

# Output folder for enriched seizure points
dir_out <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/04_normalization/output_V2"
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Years to process
years <- 2017:2022

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

# ----------------------------
# 1) Loop over years
# ----------------------------
for (yy in years) {
  cat("Processing year:", yy, "\n")
  
  # 1.1 Paths for this year
  thi_path  <- file.path(dir_thiessen_pop, sprintf("thiessen_%d_pop.shp", yy))
  seiz_path <- file.path(dir_seiz_in,      sprintf("seiz_%d.shp", yy))
  
  if (!file.exists(thi_path)) {
    warning(sprintf("Thiessen population file not found, skipping year %d: %s", yy, thi_path))
    next
  }
  if (!file.exists(seiz_path)) {
    warning(sprintf("Seizure points file not found, skipping year %d: %s", yy, seiz_path))
    next
  }
  
  # 1.2 Read Thiessen polygons with population
  thi <- st_read(thi_path, quiet = TRUE)
  
  # Check required columns in Thiessen (ID_SEIZ truncated, pop_gpw, pp_wrld)
  cols_thi <- names(thi)
  required_thi <- c("ID_SEIZ", "pop_gpw", "pp_wrld")
  missing_thi <- setdiff(required_thi, cols_thi)
  if (length(missing_thi) > 0) {
    stop(sprintf("Missing columns in %s: %s",
                 basename(thi_path), paste(missing_thi, collapse = ", ")))
  }
  
  # Keep only key and population columns (no geometry)
  thi_attr <- thi %>%
    st_drop_geometry() %>%
    select(ID_SEIZ, pop_gpw, pp_wrld)
  
  # 1.3 Read seizure points
  seiz <- st_read(seiz_path, quiet = TRUE)
  
  # In seizures, the key is ID_SEIZURE (not ID_SEIZ)
  if (!("ID_SEIZURE" %in% names(seiz))) {
    stop(sprintf("ID_SEIZURE field not found in seizure file: %s", basename(seiz_path)))
  }
  
  # 1.4 Join population from Thiessen to seizure points
  # Match: seizures$ID_SEIZURE == thiessen$ID_SEIZ
  seiz_joined <- seiz %>%
    left_join(thi_attr, by = c("ID_SEIZURE" = "ID_SEIZ"))
  
  # 1.5 Save output shapefile
  out_path <- file.path(dir_out, sprintf("seiz_%d_pop.shp", yy))
  remove_existing_shp(out_path)
  st_write(seiz_joined, out_path, driver = "ESRI Shapefile", quiet = TRUE)
  
  cat("  > Saved enriched seizures:", basename(out_path),
      " (features:", nrow(seiz_joined), ")\n")
}

cat("Done. Seizure point layers with population saved in 04_normalization/output.\n")
