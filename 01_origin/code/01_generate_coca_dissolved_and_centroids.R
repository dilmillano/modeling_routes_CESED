# ------------------------------------------------------------
# R script: 01_generate_coca_dissolved_and_centroids
# - For each year column C_YYYY in the SIMCI coca polygons:
#   * Filter cells with more than 10 and more than 20 hectares
#   * Dissolve contiguous polygons separately for each threshold
#   * Compute centroids of the dissolved polygons
#   * Plot dissolved polygons and centroids for visual tracking
# - Input:
#   * COCA_SIMCI_21012025.shp with columns:
#       - C_2017, C_2018, ..., C_2022 (hectares of coca)
# - Outputs (per year, saved in the output folder):
#   * dissolved_YYYY_gt10ha.shp  : dissolved polygons with >10 ha
#   * centroids_YYYY_gt10ha.shp  : centroids of dissolved polygons >10 ha
#   * dissolved_YYYY_gt20ha.shp  : dissolved polygons with >20 ha
#   * centroids_YYYY_gt20ha.shp  : centroids of dissolved polygons >20 ha
#   * map_coca_YYYY_10_20ha.png  : map with polygons + both sets of centroids
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(rlang)
  library(tools)
  library(ggplot2)
})

# ----------------------------
# 0) Paths and constants
# ----------------------------

# Input shapefile with coca by year (C_YYYY columns)
input_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/01_origin/input/COCA_SIMCI_21012025.shp"

# Output folder for dissolved polygons, centroids, and maps
output_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/01_origin/output"

# Thresholds in hectares to filter cells (both will be applied per year)
thresholds_ha <- c(10, 20)

# Create output folder if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ----------------------------
# 1) Load input shapefile
# ----------------------------

coca_sf <- st_read(input_path)

# Optional: inspect column names
print(colnames(coca_sf))

# ----------------------------
# 2) Identify year columns C_YYYY (2017-2022)
# ----------------------------

target_years <- 2017:2022
year_cols <- paste0("C_", target_years)
year_cols <- year_cols[year_cols %in% names(coca_sf)]

message("Year columns detected: ", paste(year_cols, collapse = ", "))

# ----------------------------
# 3) Define processing function per year
# ----------------------------

process_year <- function(sf_data, col_name, thresholds, out_dir) {
  # Extract year label from column name, e.g. "C_2017" -> "2017"
  year_label <- sub("C_", "", col_name)
  message("Processing year: ", year_label)
  
  # Keep geometry and the specific year column
  year_data <- sf_data %>%
    select(geometry, all_of(col_name))
  
  # List to store dissolved polygons and centroids per threshold for plotting
  results <- list()
  
  # ---- 3.1) Loop over thresholds (>10 ha, >20 ha) ----
  for (thr in thresholds) {
    thr_label <- paste0("gt", thr, "ha")
    message("  Threshold: > ", thr, " ha")
    
    # Filter polygons with values > thr hectares in the year column
    filtered <- year_data %>%
      filter(!!sym(col_name) > thr)
    
    # If no polygons remain after filtering, skip this threshold
    if (nrow(filtered) == 0) {
      message("    No polygons with values >", thr,
              " ha for year ", year_label, " - skipping this threshold.")
      next
    }
    
    # Dissolve contiguous polygons
    dissolved <- st_union(filtered)
    dissolved <- st_cast(dissolved, "POLYGON")  # ensure individual polygons
    
    # Compute centroids of dissolved polygons
    centroids <- st_centroid(dissolved)
    
    # Convert dissolved and centroids to sf objects for plotting and writing
    dissolved_sf <- st_as_sf(data.frame(
      id = seq_along(dissolved),
      geometry = dissolved
    ))
    
    centroids_sf <- st_as_sf(data.frame(
      id = seq_along(centroids),
      geometry = centroids
    ))
    
    # Build output file paths
    dissolved_file <- file.path(out_dir,
                                paste0("dissolved_", year_label, "_", thr_label, ".shp"))
    centroids_file <- file.path(out_dir,
                                paste0("centroids_", year_label, "_", thr_label, ".shp"))
    
    # Save shapefiles
    st_write(dissolved_sf, dissolved_file, delete_layer = TRUE)
    st_write(centroids_sf, centroids_file, delete_layer = TRUE)
    
    message("    Saved shapefiles: ",
            basename(dissolved_file), " and ", basename(centroids_file))
    
    # Store for plotting
    results[[thr_label]] <- list(
      threshold_value = thr,
      dissolved = dissolved_sf,
      centroids = centroids_sf
    )
  }
  
  # ---- 3.2) Plot polygons and centroids (if at least one threshold has results) ----
  if (length(results) == 0) {
    message("  No results for any threshold in year ", year_label,
            " - map will not be created.")
    return(invisible(NULL))
  }
  
  # Choose polygons to plot:
  # - Prefer dissolved polygons for >10 ha if available,
  #   otherwise use the first available threshold
  if ("gt10ha" %in% names(results)) {
    poly_to_plot <- results[["gt10ha"]]$dissolved
  } else {
    poly_to_plot <- results[[1]]$dissolved
  }
  
  # Combine centroids for all thresholds into one sf object
  centroids_list <- list()
  for (nm in names(results)) {
    thr_val <- results[[nm]]$threshold_value
    cent_sf <- results[[nm]]$centroids
    cent_sf$threshold <- paste0("> ", thr_val, " ha")
    centroids_list[[nm]] <- cent_sf
  }
  centroids_all <- do.call(rbind, centroids_list)
  
  # Define colors for thresholds (if both exist, both will be shown)
  unique_thr <- sort(unique(centroids_all$threshold))
  color_values <- c("> 10 ha" = "red", "> 20 ha" = "blue")
  color_values <- color_values[unique_thr]
  
  # Build plot
  p <- ggplot() +
    geom_sf(data = poly_to_plot, fill = NA, color = "grey40", linewidth = 0.3) +
    geom_sf(data = centroids_all, aes(color = threshold), size = 1) +
    scale_color_manual(values = color_values, name = "Centroid thresholds") +
    ggtitle(paste("Coca polygons and centroids (", year_label, ")", sep = "")) +
    theme_minimal()
  
  # Build map file name (e.g., map_coca_2017_10_20ha.png)
  thr_suffix <- paste(thresholds, collapse = "_")
  map_file <- file.path(out_dir,
                        paste0("map_coca_", year_label, "_", thr_suffix, "ha.png"))
  
  ggsave(
    filename = map_file,
    plot     = p,
    width    = 6,
    height   = 6,
    dpi      = 300
  )
  
  message("  Saved map: ", basename(map_file))
}

# ----------------------------
# 4) Loop over all year columns
# ----------------------------

for (col_name in year_cols) {
  process_year(
    sf_data    = coca_sf,
    col_name   = col_name,
    thresholds = thresholds_ha,
    out_dir    = output_dir
  )
}

message("All years processed.")
