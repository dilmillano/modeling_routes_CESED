################################################################################
# k = 50 nearest-neighbor assignments (Euclidean) + elbow plot
# FINAL FIGURE:
#   - Shaded band: k ∈ [18, 22]
#   - Dotted line: k* = 20 (selected operational k)
#   - Title/axes in English
#   - Y-axis with thousands separator
################################################################################

# =========================
# 0) Packages
# =========================
pkgs <- c("sf", "dplyr", "stringr", "ggplot2", "readr", "tibble", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))

# =========================
# 1) Paths
# =========================
path_orig <- "C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/rutas/modeling_routes_CESED/01_origin/output/centroids_2017_gt20ha.shp"
path_dest <- "C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/04_normalization/output_V2/seiz_2017_pop_wp_p99.shp"

out_dir <- "C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/rutas/modeling_routes_CESED/04_LCP/output/test_2017"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Parameters
KMAX      <- 50
band_min  <- 18
band_max  <- 22

# Here we set the operational elbow to be displayed as k*
k_star    <- 20

# =========================
# 2) Helpers
# =========================
choose_projected_crs <- function(x_sf) {
  if (!sf::st_is_longlat(x_sf)) return(sf::st_crs(x_sf))
  bb <- sf::st_bbox(x_sf)
  lon <- (bb["xmin"] + bb["xmax"]) / 2
  lat <- (bb["ymin"] + bb["ymax"]) / 2
  zone <- floor((as.numeric(lon) + 180) / 6) + 1
  epsg <- if (as.numeric(lat) >= 0) 32600 + zone else 32700 + zone
  sf::st_crs(epsg)
}

guess_id_col <- function(df) {
  nms <- names(df)
  pats <- c("^id$", "id_", "gid", "codigo", "cod", "objectid", "fid")
  for (p in pats) {
    hit <- nms[stringr::str_detect(tolower(nms), p)]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

# =========================
# 3) Read data
# =========================
orig <- sf::st_read(path_orig, quiet = TRUE)
dest <- sf::st_read(path_dest, quiet = TRUE)

# Ensure IDs exist (or create)
orig_id_col <- guess_id_col(orig)
dest_id_col <- guess_id_col(dest)

if (is.na(orig_id_col)) {
  orig <- orig %>% dplyr::mutate(ID_ORIG = dplyr::row_number())
} else {
  orig <- orig %>% dplyr::mutate(ID_ORIG = as.character(.data[[orig_id_col]]))
}

if (is.na(dest_id_col)) {
  dest <- dest %>% dplyr::mutate(ID_DEST = dplyr::row_number())
} else {
  dest <- dest %>% dplyr::mutate(ID_DEST = as.character(.data[[dest_id_col]]))
}

# =========================
# 4) Project to meters
# =========================
crs_proj <- choose_projected_crs(orig)
orig_p <- sf::st_transform(orig, crs_proj)
dest_p <- sf::st_transform(dest, crs_proj)

orig_geom <- sf::st_geometry(orig_p)
dest_geom <- sf::st_geometry(dest_p)

# =========================
# 5) Assign KMAX nearest destinations per origin (Euclidean)
# =========================
K_use <- min(KMAX, length(dest_geom))
if (K_use < KMAX) message("Warning: fewer destinations than KMAX. Using K=", K_use)

assign_list <- vector("list", length(orig_geom))

for (i in seq_along(orig_geom)) {
  d <- sf::st_distance(orig_geom[i], dest_geom)   # units
  d_num <- as.numeric(d)                          # meters (projected CRS)
  ord <- order(d_num, decreasing = FALSE)[1:K_use]
  
  assign_list[[i]] <- tibble::tibble(
    ID_ORIG = as.character(orig_p$ID_ORIG[i]),
    ID_DEST = as.character(dest_p$ID_DEST[ord]),
    k       = seq_len(K_use),
    dist_m  = d_num[ord]
  )
}

assign_tbl <- dplyr::bind_rows(assign_list)

# Save assignments (optional audit)
readr::write_csv(assign_tbl, file.path(out_dir, "Assignments_Origin_TraffickingNode_2017_k50.csv"))

# =========================
# 6) Elbow curve summary (median / P75 / P90)
# =========================
curve <- assign_tbl %>%
  dplyr::group_by(k) %>%
  dplyr::summarise(
    median_m   = median(dist_m, na.rm = TRUE),
    p75_m      = quantile(dist_m, 0.75, na.rm = TRUE),
    p90_m      = quantile(dist_m, 0.90, na.rm = TRUE),
    n_origins  = dplyr::n_distinct(ID_ORIG),
    .groups = "drop"
  ) %>%
  dplyr::arrange(k)

readr::write_csv(curve, file.path(out_dir, "ElbowCurve_EuclideanDistance_2017_k50.csv"))

# =========================
# 7) FINAL PLOT (improved styling)
#  - Shaded band: k ∈ [18,22]
#  - k* = 20 with "halo" (two-layer vertical line)
#  - Legend explaining lines
#  - Thin lines + different colors + different point shapes (circle/square/diamond)
#  - No subtitle
# =========================

band_min_plot <- max(band_min, min(curve$k))
band_max_plot <- min(band_max, max(curve$k))

# Build a long table so legend controls shapes/colors cleanly
curve_long <- curve %>%
  dplyr::select(k, median_m, p75_m, p90_m) %>%
  tidyr::pivot_longer(
    cols = c(median_m, p75_m, p90_m),
    names_to = "series",
    values_to = "dist_m"
  ) %>%
  dplyr::mutate(
    series = dplyr::recode(
      series,
      "median_m" = "Median",
      "p75_m"    = "P75",
      "p90_m"    = "P90"
    ),
    series = factor(series, levels = c("Median", "P75", "P90"))
  )

p_final <- ggplot() +
  # Shaded band (18–22)
  annotate(
    "rect",
    xmin = band_min_plot, xmax = band_max_plot,
    ymin = -Inf, ymax = Inf,
    alpha = 0.15
  ) +
  # k* halo: wide white line underneath + dotted line on top
  geom_vline(xintercept = k_star, linewidth = 4.0, color = "white") +
  geom_vline(xintercept = k_star, linetype = "dotted", linewidth = 1.1) +
  # k* label with subtle halo for readability
  annotate(
    "label",
    x = k_star, y = max(curve_long$dist_m, na.rm = TRUE),
    label = paste0("k* = ", k_star),
    label.size = 0,      # no border
    alpha = 0.85,
    vjust = 0.5,
    hjust = 0.0
  ) +
  # Lines + points (thin; shapes differ)
  geom_line(
    data = curve_long,
    aes(x = k, y = dist_m, color = series),
    linewidth = 0.6
  ) +
  geom_point(
    data = curve_long,
    aes(x = k, y = dist_m, color = series, shape = series),
    size = 1.8,
    stroke = 0.4
  ) +
  # Labels (English) - no subtitle
  labs(
    title = "Euclidean Distance Between Origin Points and Trafficking Nodes",
    x = "k (k-th nearest trafficking node per origin)",
    y = "Distance (m)",
    color = "Curve",
    shape = "Curve"
  ) +
  # Thousands separator on Y axis
  scale_y_continuous(labels = scales::label_comma(big.mark = ",", accuracy = 1)) +
  # Shapes: circle, square, diamond
  scale_shape_manual(values = c("Median" = 16, "P75" = 15, "P90" = 18)) +
  # Colors: leave ggplot default palette? You asked different colors; default already differs.
  # If you want fixed colors, uncomment and set values:
  # scale_color_manual(values = c("Median"="#1b9e77","P75"="#7570b3","P90"="#d95f02")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10),
    plot.title   = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_final)

ggsave(
  filename = file.path(out_dir, "ElbowCurve_Band18_22_kStar20_EuclideanDistance_STYLED.png"),
  plot = p_final,
  width = 9, height = 5, dpi = 300
)
