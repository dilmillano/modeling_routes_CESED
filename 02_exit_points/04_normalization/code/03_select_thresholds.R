# ------------------------------------------------------------
# R script: 03_select_thresholds_V2
# - Inputs:
#   * seiz_2017_pop.shp ... seiz_2022_pop.shp
#       in 04_normalization/output_V2
#   * percentiles_z_per_cultivo.xlsx (ya calculado antes)
#   * World_Countries.shp
# - Paso 1: leer y apilar todos los seiz_YYYY_pop.shp
# - Paso 2: leer percentiles por cultivo desde Excel
# - Paso 3: clasificar puntos como "land" (dentro de World_Countries)
#           o "ocean" (fuera)
# - Paso 4: para cada año:
#     * TIERRA:
#         - GPW: z_pc_gpw >= gpw_p80 / gpw_p90 / gpw_p99 (por cultivo)
#         - WP : z_pc_wp  >= wp_p80  / wp_p90  / wp_p99  (por cultivo)
#     * OCEANO:
#         - cantidd > 1000 kg
#     * unir tierra+oceano y guardar:
#         seiz_YYYY_pop_gpw_p80.shp
#         seiz_YYYY_pop_gpw_p90.shp
#         seiz_YYYY_pop_gpw_p99.shp
#         seiz_YYYY_pop_wp_p80.shp
#         seiz_YYYY_pop_wp_p90.shp
#         seiz_YYYY_pop_wp_p99.shp
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(openxlsx)
  library(tools)
})

# ----------------------------
# 0) Paths and constants
# ----------------------------

dir_seiz <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/04_normalization/output_V2"

world_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/04_normalization/input/World_Countries.shp"

years <- 2017:2022

# Excel con percentiles (ya creado en el paso anterior)
percentiles_xlsx <- file.path(dir_seiz, "percentiles_z_per_cultivo.xlsx")

# ----------------------------
# Helpers
# ----------------------------

# Borrar shapefile anterior de forma limpia
remove_existing_shp <- function(path_with_ext) {
  base_no_ext <- tools::file_path_sans_ext(path_with_ext)
  exts <- c(".shp",".shx",".dbf",".prj",".cpg",".qmd",".qix",".sbn",".sbx")
  files <- paste0(base_no_ext, exts)
  files <- files[file.exists(files)]
  if (length(files)) invisible(file.remove(files))
}

# ----------------------------
# 1) Leer y apilar todos los seiz_YYYY_pop.shp
# ----------------------------

all_list <- list()

for (yy in years) {
  shp_path <- file.path(dir_seiz, sprintf("seiz_%d_pop.shp", yy))
  if (!file.exists(shp_path)) {
    warning(sprintf("File not found, skipping year %d: %s", yy, shp_path))
    next
  }
  cat("Reading:", basename(shp_path), "\n")
  tmp <- st_read(shp_path, quiet = TRUE)
  # Asegurar que year exista y sea numerico
  if (!("year" %in% names(tmp))) {
    tmp$year <- yy
  }
  tmp$year <- as.numeric(tmp$year)
  all_list[[as.character(yy)]] <- tmp
}

if (length(all_list) == 0) {
  stop("No seiz_YYYY_pop.shp files were found in output_V2. Nothing to process.")
}

all_seiz <- do.call(rbind, all_list)

# ----------------------------
# 2) Leer percentiles desde Excel
# ----------------------------

if (!file.exists(percentiles_xlsx)) {
  stop(sprintf("Percentiles Excel not found: %s", percentiles_xlsx))
}

percentiles_tbl <- read.xlsx(percentiles_xlsx, sheet = "Percentiles_Z")

# Esperamos columnas al menos:
# cultivo, gpw_p80, gpw_p90, gpw_p99, wp_p80, wp_p90, wp_p99
needed_cols_p <- c("cultivo", "gpw_p80", "gpw_p90", "gpw_p99",
                   "wp_p80", "wp_p90", "wp_p99")
missing_p <- setdiff(needed_cols_p, names(percentiles_tbl))
if (length(missing_p) > 0) {
  stop(sprintf("Missing columns in percentiles table: %s",
               paste(missing_p, collapse = ", ")))
}

cat("\nPercentiles loaded from Excel (first rows):\n")
print(head(percentiles_tbl))

# ----------------------------
# 3) Clasificar puntos como tierra u oceano
# ----------------------------

world <- st_read(world_path, quiet = TRUE)
if (is.na(st_crs(world))) {
  stop("World_Countries.shp has no CRS defined.")
}

world <- suppressWarnings(st_make_valid(world))

if (st_crs(world) != st_crs(all_seiz)) {
  world <- st_transform(world, st_crs(all_seiz))
}

all_seiz <- suppressWarnings(st_make_valid(all_seiz))

cat("\nComputing land / ocean classification...\n")
inters <- st_intersects(all_seiz, world)
is_land <- lengths(inters) > 0

all_seiz$is_land <- is_land

cat("  > Land points   :", sum(all_seiz$is_land, na.rm = TRUE), "\n")
cat("  > Ocean points  :", sum(!all_seiz$is_land, na.rm = TRUE), "\n")

# ----------------------------
# 4) Para cada año: filtrar por umbrales y guardar GPW/WP
# ----------------------------

for (yy in years) {
  cat("\n=================================\n")
  cat("Processing year:", yy, "\n")
  
  seiz_year <- all_seiz %>%
    filter(year == yy)
  
  if (nrow(seiz_year) == 0) {
    cat("  > No records for this year in the stacked data. Skipping.\n")
    next
  }
  
  # Dividir en tierra y oceano
  seiz_land  <- seiz_year %>% filter(is_land)
  seiz_ocean <- seiz_year %>% filter(!is_land)
  
  cat("  > Land points  in", yy, ":", nrow(seiz_land), "\n")
  cat("  > Ocean points in", yy, ":", nrow(seiz_ocean), "\n")
  
  # Revisar columnas necesarias
  if (!all(c("cultivo", "z_pc_gpw", "z_pc_wp", "cantidd") %in% names(seiz_year))) {
    stop(sprintf("Year %d is missing one of the required columns: cultivo, z_pc_gpw, z_pc_wp, cantidd", yy))
  }
  
  # Subconjunto de océano: cantidd > 1000 kg
  if (nrow(seiz_ocean) > 0) {
    seiz_ocean_thr <- seiz_ocean %>%
      mutate(cantidd = as.numeric(cantidd)) %>%
      filter(is.finite(cantidd), cantidd > 1000)
    cat("  > Ocean points with cantidd > 1000 kg in", yy, ":",
        nrow(seiz_ocean_thr), "\n")
  } else {
    seiz_ocean_thr <- seiz_ocean[0, ]
    cat("  > No ocean points in this year. Ocean threshold selection skipped.\n")
  }
  
  # ---- TIERRA: preparar base con z_pc_gpw / z_pc_wp numéricas ----
  land_base <- seiz_land %>%
    mutate(
      z_pc_gpw = as.numeric(z_pc_gpw),
      z_pc_wp  = as.numeric(z_pc_wp)
    )
  
  # ---------------- GPW: p80, p90, p99 ----------------
  
  land_gpw <- land_base %>%
    left_join(
      percentiles_tbl %>% select(cultivo, gpw_p80, gpw_p90, gpw_p99),
      by = "cultivo"
    )
  
  # GPW p80
  seiz_gpw_p80_land <- land_gpw %>%
    filter(!is.na(gpw_p80), is.finite(z_pc_gpw)) %>%
    filter(z_pc_gpw >= gpw_p80) %>%
    select(-gpw_p80, -gpw_p90, -gpw_p99)
  
  # GPW p90
  seiz_gpw_p90_land <- land_gpw %>%
    filter(!is.na(gpw_p90), is.finite(z_pc_gpw)) %>%
    filter(z_pc_gpw >= gpw_p90) %>%
    select(-gpw_p80, -gpw_p90, -gpw_p99)
  
  # GPW p99
  seiz_gpw_p99_land <- land_gpw %>%
    filter(!is.na(gpw_p99), is.finite(z_pc_gpw)) %>%
    filter(z_pc_gpw >= gpw_p99) %>%
    select(-gpw_p80, -gpw_p90, -gpw_p99)
  
  cat("  > Land (GPW) p80 in", yy, ":", nrow(seiz_gpw_p80_land), "\n")
  cat("  > Land (GPW) p90 in", yy, ":", nrow(seiz_gpw_p90_land), "\n")
  cat("  > Land (GPW) p99 in", yy, ":", nrow(seiz_gpw_p99_land), "\n")
  
  # ---------------- WorldPop: p80, p90, p99 ----------------
  
  land_wp <- land_base %>%
    left_join(
      percentiles_tbl %>% select(cultivo, wp_p80, wp_p90, wp_p99),
      by = "cultivo"
    )
  
  # WP p80
  seiz_wp_p80_land <- land_wp %>%
    filter(!is.na(wp_p80), is.finite(z_pc_wp)) %>%
    filter(z_pc_wp >= wp_p80) %>%
    select(-wp_p80, -wp_p90, -wp_p99)
  
  # WP p90
  seiz_wp_p90_land <- land_wp %>%
    filter(!is.na(wp_p90), is.finite(z_pc_wp)) %>%
    filter(z_pc_wp >= wp_p90) %>%
    select(-wp_p80, -wp_p90, -wp_p99)
  
  # WP p99
  seiz_wp_p99_land <- land_wp %>%
    filter(!is.na(wp_p99), is.finite(z_pc_wp)) %>%
    filter(z_pc_wp >= wp_p99) %>%
    select(-wp_p80, -wp_p90, -wp_p99)
  
  cat("  > Land (WP)  p80 in", yy, ":", nrow(seiz_wp_p80_land), "\n")
  cat("  > Land (WP)  p90 in", yy, ":", nrow(seiz_wp_p90_land), "\n")
  cat("  > Land (WP)  p99 in", yy, ":", nrow(seiz_wp_p99_land), "\n")
  
  # ---- Función auxiliar para unir tierra+oceano y guardar ----
  merge_and_save <- function(land_sf, ocean_sf, suffix) {
    merged <- rbind(land_sf, ocean_sf)
    cat("  > Total selected for", suffix, "in", yy, ":", nrow(merged), "\n")
    out_path <- file.path(dir_seiz, sprintf("seiz_%d_pop_%s.shp", yy, suffix))
    remove_existing_shp(out_path)
    st_write(merged, out_path, driver = "ESRI Shapefile", quiet = TRUE)
    cat("  > Saved:", out_path, "\n")
  }
  
  # Guardar GPW
  merge_and_save(seiz_gpw_p80_land, seiz_ocean_thr, "gpw_p80")
  merge_and_save(seiz_gpw_p90_land, seiz_ocean_thr, "gpw_p90")
  merge_and_save(seiz_gpw_p99_land, seiz_ocean_thr, "gpw_p99")
  
  # Guardar WP
  merge_and_save(seiz_wp_p80_land, seiz_ocean_thr, "wp_p80")
  merge_and_save(seiz_wp_p90_land, seiz_ocean_thr, "wp_p90")
  merge_and_save(seiz_wp_p99_land, seiz_ocean_thr, "wp_p99")
}

cat("\nDone. Threshold-based shapefiles saved in output_V2 using precomputed percentiles.\n")
