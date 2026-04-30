# ------------------------------------------------------------
# R script: 02_compute_per_capita_and_z + 06_percentiles_by_cultivo
# - Parte 1: para cada año, calcular per capita y z-scores
# - Parte 2: apilar todos los años y calcular percentiles por cultivo
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tools)
  library(openxlsx)
})

# ============================================================
# 0) Paths and constants
# ============================================================

dir_seiz <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/04_normalization/output_V2"
years    <- 2017:2022

# Excel output file for percentiles
out_xlsx <- file.path(dir_seiz, "percentiles_z_per_cultivo.xlsx")

# ============================================================
# Helpers
# ============================================================

# Cleanly overwrite an ESRI Shapefile set
remove_existing_shp <- function(path_with_ext) {
  base_no_ext <- tools::file_path_sans_ext(path_with_ext)
  exts <- c(".shp",".shx",".dbf",".prj",".cpg",".qmd",".qix",".sbn",".sbx")
  files <- paste0(base_no_ext, exts)
  files <- files[file.exists(files)]
  if (length(files)) invisible(file.remove(files))
}

# Compute z-score safely (returns vector of same length)
z_score_safe <- function(x) {
  v  <- as.numeric(x)
  ok <- is.finite(v)
  v_ok <- v[ok]
  if (length(unique(v_ok)) <= 1) {
    # Not enough variation to compute z-score
    return(rep(NA_real_, length(v)))
  } else {
    z <- rep(NA_real_, length(v))
    z[ok] <- as.numeric(scale(v_ok, center = TRUE, scale = TRUE))
    return(z)
  }
}

# Safe percentiles
probs <- c(0.25, 0.50, 0.75, 0.80, 0.90, 0.95, 0.99)

compute_percentiles <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  as.numeric(quantile(x, probs = probs, na.rm = TRUE))
}

# ============================================================
# 1) Compute per capita and z-scores per year
# ============================================================

cat("==== STEP 1: per capita and z-scores per year ====\n")

for (yy in years) {
  cat("Processing year:", yy, "\n")
  
  seiz_path <- file.path(dir_seiz, sprintf("seiz_%d_pop.shp", yy))
  if (!file.exists(seiz_path)) {
    warning(sprintf("File not found, skipping year %d: %s", yy, seiz_path))
    next
  }
  
  # 1.1 Read seizures
  seiz <- st_read(seiz_path, quiet = TRUE)
  
  # 1.2 Check required columns
  needed_cols <- c("cantidd", "pop_gpw", "pp_wrld")
  missing_cols <- setdiff(needed_cols, names(seiz))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in %s: %s",
                 basename(seiz_path), paste(missing_cols, collapse = ", ")))
  }
  
  # 1.3 Ensure numeric
  seiz <- seiz %>%
    mutate(
      cantidd = as.numeric(cantidd),
      pop_gpw = as.numeric(pop_gpw),
      pp_wrld = as.numeric(pp_wrld)
    )
  
  # 1.4 Compute per capita (avoid division by zero)
  seiz <- seiz %>%
    mutate(
      pc_gpw = ifelse(pop_gpw > 0, cantidd / pop_gpw, NA_real_),
      pc_wp  = ifelse(pp_wrld > 0, cantidd / pp_wrld, NA_real_)
    )
  
  # Replace infinite values with NA (just in case)
  seiz$pc_gpw[is.infinite(seiz$pc_gpw)] <- NA_real_
  seiz$pc_wp[is.infinite(seiz$pc_wp)]   <- NA_real_
  
  # 1.5 Compute z-scores of per capita values within this year
  seiz <- seiz %>%
    mutate(
      z_pc_gpw = z_score_safe(pc_gpw),
      z_pc_wp  = z_score_safe(pc_wp)
    )
  
  # 1.6 Save back (overwrite same shapefile)
  out_path <- seiz_path
  remove_existing_shp(out_path)
  st_write(seiz, out_path, driver = "ESRI Shapefile", quiet = TRUE)
  
  cat("  > Updated:", basename(out_path),
      "with pc_gpw, pc_wp, z_pc_gpw, z_pc_wp\n")
}

cat("Done STEP 1. Fields pc_gpw, pc_wp, z_pc_gpw, z_pc_wp added to seiz_YYYY_pop.shp in output_V2.\n\n")

# ============================================================
# 2) Percentiles by cultivo (using all years together)
# ============================================================

cat("==== STEP 2: percentiles by cultivo over all years ====\n")

all_list <- list()

for (yy in years) {
  shp_path <- file.path(dir_seiz, sprintf("seiz_%d_pop.shp", yy))
  if (!file.exists(shp_path)) {
    warning(sprintf("File not found, skipping year %d: %s", yy, shp_path))
    next
  }
  cat("Reading:", basename(shp_path), "\n")
  tmp <- st_read(shp_path, quiet = TRUE)
  tmp$year <- yy
  all_list[[as.character(yy)]] <- tmp
}

if (length(all_list) == 0) {
  stop("No shapefiles found to process in output_V2.")
}

all_seiz <- do.call(rbind, all_list)
df <- st_drop_geometry(all_seiz)

# 2.1 Verify required columns
needed_cols2 <- c("cultivo", "z_pc_gpw", "z_pc_wp")
missing_cols2 <- setdiff(needed_cols2, names(df))
if (length(missing_cols2) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols2, collapse = ", ")))
}

df <- df %>%
  filter(!is.na(cultivo)) %>%
  mutate(
    z_pc_gpw = as.numeric(z_pc_gpw),
    z_pc_wp  = as.numeric(z_pc_wp)
  )

# 2.2 Compute percentiles by cultivo

# GPW
res_gpw <- df %>%
  group_by(cultivo) %>%
  summarise(
    {
      q <- compute_percentiles(z_pc_gpw)
      tibble(
        gpw_p25 = q[1],
        gpw_p50 = q[2],
        gpw_p75 = q[3],
        gpw_p80 = q[4],
        gpw_p90 = q[5],
        gpw_p95 = q[6],
        gpw_p99 = q[7]
      )
    },
    .groups = "drop"
  )

# WorldPop
res_wp <- df %>%
  group_by(cultivo) %>%
  summarise(
    {
      q <- compute_percentiles(z_pc_wp)
      tibble(
        wp_p25 = q[1],
        wp_p50 = q[2],
        wp_p75 = q[3],
        wp_p80 = q[4],
        wp_p90 = q[5],
        wp_p95 = q[6],
        wp_p99 = q[7]
      )
    },
    .groups = "drop"
  )

percentiles_cultivo <- res_gpw %>%
  left_join(res_wp, by = "cultivo")

# 2.3 Save to Excel

wb <- createWorkbook()
addWorksheet(wb, "Percentiles_Z")
writeData(wb, "Percentiles_Z", percentiles_cultivo)
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

cat("\nPercentile summary saved to Excel:\n", out_xlsx, "\n")
cat("\nPreview (first rows):\n")
print(head(percentiles_cultivo))
