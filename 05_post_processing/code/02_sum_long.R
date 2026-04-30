################################################################################
# RESUMEN POR AÑO: SUMA DE LONGITUD (km) POR CATEGORÍA "conf"
# - Lee shapefiles: rutas_LCP_EPOF_YYYY.shp (2017-2022)
# - Calcula long_km por segmento (si no existe) usando geometría
# - Agrupa por year y conf, sumando long_km
# - Exporta a Excel: EPOF_length_by_conf.xlsx
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(openxlsx)
})

sf_use_s2(FALSE)

# -------------------------
# 1) Inputs
# -------------------------
in_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/05_post_processing/output"
years  <- c("2017","2018","2019","2020","2021","2022")

# salida excel
# --- SOLO CAMBIO: RUTA DE SALIDA DEL EXCEL ---
out_dir_results <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results"
dir.create(out_dir_results, showWarnings = FALSE, recursive = TRUE)

out_xlsx <- file.path(out_dir_results, "EPOF_length_by_conf.xlsx")

# -------------------------
# 2) Loop lectura + resumen
# -------------------------
all_summaries <- list()

for (yr in years) {
  
  shp <- file.path(in_dir, paste0("rutas_LCP_EPOF_", yr, ".shp"))
  
  cat("\n====================\n")
  cat("AÑO:", yr, "\n")
  cat("Archivo:", shp, "\n")
  cat("====================\n")
  
  if (!file.exists(shp)) {
    cat("??? NO EXISTE -> salto este año\n")
    next
  }
  
  x <- st_read(shp, quiet = TRUE)
  
  # Validar columna conf
  if (!"conf" %in% names(x)) {
    stop("??? No existe la columna 'conf' en: ", shp,
         "\nColumnas disponibles: ", paste(names(x), collapse = ", "))
  }
  
  # Asegurar que conf sea numérica
  x$conf <- as.numeric(x$conf)
  
  # Calcular long_km si no existe (o si existe pero quieres recalcular siempre)
  # OJO: esto asume CRS proyectado en metros (lo usual en tus capas EPSG:9377, etc.)
  # Si fuera grados, esto quedaría mal (pero en tu caso venías en 9377).
  x$long_km <- as.numeric(st_length(x)) / 1000
  
  cat("CRS EPSG:", st_crs(x)$epsg, "\n")
  cat("n_filas:", nrow(x), "\n")
  cat("long_km total (aprox):", round(sum(x$long_km, na.rm = TRUE), 3), "\n")
  cat("conf únicos:", paste(sort(unique(x$conf)), collapse = ", "), "\n")
  
  # Resumen por conf
  sum_df <- x |>
    st_drop_geometry() |>
    group_by(conf = conf) |>
    summarise(long_km = sum(long_km, na.rm = TRUE), .groups = "drop") |>
    mutate(year = as.integer(yr)) |>
    select(year, conf, long_km) |>
    arrange(year, conf)
  
  all_summaries[[yr]] <- sum_df
}

final_table <- bind_rows(all_summaries) |>
  arrange(year, conf)

cat("\n====================\n")
cat("TABLA FINAL\n")
cat("====================\n")
print(final_table)

# -------------------------
# 3) Exportar a Excel
# -------------------------
wb <- createWorkbook()
addWorksheet(wb, "length_by_conf")
writeDataTable(wb, "length_by_conf", final_table)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

cat("\nLISTO ??? Excel guardado en:\n", out_xlsx, "\n")
