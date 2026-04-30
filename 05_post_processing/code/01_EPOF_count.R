################################################################################
# CONTEO POR SEGMENTO (INTERSECT) - PARA 2017-2022
# - Lee original + planarize desde la GDB
# - Calcula count = cuántas rutas (original) intersectan cada segmento
# - Paraleliza por bloques (Windows friendly)
# - Guarda una copia del planarize con sufijo _COUNT en la misma GDB
################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(future.apply)
})

# -------------------------
# 1) Configuración general
# -------------------------
sf_use_s2(FALSE)  # importante para evitar complicaciones geométricas

gdb <- "C:/Users/d.millanorduz/Documents/ArcGIS/Projects/COCA_ROUTES/COCA_ROUTES.gdb"

pairs <- list(
  "2017" = list(orig="rutas_LCP_2017_escenar_Merge1",
                seg ="rutas_LCP_2017_escenar_Merge_ExportFeatures"),
  "2018" = list(orig="rutas_LCP_2018_escenar_Merge",
                seg ="rutas_LCP_2018_escenar_Merge_ExportFeatures"),
  "2019" = list(orig="rutas_LCP_2019_escenar_Merge",
                seg ="rutas_LCP_2019_escenar_Merge_ExportFeatures"),
  "2020" = list(orig="rutas_LCP_2020_escenar_Merge",
                seg ="rutas_LCP_2020_escenar_Merge_ExportFeatures"),
  "2021" = list(orig="rutas_LCP_2021_escenar_Merge",
                seg ="rutas_LCP_2021_escenar_Merge_ExportFeatures"),
  "2022" = list(orig="rutas_LCP_2022_escenar_Merge",
                seg ="rutas_LCP_2022_escenar_Merge_ExportFeatures")
)

# -------------------------
# 2) Paralelización (Windows)
# -------------------------
# Usa múltiples procesos (multisession) en Windows.
# Puedes ajustar workers según tu PC (ej: 6, 8, 12).
workers <- max(1, parallel::detectCores() - 2)
plan(multisession, workers = workers)

cat("============================================================\n")
cat("GDB:", gdb, "\n")
cat("Workers:", workers, "\n")
cat("Capas en GDB (solo para verificar):\n")
print(st_layers(gdb)$name)
cat("============================================================\n\n")

# -------------------------
# 3) Función por año
# -------------------------
process_year_intersect <- function(year, orig_layer, seg_layer, gdb_path,
                                   block_size = 2000) {
  
  cat("\n\n====================\n")
  cat("AÑO:", year, "\n")
  cat("Original  :", orig_layer, "\n")
  cat("Planarize :", seg_layer, "\n")
  cat("====================\n")
  
  t0 <- Sys.time()
  
  cat("Leyendo capas...\n")
  orig <- st_read(dsn = gdb_path, layer = orig_layer, quiet = TRUE)
  seg  <- st_read(dsn = gdb_path, layer = seg_layer,  quiet = TRUE)
  
  cat("  n_original:", nrow(orig), "\n")
  cat("  n_segments:", nrow(seg),  "\n")
  cat("  CRS:", st_crs(seg)$epsg, "\n")
  
  # Asegurar mismo CRS
  if (!st_crs(orig) == st_crs(seg)) {
    cat("  CRS distintos -> transformando ORIGINAL al CRS del planarize...\n")
    orig <- st_transform(orig, st_crs(seg))
  }
  
  # Nota: st_intersects solo necesita geometrías válidas normalmente,
  # pero si quieres blindaje:
  inv_o <- sum(!st_is_valid(orig))
  inv_s <- sum(!st_is_valid(seg))
  cat("  inválidas original:", inv_o, "\n")
  cat("  inválidas segment :", inv_s, "\n")
  if (inv_o > 0) orig <- st_make_valid(orig)
  if (inv_s > 0) seg  <- st_make_valid(seg)
  
  n <- nrow(seg)
  idx_blocks <- split(seq_len(n), ceiling(seq_len(n) / block_size))
  cat("Bloques:", length(idx_blocks), " | block_size:", block_size, "\n")
  
  # Función para un bloque
  block_fun <- function(idx) {
    # devuelve vector counts del bloque
    rel <- st_intersects(seg[idx, ], orig, sparse = TRUE)
    lengths(rel)
  }
  
  cat("Calculando st_intersects por bloques (PARALELO)...\n")
  t1 <- Sys.time()
  
  # con future_lapply paralelizamos bloques
  counts_list <- future_lapply(seq_along(idx_blocks), function(b) {
    idx <- idx_blocks[[b]]
    cat("  -> bloque", b, "/", length(idx_blocks),
        "| filas", min(idx), "-", max(idx), "\n")
    block_fun(idx)
  })
  
  counts <- unlist(counts_list, use.names = FALSE)
  
  t2 <- Sys.time()
  cat("Terminado intersects. Tiempo:",
      round(as.numeric(difftime(t2, t1, units="mins")), 2), "min\n")
  
  cat("Resumen counts:\n")
  cat("  min:", min(counts), "\n")
  cat("  max:", max(counts), "\n")
  cat("  mean:", round(mean(counts), 3), "\n")
  cat("  median:", median(counts), "\n")
  
  # agregar columna count
  seg$count <- as.integer(counts)
  
  # -------------------------
  # 4) Guardar output en GDB
  # -------------------------
  out_layer <- paste0(seg_layer, "_COUNT")
  cat("Guardando output:", out_layer, "\n")
  
  existing <- st_layers(gdb_path)$name
  if (out_layer %in% existing) {
    cat("  Output ya existe -> borrando...\n")
    st_delete(dsn = gdb_path, layer = out_layer)
  }
  
  st_write(seg, dsn = gdb_path, layer = out_layer, quiet = TRUE)
  
  t3 <- Sys.time()
  cat("LISTO ??? Año", year, "\n")
  cat("  Output:", out_layer, "\n")
  cat("  Tiempo total:",
      round(as.numeric(difftime(t3, t0, units="mins")), 2), "min\n")
  
  invisible(out_layer)
}

# -------------------------
# 5) Ejecutar 2017-2022
# -------------------------
out_layers <- character(0)

for (yr in names(pairs)) {
  out_layers <- c(out_layers, process_year_intersect(
    year       = yr,
    orig_layer = pairs[[yr]]$orig,
    seg_layer  = pairs[[yr]]$seg,
    gdb_path   = gdb,
    block_size = 2000      # puedes subir a 5000 si tienes buena RAM
  ))
}

cat("\n============================================================\n")
cat("TERMINADO ???\n")
cat("Layers creados:\n")
print(out_layers)
cat("============================================================\n")

# (opcional) volver a secuencial
plan(sequential)


# ============================================================
# CONFIDENCE POR CLASES DE PERCENTILES (por año)
# Percentiles: 10, 25, 50, 75, 95, 100
#
# Reglas:
#  < P10                   -> 10
#  [P10, < P25)             -> 25
#  [P25, < P50)             -> 50
#  [P50, < P75)             -> 75
#  [P75, < P95)             -> 95
#  >= P95                   -> 100
#
# SALIDA: SHAPEFILE (.shp) en:
# C:\Users\d.millanorduz\OneDrive - Universidad de los Andes\Diana_CESED\rutas\modeling_routes_CESED\05_post_processing\output
#
# Nombre: rutas_LCP_EPOF_YYYY.shp
# ============================================================

library(sf)
sf_use_s2(FALSE)

# -------------------------
# Entrada (GDB)
# -------------------------
gdb <- "C:/Users/d.millanorduz/Documents/ArcGIS/Projects/COCA_ROUTES/COCA_ROUTES.gdb"
years <- c("2017", "2018", "2019", "2020", "2021", "2022")

# -------------------------
# Salida (Shapefile folder)
# -------------------------
out_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/05_post_processing/output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (yr in years) {
  
  cat("\n====================\n")
  cat("Procesando + exportando SHP - Año", yr, "\n")
  cat("====================\n")
  
  layer_in <- paste0("rutas_LCP_", yr, "_escenar_Merge_ExportFeatures_COUNT")
  out_name <- paste0("rutas_LCP_EPOF_", yr)
  out_shp  <- file.path(out_dir, paste0(out_name, ".shp"))
  
  # Leer desde GDB
  x <- st_read(gdb, layer_in, quiet = TRUE)
  
  if (!"count" %in% names(x)) {
    stop("??? No existe la columna 'count' en ", layer_in)
  }
  
  # Calcular percentiles (por año)
  qs <- quantile(
    x$count,
    probs = c(0.10, 0.25, 0.50, 0.75, 0.95, 1.00),
    na.rm = TRUE,
    type = 7
  )
  
  p10 <- as.numeric(qs[1]); p25 <- as.numeric(qs[2]); p50 <- as.numeric(qs[3])
  p75 <- as.numeric(qs[4]); p95 <- as.numeric(qs[5]); p100 <- as.numeric(qs[6])
  
  cat("  P10 :", p10,  "\n")
  cat("  P25 :", p25,  "\n")
  cat("  P50 :", p50,  "\n")
  cat("  P75 :", p75,  "\n")
  cat("  P95 :", p95,  "\n")
  cat("  P100:", p100, "\n")
  
  # Asignar confidence según reglas
  v <- x$count
  x$confidence <- NA_integer_
  
  x$confidence[v <  p10]            <- 10L
  x$confidence[v >= p10 & v < p25]  <- 25L
  x$confidence[v >= p25 & v < p50]  <- 50L
  x$confidence[v >= p50 & v < p75]  <- 75L
  x$confidence[v >= p75 & v < p95]  <- 95L
  x$confidence[v >= p95]           <- 100L
  
  cat("  Distribución confidence:\n")
  print(table(x$confidence, useNA = "ifany"))
  
  # --- Limpiar nombres de columnas para SHP (máx 10 caracteres) ---
  # shapefile tiene límite duro de 10 caracteres; "confidence" -> "conf"
  # y aseguramos unicidad.
  names(x) <- make.names(names(x), unique = TRUE)
  names(x) <- substr(names(x), 1, 10)
  names(x)[names(x) == "confidence"] <- "conf"  # por si quedó exactamente
  
  # --- Borrar shapefile previo (si existe) ---
  # Shapefile son varios archivos: .shp .dbf .shx .prj .cpg ...
  base <- tools::file_path_sans_ext(out_shp)
  exts <- c(".shp",".dbf",".shx",".prj",".cpg",".qmd",".sbn",".sbx",".shp.xml")
  for (e in exts) {
    f <- paste0(base, e)
    if (file.exists(f)) file.remove(f)
  }
  
  # --- Exportar SHP ---
  cat("  Guardando SHP:\n   ", out_shp, "\n")
  st_write(x, dsn = out_shp, quiet = TRUE)
  
  cat("  LISTO ???", out_name, "\n")
}

cat("\nTERMINADO ??? Shapefiles exportados en:\n")
cat(out_dir, "\n")



# ============================================================
# 1) CARGAR SHAPEFILES YA EXPORTADOS (rutas_LCP_EPOF_YYYY.shp)
# 2) FILTRAR conf >= 25
# 3) EXPORTAR NUEVOS SHP: rutas_LCP_EPOF_YYYY_conf25.shp
# ============================================================

library(sf)
sf_use_s2(FALSE)

in_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/05_post_processing/output"

years <- c("2017","2018","2019","2020","2021","2022")

# helper: borrar shapefile completo (todas las extensiones)
delete_shapefile <- function(shp_path) {
  base <- tools::file_path_sans_ext(shp_path)
  exts <- c(".shp",".dbf",".shx",".prj",".cpg",".qmd",".sbn",".sbx",".shp.xml")
  for (e in exts) {
    f <- paste0(base, e)
    if (file.exists(f)) file.remove(f)
  }
}

for (yr in years) {
  
  cat("\n====================\n")
  cat("AÑO:", yr, "\n")
  cat("====================\n")
  
  in_name <- paste0("rutas_LCP_EPOF_", yr, ".shp")
  in_shp  <- file.path(in_dir, in_name)
  
  if (!file.exists(in_shp)) {
    cat("  ??? No existe:", in_shp, "\n")
    next
  }
  
  cat("  Leyendo:", in_name, "\n")
  x <- st_read(in_shp, quiet = TRUE)
  
  # Detectar columna de confianza (por si quedó como conf o confidence u otra truncada)
  cand <- c("conf", "confidence", "CONF", "Confidence")
  conf_col <- cand[cand %in% names(x)][1]
  
  if (is.na(conf_col) || is.null(conf_col) || conf_col == "") {
    stop("??? No encuentro columna de confianza en ", in_name,
         "\nColumnas disponibles: ", paste(names(x), collapse = ", "))
  }
  
  cat("  Columna confianza:", conf_col, "\n")
  
  # Filtrar >= 25
  x_high <- x[x[[conf_col]] >= 25, ]
  
  cat("  n_total:", nrow(x), "\n")
  cat("  n_conf>=25:", nrow(x_high), "\n")
  
  out_name <- paste0("rutas_LCP_EPOF_", yr, "_conf25.shp")
  out_shp  <- file.path(in_dir, out_name)
  
  # Borrar si existe
  if (file.exists(out_shp)) delete_shapefile(out_shp)
  
  cat("  Guardando:", out_name, "\n")
  st_write(x_high, dsn = out_shp, quiet = TRUE)
  
  cat("  LISTO ???", out_name, "\n")
}

cat("\nTERMINADO ???\n")
