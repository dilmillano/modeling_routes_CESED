# ============================================================
# YEAR PRESENCE por segmento (igual que count, pero guardando años)
# - Input (GDB):
#   * rutas_LCP_EPOF_Merge_conf25            (tiene MERGE_SRC con año embebido)
#   * rutas_LCP_EPOF_Merg_conf25_Dissolve1   (segmentos únicos / planarize)
# - Output (GDB):
#   * rutas_LCP_EPOF_conf25_year_presence    (añade years, n_years)
#
# Columnas de salida:
#   - years   (TEXT): "2017,2019,2022"
#   - n_years (INT) : 3
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(future.apply)
})

sf_use_s2(FALSE)

# ----------------------------
# 0) Config
# ----------------------------
gdb <- "C:/Users/d.millanorduz/Documents/ArcGIS/Projects/COCA_ROUTES/COCA_ROUTES.gdb"

layer_merge    <- "rutas_LCP_EPOF_Merge_conf25"
layer_segments <- "rutas_LCP_EPOF_Merg_conf25_Dissolve1"
layer_out      <- "rutas_LCP_EPOF_conf25_year_presence"

# Paralelización / bloques
n_workers  <- max(1, parallel::detectCores() - 1)
block_size <- 2000L

# ----------------------------
# 1) Leer capas
# ----------------------------
cat("\n====================\n")
cat("Leyendo capas...\n")
cat("====================\n")

routes <- st_read(gdb, layer_merge, quiet = TRUE)
segs   <- st_read(gdb, layer_segments, quiet = TRUE)

cat("n_routes:", nrow(routes), "\n")
cat("n_segs  :", nrow(segs), "\n")

cat("\nColumnas routes:\n"); print(names(routes))
cat("\nColumnas segs:\n"); print(names(segs))

# ----------------------------
# 2) Validaciones + construir year desde MERGE_SRC
# ----------------------------
if (!"MERGE_SRC" %in% names(routes)) {
  stop("??? El layer merge NO tiene columna 'MERGE_SRC'. Revisa la tabla del merge.")
}

# Extraer año (4 dígitos) desde MERGE_SRC (ej: rutas_LCP_EPOF_2017_conf25)
routes$year <- suppressWarnings(as.integer(sub(".*?(\\d{4}).*", "\\1", routes$MERGE_SRC)))

# Chequeo de extracción
bad <- is.na(routes$year)
if (any(bad)) {
  cat("?????? Filas con year NA (MERGE_SRC no coincide con patrón YYYY). Ejemplos:\n")
  print(unique(routes$MERGE_SRC[bad])[1:min(10, length(unique(routes$MERGE_SRC[bad])))])
  stop("??? No pude extraer el año desde MERGE_SRC en algunas filas. Revisa esos valores.")
}

cat("\nAños detectados en MERGE_SRC:\n")
print(sort(unique(routes$year)))

# CRS consistente
cat("\nCRS routes:", st_crs(routes)$epsg, "\n")
cat("CRS segs  :", st_crs(segs)$epsg, "\n")

if (st_crs(routes) != st_crs(segs)) {
  cat("?????? CRS distinto: transformando routes -> CRS de segs\n")
  routes <- st_transform(routes, st_crs(segs))
}

# Quitar geometrías vacías por seguridad
routes <- routes[!st_is_empty(routes), ]
segs   <- segs[!st_is_empty(segs), ]

# (Opcional) Make valid por seguridad
cat("\nChequeando geometrías inválidas...\n")
inv_r <- sum(!st_is_valid(routes))
inv_s <- sum(!st_is_valid(segs))
cat("  inválidas routes:", inv_r, "\n")
cat("  inválidas segs  :", inv_s, "\n")

if (inv_r > 0) routes <- st_make_valid(routes)
if (inv_s > 0) segs   <- st_make_valid(segs)

# ----------------------------
# 3) Preparar paralelización
# ----------------------------
plan(multisession, workers = n_workers)

idx <- seq_len(nrow(segs))
blocks <- split(idx, ceiling(idx / block_size))

cat("\n====================\n")
cat("Paralelización\n")
cat("====================\n")
cat("workers   :", n_workers, "\n")
cat("block_size:", block_size, "\n")
cat("bloques   :", length(blocks), "\n\n")

# ----------------------------
# 4) Función: años por bloque (overlap lineal real: longitud > 0)
# ----------------------------
years_for_block <- function(seg_ids, segs, routes) {
  
  seg_block <- segs[seg_ids, , drop = FALSE]
  
  # Candidatos por bbox/intersects (rápido)
  cand_list <- st_intersects(seg_block, routes)
  
  years_txt <- character(length(seg_ids))
  n_years   <- integer(length(seg_ids))
  
  for (i in seq_along(seg_ids)) {
    
    cand <- cand_list[[i]]
    
    if (length(cand) == 0) {
      years_txt[i] <- NA_character_
      n_years[i]   <- 0L
      next
    }
    
    s1 <- seg_block[i, , drop = FALSE]
    
    # Intersección geométrica (puede producir puntos o líneas)
    inter <- suppressWarnings(st_intersection(routes[cand, c("year")], s1))
    
    if (nrow(inter) == 0) {
      years_txt[i] <- NA_character_
      n_years[i]   <- 0L
      next
    }
    
    # Filtrar solo intersecciones LINEALES (longitud > 0)
    L <- as.numeric(st_length(inter))
    inter <- inter[!is.na(L) & L > 0, , drop = FALSE]
    
    if (nrow(inter) == 0) {
      years_txt[i] <- NA_character_
      n_years[i]   <- 0L
      next
    }
    
    yrs <- sort(unique(as.integer(inter$year)))
    years_txt[i] <- paste(yrs, collapse = ",")
    n_years[i]   <- length(yrs)
  }
  
  data.frame(
    row_id  = seg_ids,
    years   = years_txt,
    n_years = n_years,
    stringsAsFactors = FALSE
  )
}

# ----------------------------
# 5) Ejecutar en paralelo por bloques
# ----------------------------
cat("Calculando years/n_years por segmento...\n\n")

res_list <- future_lapply(seq_along(blocks), function(b) {
  seg_ids <- blocks[[b]]
  cat("  Bloque", b, "de", length(blocks), "| n =", length(seg_ids), "\n")
  years_for_block(seg_ids, segs, routes)
})

res <- do.call(rbind, res_list)

# ----------------------------
# 6) Integrar resultados al planarize
# ----------------------------
# Crear columnas nuevas en segs
segs$years   <- NA_character_
segs$n_years <- 0L

segs$years[res$row_id]   <- res$years
segs$n_years[res$row_id] <- res$n_years

cat("\n====================\n")
cat("Resumen n_years\n")
cat("====================\n")
print(table(segs$n_years, useNA = "ifany"))

cat("\nEjemplos years (no NA):\n")
print(head(na.omit(segs$years), 15))

# ----------------------------
# 7) Guardar en la MISMA GDB
# ----------------------------
existing <- st_layers(gdb)$name
if (layer_out %in% existing) {
  cat("\nLayer existe -> borrando:", layer_out, "\n")
  st_delete(dsn = gdb, layer = layer_out)
}

cat("\nEscribiendo salida:", layer_out, "\n")
st_write(segs, dsn = gdb, layer = layer_out, quiet = TRUE)

cat("\n====================\n")
cat("LISTO ???\n")
cat("====================\n")
cat("GDB   :", gdb, "\n")
cat("OUT   :", layer_out, "\n")
cat("n_segs:", nrow(segs), "\n")






# ============================================================
# Agregar columna "category" (SIN categorías extra)
# - Persistent: n_years == 6
# - Regular   : n_years %in% c(4,5)
# - n_years %in% c(2,3): Emergence vs Decline
#   * Emergence: mayoría de años en 2019-2022
#   * Decline  : mayoría de años en 2017-2019
#   * Si hay empate (ej. 1 y 1), se decide por el promedio:
#       mean(years) >= 2019 -> Emergence, si no -> Decline
#
# Input GDB:
#   rutas_LCP_EPOF_conf25_year_presence
# Output GDB:
#   rutas_LCP_EPOF_conf25_year_presence_CAT
# ============================================================

library(sf)

gdb <- "C:/Users/d.millanorduz/Documents/ArcGIS/Projects/COCA_ROUTES/COCA_ROUTES.gdb"
layer_in  <- "rutas_LCP_EPOF_conf25_year_presence"
layer_out <- "rutas_LCP_EPOF_conf25_year_presence_CAT"

cat("Leyendo:", layer_in, "\n")
x <- st_read(gdb, layer_in, quiet = TRUE)

# ---- Validaciones
need_cols <- c("years", "n_years")
if (!all(need_cols %in% names(x))) {
  stop("??? Faltan columnas requeridas: ", paste(setdiff(need_cols, names(x)), collapse = ", "))
}

# ---- Asegurar tipo entero (esto arregla el bug de Persistent)
x$n_years <- suppressWarnings(as.integer(x$n_years))

# Helper: parse "2017,2019,2022" -> c(2017,2019,2022)
parse_years <- function(s) {
  if (is.na(s) || !nzchar(s)) return(integer(0))
  as.integer(trimws(strsplit(s, ",", fixed = TRUE)[[1]]))
}

# ---- Inicializar category
x$category <- NA_character_

# ---- Reglas directas
x$category[!is.na(x$n_years) & x$n_years == 6] <- "Persistent"
x$category[!is.na(x$n_years) & x$n_years %in% c(4,5)] <- "Regular"

# ---- Reglas para n_years 2 o 3 (DEBE cubrir TODO)
idx <- which(!is.na(x$n_years) & x$n_years %in% c(2,3))

if (length(idx) > 0) {
  
  yrs_list <- lapply(x$years[idx], parse_years)
  
  # Conteos por ventanas (nota: 2019 está en ambas ventanas como tú lo definiste)
  early_ct <- vapply(yrs_list, function(v) sum(v >= 2017 & v <= 2019), integer(1))
  late_ct  <- vapply(yrs_list, function(v) sum(v >= 2019 & v <= 2022), integer(1))
  mean_yr  <- vapply(yrs_list, function(v) if (length(v) == 0) NA_real_ else mean(v), numeric(1))
  
  # 1) Mayoría late -> Emergence
  sel_emerg <- idx[late_ct > early_ct]
  if (length(sel_emerg) > 0) x$category[sel_emerg] <- "Emergence"
  
  # 2) Mayoría early -> Decline
  sel_decl <- idx[early_ct > late_ct]
  if (length(sel_decl) > 0) x$category[sel_decl] <- "Decline"
  
  # 3) Empates -> usar promedio del año como desempate (esto garantiza cobertura total)
  tie <- idx[late_ct == early_ct]
  if (length(tie) > 0) {
    # mean(years) >= 2019 -> Emergence, else Decline
    x$category[tie[mean_yr[late_ct == early_ct] >= 2019]] <- "Emergence"
    x$category[tie[mean_yr[late_ct == early_ct] <  2019]] <- "Decline"
  }
}

# ---- Chequeos clave
cat("\nChequeo Persistent (n_years==6):\n")
print(table(x$category[x$n_years == 6], useNA = "ifany"))

cat("\nChequeo Regular (n_years in 4,5):\n")
print(table(x$category[x$n_years %in% c(4,5)], useNA = "ifany"))

cat("\nChequeo n_years 2 o 3 (NO debe haber NA):\n")
print(table(x$category[x$n_years %in% c(2,3)], useNA = "ifany"))

na_23 <- sum(is.na(x$category) & x$n_years %in% c(2,3))
cat("NA en (2 o 3 años):", na_23, "\n")
if (na_23 > 0) stop("??? Quedaron NA en n_years 2 o 3 y eso no debe pasar.")

cat("\nDistribución general category:\n")
print(table(x$category, useNA = "ifany"))

# ---- Guardar en la misma GDB
existing <- st_layers(gdb)$name
if (layer_out %in% existing) {
  cat("\nLayer existe -> borrando:", layer_out, "\n")
  st_delete(dsn = gdb, layer = layer_out)
}

st_write(x, dsn = gdb, layer = layer_out, quiet = TRUE)

cat("\nLISTO ??? Guardado:\n")
cat("GDB :", gdb, "\n")
cat("OUT :", layer_out, "\n")
