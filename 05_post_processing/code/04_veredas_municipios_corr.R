# ============================================================
# Categorizar VEREDAS y MUNICIPIOS según presencia de rutas:
#  - "both"        : toca rutas modeladas y cualitativas
#  - "modeled"     : solo toca rutas modeladas
#  - "qualitative" : solo toca rutas cualitativas
#  - "no_routes"   : no toca ninguna
#
# NUEVA VERSIÓN (febrero_2026):
#   - rutas modeladas: 06_results/febrero_2026/rutas_LCP_EPOF_merge_conf25.shp
#   - salidas:        06_results/febrero_2026/  (sin subcarpetas)
# ============================================================

suppressPackageStartupMessages({
  library(sf)
})

sf_use_s2(FALSE)

# ----------------------------
# Rutas de entrada (las mismas)
# ----------------------------
veredas_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/data/shp_infosecundaria/SHP_CRVEREDAS_2020/CRVeredas_2020.shp"
mpios_path   <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/data/shp_infosecundaria/SHP_MGN2018_INTGRD_MPIO/MPIOS_limpio.shp"

# SOLO cambia esto (modeled) + directorio de salida
modeled_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results/febrero_2026/rutas_LCP_EPOF_merge_conf25.shp"

# Cualitativas: igual que antes
qual_path    <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/documentacion/RUTAS/Process_routes/1.Data/qualitative_routes.shp"

# Salidas: misma carpeta de febrero_2026
out_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results/febrero_2026"

out_veredas <- file.path(out_dir, "veredas_routes_category.shp")
out_mpios   <- file.path(out_dir, "municipios_routes_category.shp")

# ----------------------------
# 1) Cargar datos
# ----------------------------
cat("Leyendo veredas...\n")
veredas <- st_read(veredas_path, quiet = TRUE)

cat("Leyendo municipios...\n")
mpios <- st_read(mpios_path, quiet = TRUE)

cat("Leyendo rutas modeladas...\n")
modeled <- st_read(modeled_path, quiet = TRUE)

cat("Leyendo rutas cualitativas...\n")
qual <- st_read(qual_path, quiet = TRUE)

cat("\nCRS:\n")
cat("  veredas :", st_crs(veredas)$epsg, "\n")
cat("  mpios   :", st_crs(mpios)$epsg, "\n")
cat("  modeled :", st_crs(modeled)$epsg, "\n")
cat("  qual    :", st_crs(qual)$epsg, "\n")

# ----------------------------
# 2) Asegurar CRS consistente (para cada capa de polígonos)
# ----------------------------
modeled_v <- if (st_crs(modeled) != st_crs(veredas)) st_transform(modeled, st_crs(veredas)) else modeled
qual_v    <- if (st_crs(qual)    != st_crs(veredas)) st_transform(qual,    st_crs(veredas)) else qual

modeled_m <- if (st_crs(modeled) != st_crs(mpios))   st_transform(modeled, st_crs(mpios))   else modeled
qual_m    <- if (st_crs(qual)    != st_crs(mpios))   st_transform(qual,    st_crs(mpios))   else qual

# ----------------------------
# 3) Función para categorizar por intersección
# ----------------------------
categorize_polygons <- function(polys, modeled_lines, qual_lines) {
  # TRUE si existe al menos 1 intersección con alguna ruta
  has_modeled <- lengths(st_intersects(polys, modeled_lines)) > 0
  has_qual    <- lengths(st_intersects(polys, qual_lines)) > 0
  
  category <- ifelse(has_modeled & has_qual, "both",
                     ifelse(has_modeled & !has_qual, "modeled",
                            ifelse(!has_modeled & has_qual, "qualitative",
                                   "no_routes")))
  
  polys$category <- category
  polys
}

# ----------------------------
# 4) Categorizar veredas y municipios
# ----------------------------
cat("\nCategorizando veredas...\n")
veredas_out <- categorize_polygons(veredas, modeled_v, qual_v)
cat("Distribución veredas:\n")
print(table(veredas_out$category, useNA = "ifany"))

cat("\nCategorizando municipios...\n")
mpios_out <- categorize_polygons(mpios, modeled_m, qual_m)
cat("Distribución municipios:\n")
print(table(mpios_out$category, useNA = "ifany"))

# ----------------------------
# 5) Exportar shapefiles (sobrescribe si existen)
# ----------------------------
# Nota: st_delete sobre shapefile puede fallar si hay locks; borramos por patrón
delete_shp_bundle <- function(shp_path) {
  base <- sub("\\.shp$", "", shp_path, ignore.case = TRUE)
  exts <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".qpj", ".sbn", ".sbx", ".xml")
  for (e in exts) {
    f <- paste0(base, e)
    if (file.exists(f)) file.remove(f)
  }
}

if (file.exists(out_veredas)) {
  cat("\nBorrando shapefile existente veredas...\n")
  delete_shp_bundle(out_veredas)
}
if (file.exists(out_mpios)) {
  cat("\nBorrando shapefile existente municipios...\n")
  delete_shp_bundle(out_mpios)
}

cat("\nEscribiendo:", out_veredas, "\n")
st_write(veredas_out, out_veredas, quiet = TRUE)

cat("Escribiendo:", out_mpios, "\n")
st_write(mpios_out, out_mpios, quiet = TRUE)

cat("\nLISTO ???\n")
cat("  Veredas    :", out_veredas, "\n")
cat("  Municipios :", out_mpios, "\n")



# ============================================================
# Resumen de categorías para VEREDAS y MUNICIPIOS
# (conteos impresos en consola) - usando las salidas en febrero_2026
# ============================================================

suppressPackageStartupMessages({
  library(sf)
})

out_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results/febrero_2026"

veredas_shp <- file.path(out_dir, "veredas_routes_category.shp")
mpios_shp   <- file.path(out_dir, "municipios_routes_category.shp")

print_summary <- function(x, label) {
  cat("\n=============================\n")
  cat("RESUMEN:", label, "\n")
  cat("=============================\n")
  cat("Total:", nrow(x), "\n\n")
  print(table(x$category, useNA = "ifany"))
  cat("\n")
}

cat("\nLeyendo veredas (salida)...\n")
veredas2 <- st_read(veredas_shp, quiet = TRUE)

cat("Leyendo municipios (salida)...\n")
mpios2 <- st_read(mpios_shp, quiet = TRUE)

print_summary(veredas2, "VEREDAS")
print_summary(mpios2, "MUNICIPIOS")

cat("LISTO ??? Resumen impreso en consola\n")
