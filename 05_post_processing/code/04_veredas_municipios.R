# ============================================================
# Categorizar VEREDAS y MUNICIPIOS según presencia de rutas:
#  - "both"        : toca rutas modeladas y cualitativas
#  - "modeled"     : solo toca rutas modeladas
#  - "qualitative" : solo toca rutas cualitativas
#  - "no_routes"   : no toca ninguna
#
# Salida (shapefiles) en:
#   C:\Users\d.millanorduz\OneDrive - Universidad de los Andes\Diana_CESED\rutas\modeling_routes_CESED\06_results
# ============================================================

suppressPackageStartupMessages({
  library(sf)
})

sf_use_s2(FALSE)

# ----------------------------
# Rutas de entrada
# ----------------------------
veredas_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/data/shp_infosecundaria/SHP_CRVEREDAS_2020/CRVeredas_2020.shp"
mpios_path   <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/data/shp_infosecundaria/SHP_MGN2018_INTGRD_MPIO/MPIOS_limpio.shp"

modeled_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results/shp/rutas_LCP_EPOF_merge_conf25.shp"
qual_path    <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/documentacion/RUTAS/Process_routes/1.Data/qualitative_routes.shp"

out_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results"

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
if (file.exists(out_veredas)) {
  cat("\nBorrando shapefile existente veredas...\n")
  st_delete(out_veredas, quiet = TRUE)
}
if (file.exists(out_mpios)) {
  cat("\nBorrando shapefile existente municipios...\n")
  st_delete(out_mpios, quiet = TRUE)
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
# (conteos impresos en consola)
# ============================================================

library(sf)

# ----------------------------
# Rutas de salida ya generadas
# ----------------------------
out_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results/shp"

veredas_shp <- file.path(out_dir, "veredas_routes_category.shp")
mpios_shp   <- file.path(out_dir, "municipios_routes_category.shp")

# ----------------------------
# Función de resumen
# ----------------------------
print_summary <- function(x, label) {
  
  cat("\n=============================\n")
  cat("RESUMEN:", label, "\n")
  cat("=============================\n")
  
  cat("Total:", nrow(x), "\n\n")
  
  tab <- table(x$category, useNA = "ifany")
  print(tab)
  
  cat("\n")
}

# ----------------------------
# Leer shapefiles
# ----------------------------
cat("Leyendo veredas...\n")
veredas <- st_read(veredas_shp, quiet = TRUE)

cat("Leyendo municipios...\n")
mpios <- st_read(mpios_shp, quiet = TRUE)

# ----------------------------
# Imprimir resúmenes
# ----------------------------
print_summary(veredas, "VEREDAS")
print_summary(mpios, "MUNICIPIOS")

cat("LISTO ??? Resumen impreso en consola\n")

