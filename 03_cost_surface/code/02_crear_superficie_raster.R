# -----------------------------------------------------------
# CREACIÓN DE RÁSTER CATEGÓRICO 1 km x 1 km CON PRIORIDAD:
# 1 = Ríos
# 2 = Vías primarias (Colombia + OSM)
# 3 = Vías secundarias (Colombia + OSM)
# 4 = Vías terciarias (Colombia + OSM)
# 5 = Caminos
# 6 = Cuerpos de agua (sobreescribe cualquier valor 1-5)
# 7 = Batallones (sobreescribe incluso cuerpos de agua)
# 8 = Puertos (sobreescribe TODO lo anterior)
# -----------------------------------------------------------

library(terra)

# -----------------------------------------------------------
# 1. Definir rutas de entrada y salida
# -----------------------------------------------------------

# Área de trabajo (extent)
shp_area <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/00_area/extent_area.shp"

# Ríos
shp_rios_igac  <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/drenaje_doble_IGAC.shp"
shp_rios_hydro <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/hydrorivers.shp"

# Vías Colombia
shp_vias_prim_col   <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/vias_primarias.shp"
shp_vias_sec_col    <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/vias_secundarias.shp"
shp_vias_ter_col    <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/vias_terciarias.shp"
shp_vias_camino     <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/vias_caminos.shp"

# Vías OSM (otros países / complementarias)
shp_vias_prim_osm <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/highway_primary_osm.shp"
shp_vias_sec_osm  <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/highway_secondary_osm.shp"
shp_vias_ter_osm  <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/highway_tertiary_osm.shp"

# Nuevas capas
shp_cuerpos_agua <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/cuerpos_agua.shp"
shp_batallones   <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/batallones.shp"
shp_puertos      <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/puertos_osm.shp"

# Carpeta de salida
output_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Rutas de salida de rásteres intermedios
r_rios_igac_out       <- file.path(output_dir, "r_rios_igac_1km.tif")
r_rios_hydro_out      <- file.path(output_dir, "r_rios_hydrorivers_1km.tif")
r_rios_merged_out     <- file.path(output_dir, "r_rios_merged_1km.tif")

r_vias_prim_col_out   <- file.path(output_dir, "r_vias_primarias_col_1km.tif")
r_vias_prim_osm_out   <- file.path(output_dir, "r_vias_primarias_osm_1km.tif")
r_vias_prim_all_out   <- file.path(output_dir, "r_vias_primarias_all_1km.tif")

r_vias_sec_col_out    <- file.path(output_dir, "r_vias_secundarias_col_1km.tif")
r_vias_sec_osm_out    <- file.path(output_dir, "r_vias_secundarias_osm_1km.tif")
r_vias_sec_all_out    <- file.path(output_dir, "r_vias_secundarias_all_1km.tif")

r_vias_ter_col_out    <- file.path(output_dir, "r_vias_terciarias_col_1km.tif")
r_vias_ter_osm_out    <- file.path(output_dir, "r_vias_terciarias_osm_1km.tif")
r_vias_ter_all_out    <- file.path(output_dir, "r_vias_terciarias_all_1km.tif")

r_vias_camino_out     <- file.path(output_dir, "r_vias_caminos_1km.tif")

r_cuerpos_agua_out    <- file.path(output_dir, "r_cuerpos_agua_1km.tif")
r_batallones_out      <- file.path(output_dir, "r_batallones_1km.tif")
r_puertos_out         <- file.path(output_dir, "r_puertos_1km.tif")

# Ráster final y leyenda
raster_salida <- file.path(output_dir, "cost_surface_1km.tif")
legend_file   <- file.path(output_dir, "cost_surface_1km_legend.txt")

# -----------------------------------------------------------
# 2. Cargar área y crear raster plantilla de 1 km x 1 km
# -----------------------------------------------------------

area <- vect(shp_area)

# Resolución 1 km (asumiendo CRS en metros)
res_km <- 1000

r_template <- rast(
  ext(area),
  resolution = res_km,
  crs = crs(area)
)

values(r_template) <- NA

# -----------------------------------------------------------
# 3. Función auxiliar para rasterizar con un valor constante
# -----------------------------------------------------------

burn_constant <- function(vect_layer, template_rast, value_const) {
  v_tmp <- vect_layer
  v_tmp$.__burn__ <- value_const
  
  r_out <- rasterize(
    v_tmp,
    template_rast,
    field      = ".__burn__",
    touches    = TRUE,   # marca celdas que toca la geometría
    background = NA
  )
  
  return(r_out)
}

# -----------------------------------------------------------
# 4. Cargar capas vectoriales y proyectar al CRS del área
# -----------------------------------------------------------

# Ríos
rios_igac   <- vect(shp_rios_igac)
rios_hydro  <- vect(shp_rios_hydro)

# Vías Colombia
vias_prim_col   <- vect(shp_vias_prim_col)
vias_sec_col    <- vect(shp_vias_sec_col)
vias_ter_col    <- vect(shp_vias_ter_col)
vias_camino     <- vect(shp_vias_camino)

# Vías OSM
vias_prim_osm <- vect(shp_vias_prim_osm)
vias_sec_osm  <- vect(shp_vias_sec_osm)
vias_ter_osm  <- vect(shp_vias_ter_osm)

# Otras capas
cuerpos_agua <- vect(shp_cuerpos_agua)
batallones   <- vect(shp_batallones)
puertos      <- vect(shp_puertos)

# Proyectar todo al CRS del área
rios_igac       <- project(rios_igac,       crs(area))
rios_hydro      <- project(rios_hydro,      crs(area))
vias_prim_col   <- project(vias_prim_col,   crs(area))
vias_sec_col    <- project(vias_sec_col,    crs(area))
vias_ter_col    <- project(vias_ter_col,    crs(area))
vias_camino     <- project(vias_camino,     crs(area))
vias_prim_osm   <- project(vias_prim_osm,   crs(area))
vias_sec_osm    <- project(vias_sec_osm,    crs(area))
vias_ter_osm    <- project(vias_ter_osm,    crs(area))
cuerpos_agua    <- project(cuerpos_agua,    crs(area))
batallones      <- project(batallones,      crs(area))
puertos         <- project(puertos,         crs(area))

# -----------------------------------------------------------
# 5. Rasterizar cada capa con su valor base (1-5)
# -----------------------------------------------------------

# --- RÍOS (valor = 1) ---
r_rios_igac  <- burn_constant(rios_igac,  r_template, value_const = 1)
r_rios_hydro <- burn_constant(rios_hydro, r_template, value_const = 1)

writeRaster(r_rios_igac,  r_rios_igac_out,  overwrite = TRUE)
writeRaster(r_rios_hydro, r_rios_hydro_out, overwrite = TRUE)

# Unir ríos
r_rios <- app(c(r_rios_igac, r_rios_hydro), fun = min, na.rm = TRUE)
writeRaster(r_rios, r_rios_merged_out, overwrite = TRUE)

# --- VÍAS PRIMARIAS (2) Colombia + OSM ---
r_vias_prim_col <- burn_constant(vias_prim_col, r_template, value_const = 2)
r_vias_prim_osm <- burn_constant(vias_prim_osm, r_template, value_const = 2)

writeRaster(r_vias_prim_col, r_vias_prim_col_out, overwrite = TRUE)
writeRaster(r_vias_prim_osm, r_vias_prim_osm_out, overwrite = TRUE)

# Combinar primarias
r_vias_prim_all <- app(c(r_vias_prim_col, r_vias_prim_osm), fun = min, na.rm = TRUE)
writeRaster(r_vias_prim_all, r_vias_prim_all_out, overwrite = TRUE)

# --- VÍAS SECUNDARIAS (3) Colombia + OSM ---
r_vias_sec_col <- burn_constant(vias_sec_col, r_template, value_const = 3)
r_vias_sec_osm <- burn_constant(vias_sec_osm, r_template, value_const = 3)

writeRaster(r_vias_sec_col, r_vias_sec_col_out, overwrite = TRUE)
writeRaster(r_vias_sec_osm, r_vias_sec_osm_out, overwrite = TRUE)

r_vias_sec_all <- app(c(r_vias_sec_col, r_vias_sec_osm), fun = min, na.rm = TRUE)
writeRaster(r_vias_sec_all, r_vias_sec_all_out, overwrite = TRUE)

# --- VÍAS TERCIARIAS (4) Colombia + OSM ---
r_vias_ter_col <- burn_constant(vias_ter_col, r_template, value_const = 4)
r_vias_ter_osm <- burn_constant(vias_ter_osm, r_template, value_const = 4)

writeRaster(r_vias_ter_col, r_vias_ter_col_out, overwrite = TRUE)
writeRaster(r_vias_ter_osm, r_vias_ter_osm_out, overwrite = TRUE)

r_vias_ter_all <- app(c(r_vias_ter_col, r_vias_ter_osm), fun = min, na.rm = TRUE)
writeRaster(r_vias_ter_all, r_vias_ter_all_out, overwrite = TRUE)

# --- CAMINOS (5) ---
r_vias_camino <- burn_constant(vias_camino, r_template, value_const = 5)
writeRaster(r_vias_camino, r_vias_camino_out, overwrite = TRUE)

# -----------------------------------------------------------
# 6. Combinar base 1-5 respetando prioridad (1 más alta)
# -----------------------------------------------------------

stack_layers_base <- c(
  r_rios,
  r_vias_prim_all,
  r_vias_sec_all,
  r_vias_ter_all,
  r_vias_camino
)

r_final <- app(stack_layers_base, fun = min, na.rm = TRUE)
r_final <- mask(r_final, area)

# -----------------------------------------------------------
# 7. Cuerpos de agua (6) y batallones (7): sobreescribir
# -----------------------------------------------------------

# Cuerpos de agua (6)
r_cuerpos_agua <- burn_constant(cuerpos_agua, r_template, value_const = 6)
writeRaster(r_cuerpos_agua, r_cuerpos_agua_out, overwrite = TRUE)

idx_agua <- !is.na(r_cuerpos_agua)
r_final[idx_agua] <- 6

# Batallones (7)
r_batallones <- burn_constant(batallones, r_template, value_const = 7)
writeRaster(r_batallones, r_batallones_out, overwrite = TRUE)

idx_bat <- !is.na(r_batallones)
r_final[idx_bat] <- 7

# -----------------------------------------------------------
# 8. Puertos (8): prioridad máxima, sobreescribe TODO
# -----------------------------------------------------------

r_puertos <- burn_constant(puertos, r_template, value_const = 8)
writeRaster(r_puertos, r_puertos_out, overwrite = TRUE)

idx_puerto <- !is.na(r_puertos)
r_final[idx_puerto] <- 8

# Recortar nuevamente por seguridad
r_final <- mask(r_final, area)

# -----------------------------------------------------------
# 9. Guardar raster final y crear archivo de texto con la explicación
# -----------------------------------------------------------

writeRaster(
  r_final,
  raster_salida,
  overwrite = TRUE,
  datatype = "INT1U"
)

legend_lines <- c(
  "Leyenda del raster combinado: cost_surface_1km.tif",
  "",
  "Valores de la superficie categórica:",
  "1 = Ríos (drenaje_doble_IGAC.shp + hydrorivers.shp)",
  "2 = Vías primarias (vias_primarias.shp + highway_primary_osm.shp)",
  "3 = Vías secundarias (vias_secundarias.shp + highway_secondary_osm.shp)",
  "4 = Vías terciarias (vias_terciarias.shp + highway_tertiary_osm.shp)",
  "5 = Caminos (vias_caminos.shp)",
  "6 = Cuerpos de agua (cuerpos_agua.shp) - prioridad sobre 1-5",
  "7 = Batallones (batallones.shp) - prioridad sobre 1-6",
  "8 = Puertos (puertos_osm.shp) - prioridad máxima (sobrescribe todo)",
  "",
  "Lógica de prioridad:",
  "- Se construye una superficie base con 1-5 usando el valor mínimo por píxel.",
  "- Luego se sobreescriben las celdas de cuerpos de agua con valor 6.",
  "- Después se sobreescriben las celdas de batallones con valor 7.",
  "- Finalmente se sobreescriben las celdas de puertos con valor 8 (prioridad máxima)."
)

writeLines(legend_lines, con = legend_file)

# -----------------------------------------------------------
# 10. Plot rápido para revisar (opcional)
# -----------------------------------------------------------

plot(
  r_final,
  main = "Superficie categórica 1 km\n1=ríos, 2=primarias, 3=secundarias, 4=terciarias, 5=caminos, 6=agua, 7=batallones, 8=puertos"
)


# -----------------------------------------------------------
# CREAR VERSIÓN "FONDO" DONDE LOS NA SE CONVIERTEN EN 0
# -----------------------------------------------------------

r_fondo <- r_final   # copiar raster final

# Convertir NA a 0
r_fondo[is.na(r_fondo)] <- 0

# Ruta de salida del raster fondo
raster_fondo <- file.path(output_dir, "cost_surface_1km_fondo.tif")

# Guardar raster fondo
writeRaster(
  r_fondo,
  raster_fondo,
  overwrite = TRUE,
  datatype = "INT1U"
)

# Mensaje en consola
print("Raster 'cost_surface_1km_fondo.tif' creado con NA reemplazados por 0.")
