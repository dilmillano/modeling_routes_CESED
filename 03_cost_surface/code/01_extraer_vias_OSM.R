############################################################
# 0. Cargar paquetes
############################################################

library(osmextract)
library(sf)
library(dplyr)

############################################################
# 1. Rutas a los archivos .pbf
############################################################

pbf_files <- c(
  "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/ecuador-251112.osm.pbf",
  "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/peru-251112.osm.pbf",
  "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/brazil-251112.osm.pbf",
  "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/venezuela-251112.osm.pbf"
)

############################################################
# 2. Función para leer solo las vías (highway)
############################################################

leer_vias <- function(pbf_path) {
  message("Leyendo: ", pbf_path)
  
  vias <- oe_read(
    pbf_path,
    layer = "lines",
    query = "SELECT * FROM lines WHERE highway IS NOT NULL"
  )
  
  vias <- st_make_valid(vias)
  
  # Seleccionar solo columnas si existen
  vias %>%
    dplyr::select(
      dplyr::any_of(c("highway", "name", "ref", "surface", "maxspeed")),
      dplyr::everything()
    )
}

############################################################
# 3. Leer todos los PBF y unirlos
############################################################

lista_vias <- lapply(pbf_files, leer_vias)

highways_union <- do.call(rbind, lista_vias)

# Asegurar WGS84 como CRS inicial
highways_union <- st_transform(highways_union, 4326)

############################################################
# 4. Hacer buffer de 30 m (en CRS métrico)
############################################################

# Pasar a Web Mercator (metros)
highways_3857 <- st_transform(highways_union, 3857)

# Buffer de 30 metros
highways_buffer_3857 <- st_buffer(highways_3857, dist = 30)

# Arreglar geometrías
highways_buffer_3857 <- st_make_valid(highways_buffer_3857)

# Volver a WGS84
highways_buffer_4326 <- st_transform(highways_buffer_3857, 4326)

# OPCIONAL pero MUY recomendable: quitar columna gigante other_tags si existe
highways_buffer_4326 <- highways_buffer_4326 %>%
  dplyr::select(-dplyr::any_of("other_tags"))

############################################################
# 5. Exportar como GeoPackage highway_osm.gpkg
############################################################

st_write(
  highways_buffer_4326,
  "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/highway_osm.gpkg",
  layer = "highway_osm",
  delete_dsn = TRUE
)

message("??? Archivo exportado como highway_osm.gpkg (capa: highway_osm) con buffer de 30 m")





#################################################################################################################################



library(sf)
library(dplyr)

############################################################
# 1. Leer el main highway_osm desde el GeoPackage
############################################################

gpkg_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/input/highway_osm.gpkg"

highways <- st_read(gpkg_path, layer = "highway_osm")

############################################################
# 2. Crear los tres grupos según la columna "highway"
############################################################

# 1) primary = "primary" + "primary_link"
highways_primary <- highways %>%
  filter(highway %in% c("primary", "primary_link"))

# 2) secondary = "secondary" + "secondary_link"
highways_secondary <- highways %>%
  filter(highway %in% c("secondary", "secondary_link"))

# 3) tertiary = "tertiary" + "tertiary_link"
highways_tertiary <- highways %>%
  filter(highway %in% c("tertiary", "tertiary_link"))

############################################################
# 3. Guardar cada grupo como una capa dentro del mismo GPKG
############################################################

st_write(
  highways_primary,
  gpkg_path,
  layer = "highway_primary",
  delete_layer = TRUE
)

st_write(
  highways_secondary,
  gpkg_path,
  layer = "highway_secondary",
  delete_layer = TRUE
)

st_write(
  highways_tertiary,
  gpkg_path,
  layer = "highway_tertiary",
  delete_layer = TRUE
)

message("??? Capas highway_primary, highway_secondary y highway_tertiary guardadas en highway_osm.gpkg")
