# Cargar las librerías necesarias
library(sf)
library(dplyr)

# Leer el archivo shapefile
ruta_shapefile <- "C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL/COCA_SIMCI2.shp"
shapefile_data <- st_read(ruta_shapefile)
colnames(shapefile_data)


# Definir la ruta donde se guardarán los shapefiles
ruta_salida <- "C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL/RESULTADOS/"


# Crear la carpeta si no existe
if (!dir.exists(ruta_salida)) {
  dir.create(ruta_salida, recursive = TRUE)
}

# Filtrar las columnas que contienen "A_" y seleccionar los años 2017 a 2022
years <- 2017:2022

# Función para procesar cada año: Filtrar polígonos con valores mayores a 20 en la columna del año, disolver y obtener centroides
process_year <- function(year_data, year) {
  # Filtrar por los valores de la columna del año mayores a 20
  filtered <- year_data %>% filter(!!sym(paste0("A_", year)) > 20)
  
  # Disolver solo los polígonos contiguos
  dissolved <- st_union(filtered)  # Disolver los polígonos contiguos
  dissolved <- st_cast(dissolved, "POLYGON")  # Asegurar que se mantengan polígonos individuales
  
  # Obtener los centroides de los polígonos resultantes
  centroids <- st_centroid(dissolved)
  
  # Guardar los archivos en la ruta especificada
  st_write(dissolved, paste0(ruta_salida, "disolved_", year, ".shp"), delete_layer = TRUE)
  st_write(centroids, paste0(ruta_salida, "centroids_", year, ".shp"), delete_layer = TRUE)
}

# Aplicar la función para cada año
for (year in years) {
  year_data <- shapefile_data %>% select(geometry, paste0("A_", year))
  process_year(year_data, year)
}
