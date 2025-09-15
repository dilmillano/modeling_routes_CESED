# Instalar paquetes necesarios
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")

# Cargar bibliotecas
library(sf)
library(dplyr)

# Ruta base a los shapefiles de los escenarios
ruta_base <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RUTAS_PASOS_051124/RESULTADOS/2022"

# Generar una lista con los nombres de los escenarios del 2 al 10
escenarios <- 2:10

# Procesar cada escenario individualmente
for (escenario in escenarios) {
  
  # Construir la ruta al shapefile del escenario actual
  ruta_shp <- file.path(ruta_base, paste0("rutas_optimas_2022_exploded_escenario_", escenario, ".shp"))
  
  # Verificar si el archivo existe
  if (!file.exists(ruta_shp)) {
    cat("El archivo no existe para el escenario:", escenario, "\n")
    next # Saltar al siguiente escenario si el archivo no existe
  }
  
  # Cargar el shapefile
  rutas <- st_read(ruta_shp)
  
  # Crear una nueva columna que represente la geometría como un identificador único
  rutas <- rutas %>%
    mutate(
      geometry_id = st_as_text(geometry) # Identificador único de geometría
    )
  
  # Contar cuántas veces se repite cada geometría única
  rutas_unicas <- rutas %>%
    group_by(geometry_id) %>%
    summarise(
      count = n(),
      geometry = first(geometry) # Mantener una geometría representativa
    ) %>%
    ungroup()
  
  # Convertir la columna `geometry` de nuevo a tipo geometría sf
  rutas_unicas <- st_as_sf(rutas_unicas, wkt = "geometry")
  
  # Construir la ruta de salida para el shapefile procesado
  output_path <- file.path(ruta_base, paste0("rutas_geometrias_unicas_escenario_", escenario, ".shp"))
  
  # Guardar el shapefile procesado
  st_write(rutas_unicas, output_path)
  
  # Mensaje de éxito
  cat("Procesado y guardado el shapefile con geometrías únicas para el escenario:", escenario, "\n")
}

colnames(rutas_final)

# --- COMBINAR RESULTADOS PROCESADOS ---
library(sf)
library(dplyr)

# Leer solo los shapefiles depurados
lista_rutas_unicas <- lapply(escenarios, function(escenario) {
  ruta_shp <- file.path(ruta_base, paste0("rutas_geometrias_unicas_escenario_", escenario, ".shp"))
  if (file.exists(ruta_shp)) {
    st_read(ruta_shp)
  } else {
    NULL
  }
})

# Eliminar resultados NULL en caso de que algún archivo no exista
lista_rutas_unicas <- lista_rutas_unicas[!sapply(lista_rutas_unicas, is.null)]

# Combinar los shapefiles depurados
rutas_final <- do.call(rbind, lista_rutas_unicas)

# Verificar y renombrar columnas para evitar problemas de nombres cortos
if ("gmtry_d" %in% colnames(rutas_final)) {
  rutas_final <- rutas_final %>% rename(geometry_id = gmtry_d)
}

# Validar que la columna geometry_id existe antes de continuar
if (!"geometry_id" %in% colnames(rutas_final)) {
  stop("Error: La columna 'geometry_id' no está presente en el conjunto de datos. Verifica los nombres de las columnas.")
}

# Contar geometrías únicas en el resultado combinado
rutas_final_unicas <- rutas_final %>%
  group_by(geometry_id) %>%
  summarise(
    count = sum(count, na.rm = TRUE),  # Sumar los conteos
    geometry = first(geometry)        # Mantener una geometría representativa
  ) %>%
  ungroup()

# Convertir de nuevo a tipo geometría sf
rutas_final_unicas <- st_as_sf(rutas_final_unicas, sf_column_name = "geometry")

# Guardar el shapefile final con geometrías únicas
output_path_final <- file.path(ruta_base, "rutas_geometrias_unicas_todos_escenarios.shp")
st_write(rutas_final_unicas, output_path_final, delete_layer = TRUE)

# Mensaje de éxito final
cat("El shapefile final con geometrías únicas ha sido guardado en:\n", output_path_final, "\n")
