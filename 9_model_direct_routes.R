# Cargar librerías necesarias
library(raster)
library(gdistance)
library(sf)
library(future.apply)

# Configurar la paralelización
plan(multisession, workers = 31)
options(future.globals.maxSize = 5 * 1024^3)  # Aumentar el límite de tamaño de los globals a 5 GB

# Paso 1: Cargar el raster base para el proceso de reclasificación
raster_reescalado <- raster("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VERSION2_RUTAS/DATA/raster_reescalado_1km.tif")

# Definir las matrices de reclasificación para los escenarios 2 al 10 directamente sobre el raster reescalado
reclass_matrices <- list(
  # Escenario 2
  c(0, 944, 1, 6, 2, 14, 3, 23, 4, 37, 5, 45, 10, 6, 11, 499, 12, 348, 13, 295, 14, 245, 15, 199, 16, 146, 17, 93, 20, 499, 21, 14, 22, 14, 23, 14, 24, 793),
  # Escenario 3
  c(0, 965, 1, 25, 2, 35, 3, 45, 4, 55, 5, 65, 10, 25, 11, 515, 12, 365, 13, 315, 14, 265, 15, 215, 16, 165, 17, 115, 20, 515, 21, 35, 22, 35, 23, 35, 24, 815),
  # Escenario 4
  c(0, 958, 1, 18, 2, 28, 3, 38, 4, 48, 5, 58, 10, 18, 11, 508, 12, 358, 13, 308, 14, 258, 15, 208, 16, 158, 17, 108, 20, 508, 21, 28, 22, 28, 23, 28, 24, 808),
  # Escenario 5
  c(0, 936, 1, 1, 2, 6, 3, 16, 4, 26, 5, 36, 10, 1, 11, 486, 12, 336, 13, 286, 14, 236, 15, 186, 16, 136, 17, 86, 20, 486, 21, 6, 22, 6, 23, 6, 24, 786),
  # Escenario 6
  c(0, 950, 1, 1, 2, 137, 3, 262, 4, 281, 5, 166, 10, 96, 11, 67, 12, 278, 13, 10, 14, 116, 15, 100, 16, 150, 17, 100, 20, 500, 21, 20, 22, 20, 23, 238, 24, 650),
  # Escenario 7
  c(0, 944, 1, 1, 2, 152, 3, 277, 4, 296, 5, 181, 10, 111, 11, 82, 12, 293, 13, 25, 14, 131, 15, 115, 16, 150, 17, 100, 20, 500, 21, 20, 22, 20, 23, 253, 24, 665),
  # Escenario 8
  c(0, 965, 1, 1, 2, 162, 3, 287, 4, 306, 5, 191, 10, 121, 11, 92, 12, 303, 13, 35, 14, 141, 15, 125, 16, 150, 17, 100, 20, 500, 21, 20, 22, 20, 23, 263, 24, 675),
  # Escenario 9
  c(0, 958, 1, 1, 2, 172, 3, 297, 4, 316, 5, 201, 10, 131, 11, 102, 12, 313, 13, 45, 14, 151, 15, 135, 16, 150, 17, 100, 20, 500, 21, 20, 22, 20, 23, 273, 24, 685),
  # Escenario 10
  c(0, 936, 1, 1, 2, 182, 3, 307, 4, 326, 5, 211, 10, 141, 11, 112, 12, 323, 13, 55, 14, 161, 15, 145, 16, 150, 17, 100, 20, 500, 21, 20, 22, 20, 23, 283, 24, 695)
)

# Inicializar una lista para almacenar las capas de conductancia corregidas para cada escenario
conductancia_list <- list()

# Calcular la capa de conductancia corregida para cada escenario y almacenar en la lista
for (i in 1:length(reclass_matrices)) {
  cat("Procesando la capa de conductancia para el Escenario", i + 1, "...\n")
  
  # Reclasificar el raster reescalado directamente según el escenario actual
  reclass_matrix <- reclass_matrices[[i]]
  raster_resistencia_escenario <- reclassify(raster_reescalado, matrix(reclass_matrix, ncol = 2, byrow = TRUE))
  
  # Calcular la capa de conductancia y aplicar geoCorrection
  conductancia <- transition(1 / raster_resistencia_escenario, transitionFunction = mean, directions = 8)
  conductancia_corr <- geoCorrection(conductancia, type = "c", multpl = FALSE)
  
  # Guardar la capa de conductancia corregida en la lista
  conductancia_list[[paste0("escenario_", i + 1)]] <- conductancia_corr
}

cat("Todas las capas de conductancia para los escenarios han sido calculadas y almacenadas en la lista.\n")

# Ejemplo de cómo acceder a la capa de un escenario específico
# Por ejemplo, para acceder a la capa del Escenario 2:
#conductancia_esc2 <- conductancia_list[["escenario_2"]]



###################################################################################
#####################################################################################
#####################################################################################
# Cargar las bibliotecas necesarias
library(future.apply)
library(gdistance)
library(sf)
library(raster)

# Configurar la paralelización
plan(multisession, workers = 31)
options(future.globals.maxSize = 5 * 1024^3)  # Aumentar el límite de tamaño de los globals a 5 GB

# Directorio de salida
output_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RUTAS_DIRECTOS_081124/RESULTADOS/2022/"

# Lista de escenarios
escenarios <- 2:10

# Lista para almacenar claves fallidas por escenario
claves_fallidas_global <- list()

# Función para calcular rutas en paralelo a partir de coordenadas únicas
calcular_rutas_paralelo_lote_opt <- function(conductancia_corr, coords_origen, coords_destino, claves, output_rutas, tamano_lote = 50) {
  cat("Iniciando el cálculo de rutas\n")
  
  total_puntos <- nrow(coords_origen)
  num_lotes <- ceiling(total_puntos / tamano_lote)
  
  for (lote in 1:num_lotes) {
    cat("Calculando lote", lote, "de", num_lotes, "(", tamano_lote, "rutas por lote)\n")
    
    inicio <- (lote - 1) * tamano_lote + 1
    fin <- min(lote * tamano_lote, total_puntos)
    
    # Extraer las coordenadas y claves del lote actual
    coords_origen_lote <- coords_origen[inicio:fin, ]
    coords_destino_lote <- coords_destino[inicio:fin, ]
    claves_lote <- claves[inicio:fin]
    
    rutas_list <- lapply(1:nrow(coords_origen_lote), function(i) {
      ruta_numero <- i + inicio - 1
      cat("Calculando ruta", ruta_numero, "de", total_puntos, "\n")
      
      # Extraer las coordenadas como vectores numéricos
      origen_coords <- as.numeric(coords_origen_lote[i, ])
      destino_coords <- as.numeric(coords_destino_lote[i, ])
      
      # Calcular la ruta con manejo de errores
      ruta <- tryCatch(
        shortestPath(conductancia_corr, origen_coords, destino_coords, output = "SpatialLines"),
        error = function(e) {
          cat("Error calculando ruta", ruta_numero, "clave:", claves_lote[i], "-", e$message, "\n")
          claves_fallidas_global[[as.character(esc)]] <<- c(claves_fallidas_global[[as.character(esc)]], claves_lote[i])
          return(NULL)
        }
      )
      
      # Procesar si la ruta se calculó correctamente
      if (!is.null(ruta)) {
        ruta_sf <- st_as_sf(ruta)
        ruta_sf$clave <- claves_lote[i]  # Agregar la clave a la ruta
        return(ruta_sf)
      } else {
        return(NULL)
      }
    })
    
    rutas_lote_sf <- do.call(rbind, rutas_list)  # Unir todas las rutas del lote en una sola lista
    
    if (!is.null(rutas_lote_sf)) {
      # Guardar las rutas del lote en el shapefile (usar append para agregar cada lote)
      if (lote == 1) {
        st_write(rutas_lote_sf, output_rutas, delete_layer = TRUE)  # Crear el shapefile la primera vez
      } else {
        st_write(rutas_lote_sf, output_rutas, append = TRUE)  # Agregar rutas siguientes
      }
    }
    
    gc()  # Liberar memoria después de cada lote
  }
  
  cat("Cálculo de rutas completado.\n")
}

# Bucle para procesar cada escenario
for (esc in escenarios) {
  cat("Procesando el Escenario", esc, "...\n")
  
  # Obtener la capa de conductancia para el escenario actual
  conductancia_corr <- conductancia_list[[paste0("escenario_", esc)]]
  
  # Definir la ruta de salida específica para el escenario
  output_rutas <- paste0(output_dir, "rutas_optimas_2022_escenario_", esc, ".shp")
  
  # Cargar las frecuencias de rutas (asegúrate de que el archivo existe y está en el formato esperado)
  rutas_origen_destino <- read.csv(paste0(output_dir, "Frecuencia_Origen_Destino.csv"))
  
  # Calcular rutas
  calcular_rutas_paralelo_lote_opt(
    conductancia_corr = conductancia_corr,
    coords_origen = rutas_origen_destino[, c("origen_x", "origen_y")],
    coords_destino = rutas_origen_destino[, c("destino_x", "destino_y")],
    claves = rutas_origen_destino$clave,
    output_rutas = output_rutas
  )
  
  cat("Escenario", esc, "completado.\n")
}

# Guardar claves fallidas globales
if (length(claves_fallidas_global) > 0) {
  fallidas_df <- do.call(rbind, lapply(names(claves_fallidas_global), function(esc) {
    data.frame(escenario = esc, clave = claves_fallidas_global[[esc]])
  }))
  write.csv(fallidas_df, paste0(output_dir, "claves_fallidas_global.csv"), row.names = FALSE)
  cat("Claves fallidas guardadas en:", paste0(output_dir, "claves_fallidas_global.csv"), "\n")
}

cat("Cálculo de rutas para todos los escenarios completado.\n")
