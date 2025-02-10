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


# Cargar librerías necesarias
library(raster)
library(gdistance)
library(sf)
library(future.apply)

# Configurar la paralelización
plan(multisession, workers = 31)
options(future.globals.maxSize = 5 * 1024^3)  # Aumentar el límite de tamaño de los globals a 5 GB

# Definir el directorio de salida
output_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RUTAS_PASOS_051124/RESULTADOS/2017/"

# Lista con nombres de los escenarios para guardar archivos
escenarios <- 6:10

# Función para calcular rutas en paralelo, adaptada para aceptar capas de conductancia específicas
calcular_rutas_paralelo_lote_opt <- function(conductancia_corr, coords_origen, coords_destino, claves, etapa, output_rutas, tamano_lote = 50) {
  cat("Iniciando el cálculo de rutas para la etapa:", etapa, "\n")
  
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
      cat("Calculando ruta", ruta_numero, "de", total_puntos, "en la etapa", etapa, "\n")
      
      origen_coords <- as.numeric(coords_origen_lote[i, ])
      destino_coords <- as.numeric(coords_destino_lote[i, ])
      
      # Calcular la ruta
      ruta <- shortestPath(conductancia_corr, origen_coords, destino_coords, output = "SpatialLines")
      
      if (!is.null(ruta)) {
        ruta_sf <- st_as_sf(ruta)
        ruta_sf$etapa <- etapa
        ruta_sf$clave <- claves_lote[i]
        return(ruta_sf)
      } else {
        return(NULL)
      }
    })
    
    rutas_lote_sf <- do.call(rbind, rutas_list)
    
    if (!is.null(rutas_lote_sf)) {
      if (lote == 1 && etapa == "Origen-Hoja de Coca") {
        st_write(rutas_lote_sf, output_rutas, delete_layer = TRUE)
      } else {
        st_write(rutas_lote_sf, output_rutas, append = TRUE)
      }
    }
    
    gc()
  }
  
  cat("Cálculo de rutas para la etapa", etapa, "completado.\n")
}

# Cargar los datos de frecuencias de cada etapa
rutas_origen_hoja <- read.csv(paste0(output_dir, "Frecuencia_Origen_HojaCoca.csv"))
rutas_hoja_pasta <- read.csv(paste0(output_dir, "Frecuencia_HojaCoca_PastaCoca.csv"))
rutas_pasta_base <- read.csv(paste0(output_dir, "Frecuencia_PastaCoca_BaseCoca.csv"))
rutas_base_destino <- read.csv(paste0(output_dir, "Frecuencia_BaseCoca_Destino.csv"))

# Bucle para procesar cada escenario
for (esc in escenarios) {
  cat("Procesando el Escenario", esc, "...\n")
  
  # Obtener la capa de conductancia para el escenario actual
  conductancia_corr <- conductancia_list[[paste0("escenario_", esc)]]
  
  # Definir la ruta de salida específica para el escenario
  output_rutas <- paste0(output_dir, "rutas_optimas_2017_escenario_", esc, ".shp")
  
  # Calcular rutas para cada etapa usando la capa de conductancia del escenario
  calcular_rutas_paralelo_lote_opt(
    conductancia_corr = conductancia_corr,
    coords_origen = rutas_origen_hoja[, c("origen_x", "origen_y")],
    coords_destino = rutas_origen_hoja[, c("destino_x", "destino_y")],
    claves = rutas_origen_hoja$clave,
    etapa = "Origen-Hoja de Coca",
    output_rutas = output_rutas
  )
  
  calcular_rutas_paralelo_lote_opt(
    conductancia_corr = conductancia_corr,
    coords_origen = rutas_hoja_pasta[, c("origen_x", "origen_y")],
    coords_destino = rutas_hoja_pasta[, c("destino_x", "destino_y")],
    claves = rutas_hoja_pasta$clave,
    etapa = "Hoja de Coca-Pasta de Coca",
    output_rutas = output_rutas
  )
  
  calcular_rutas_paralelo_lote_opt(
    conductancia_corr = conductancia_corr,
    coords_origen = rutas_pasta_base[, c("origen_x", "origen_y")],
    coords_destino = rutas_pasta_base[, c("destino_x", "destino_y")],
    claves = rutas_pasta_base$clave,
    etapa = "Pasta de Coca-Base de Coca",
    output_rutas = output_rutas
  )
  
  calcular_rutas_paralelo_lote_opt(
    conductancia_corr = conductancia_corr,
    coords_origen = rutas_base_destino[, c("origen_x", "origen_y")],
    coords_destino = rutas_base_destino[, c("destino_x", "destino_y")],
    claves = rutas_base_destino$clave,
    etapa = "Base de Coca-Centroides de Cocaina",
    output_rutas = output_rutas
  )
  
  cat("Cálculo de rutas para el Escenario", esc, "completado.\n")
}

cat("Cálculo de rutas para todos los escenarios completado.\n")


##########################################################################################################################################
##################### replicar las rutas con frecuencias ############################################################################
##########################################################################################################################################
# Cargar bibliotecas necesarias
library(sf)
library(dplyr)

# Ruta base y configuración de archivos
ruta_base <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RUTAS_PASOS_051124/RESULTADOS/2022/"
archivo_base <- "rutas_optimas_2022_escenario_"
archivo_replicado <- "rutas_optimas_2022_replicated_escenario_"
extensiones <- c(".shp", ".shx", ".dbf", ".prj")

# Leer los archivos de frecuencias
frecuencia_origen_hoja <- read.csv(paste0(ruta_base, "Frecuencia_Origen_HojaCoca.csv"))
frecuencia_hoja_pasta <- read.csv(paste0(ruta_base, "Frecuencia_HojaCoca_PastaCoca.csv"))
frecuencia_pasta_base <- read.csv(paste0(ruta_base, "Frecuencia_PastaCoca_BaseCoca.csv"))
frecuencia_base_destino <- read.csv(paste0(ruta_base, "Frecuencia_BaseCoca_Destino.csv"))

# Función para replicar rutas en el shapefile según la frecuencia
replicar_rutas <- function(rutas, frecuencia_data, etapa) {
  # Filtrar datos de frecuencia con valores mayores a 1
  frecuencias_multiples <- frecuencia_data %>%
    filter(frecuencia > 1)
  
  if (nrow(frecuencias_multiples) == 0) {
    cat("No hay frecuencias mayores a 1 para replicar en la etapa:", etapa, "\n")
    return(rutas)
  }
  
  # Filtrar las rutas correspondientes a la etapa
  rutas_etapa <- rutas %>%
    filter(etapa == !!etapa)
  
  # Crear una lista para almacenar las rutas replicadas
  rutas_replicadas <- list()
  
  # Iterar sobre cada clave con frecuencia mayor a 1
  for (i in 1:nrow(frecuencias_multiples)) {
    clave_actual <- frecuencias_multiples$clave[i]
    frecuencia_actual <- frecuencias_multiples$frecuencia[i]
    
    cat("Procesando clave:", clave_actual, "con frecuencia:", frecuencia_actual, "\n")
    
    ruta_original <- rutas_etapa %>%
      filter(clave == clave_actual)
    
    if (nrow(ruta_original) == 0) {
      cat("Advertencia: Clave", clave_actual, "no encontrada en las rutas.\n")
      next
    }
    
    rutas_replicadas[[i]] <- ruta_original[rep(1, frecuencia_actual), ]
  }
  
  # Combinar todas las rutas replicadas y las originales con frecuencia 1
  rutas_finales <- rutas_etapa %>%
    filter(!clave %in% frecuencias_multiples$clave) %>%
    bind_rows(do.call(rbind, rutas_replicadas))
  
  rutas <- rutas %>%
    filter(etapa != !!etapa) %>%
    bind_rows(rutas_finales)
  
  return(rutas)
}

# Bucle para procesar múltiples escenarios
for (escenario in 2:10) {
  # Crear rutas para los archivos de entrada y salida
  archivo_actual <- paste0(ruta_base, archivo_base, escenario, ".shp")
  archivo_salida <- paste0(ruta_base, archivo_replicado, escenario, ".shp")
  
  # Copiar los archivos del shapefile original al replicado
  for (ext in extensiones) {
    file.copy(from = paste0(ruta_base, archivo_base, escenario, ext),
              to = paste0(ruta_base, archivo_replicado, escenario, ext),
              overwrite = TRUE)
  }
  
  # Leer el shapefile replicado
  rutas <- st_read(archivo_salida)
  cat("Shapefile cargado exitosamente para el escenario", escenario, ".\n")
  
  # Aplicar la replicación de rutas para cada etapa
  rutas <- replicar_rutas(rutas, frecuencia_hoja_pasta, "Hoja de Coca-Pasta de Coca")
  rutas <- replicar_rutas(rutas, frecuencia_pasta_base, "Pasta de Coca-Base de Coca")
  rutas <- replicar_rutas(rutas, frecuencia_base_destino, "Base de Coca-Centroides de Cocaina")
  
  # Guardar el shapefile actualizado
  st_write(rutas, archivo_salida, delete_dsn = TRUE)
  cat("El proceso de replicación ha finalizado para el escenario", escenario, ".\n")
}
