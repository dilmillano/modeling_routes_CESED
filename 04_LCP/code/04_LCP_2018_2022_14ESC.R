#############################################################################################
# SCRIPT: Rutas LCP 2018-2022 con 14 escenarios de conductancia compuesta (categorías + pendiente)
# AUTORA: Diana Millan
#
# LÓGICA CLAVE:
#   - Las superficies de resistencia compuesta por escenario YA existen para 2017.
#   - Para cada escenario (1..14) se calcula UNA SOLA VEZ la conductancia (TransitionLayer)
#     a partir de esos raster de resistencia compuesta.
#   - Esa misma conductancia por escenario se reutiliza para todos los años 2018-2022.
#
# ENTRADAS GLOBALES:
#   - Raster de resistencia compuesta por escenario (ya calculados para 2017):
#       .../04_LCP/output/test_2017/escenarios/resistencia_compuesta_escenario_k.tif
#
# ENTRADAS POR AÑO (2018-2022):
#   - Orígenes:
#       01_origin/output/centroids_YYYY_gt20ha.shp
#   - Destinos:
#       02_exit_points/04_normalization/output_V2/seiz_YYYY_pop_wp_p99.shp
#       (filtrando cultivo == "Cocaína")
#
# SALIDAS POR AÑO (LCP_YYYY):
#   - Asignaciones origen-destino (20 destinos más cercanos por origen)
#   - CSV de rutas directas (coords origen/destino)
#   - CSV de frecuencias por clave
#   - Shapefiles de rutas LCP para cada escenario 1..14 en carpeta /escenarios
#   - Registro de tiempos de ejecución por año en un .txt
#############################################################################################

library(raster)
library(gdistance)
library(sf)
library(dplyr)
library(future.apply)
library(tibble)

########################################
# 0. Parámetros generales y rutas base
########################################

# Años de interés
years <- 2018:2022

# Escenarios que se van a usar (cutoff = 14)
escenarios <- 1:14
n_esc <- length(escenarios)

# Carpeta raíz del proyecto de rutas
root_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED"

# Carpeta base de salida para todos los años
output_root <- file.path(root_dir, "04_LCP", "output")
if (!dir.exists(output_root)) {
  dir.create(output_root, recursive = TRUE)
}

# Carpeta donde están los raster de resistencia compuesta ya calculados para 2017
esc_dir_2017 <- file.path(output_root, "test_2017", "escenarios")

# Archivo donde se registrarán los tiempos por año
path_tiempos <- file.path(output_root, "tiempos_LCP_2018_2022.txt")

########################################
# 1. Calcular CONDUCTANCIA una sola vez por escenario
########################################

cat("\n===========================\n")
cat("Cálculo único de conductancia por escenario (1..14)\n")
cat("===========================\n")

conductancia_list <- vector("list", n_esc)
names(conductancia_list) <- paste0("esc_", escenarios)

for (esc in escenarios) {
  cat("\n--- Escenario", esc, ": cargando resistencia compuesta 2017 ---\n")
  
  path_resist_comp_i <- file.path(
    esc_dir_2017,
    paste0("resistencia_compuesta_escenario_", esc, ".tif")
  )
  
  if (!file.exists(path_resist_comp_i)) {
    stop("No se encontró el raster de resistencia compuesta para escenario ",
         esc, " en: ", path_resist_comp_i)
  }
  
  resist_comp_i <- raster(path_resist_comp_i)
  
  cat("  * Calculando Transition (conductancia)...\n")
  conductancia_i <- transition(1 / resist_comp_i,
                               transitionFunction = mean,
                               directions = 8)
  
  cat("  * Aplicando geoCorrection...\n")
  conductancia_corr_i <- geoCorrection(conductancia_i,
                                       type   = "c",
                                       multpl = FALSE)
  
  conductancia_list[[paste0("esc_", esc)]] <- conductancia_corr_i
}

cat("\n??? Conductancia calculada y almacenada en memoria para los 14 escenarios.\n")

########################################
# 2. Función de cálculo de rutas LCP en paralelo (por lotes)
########################################

calcular_rutas_paralelo_lote_opt <- function(conductancia_corr,
                                             coords_origen,
                                             coords_destino,
                                             claves,
                                             output_rutas,
                                             tamano_lote = 50) {
  cat("Iniciando el cálculo de rutas LCP para este escenario...\n")
  
  total_puntos <- nrow(coords_origen)
  num_lotes <- ceiling(total_puntos / tamano_lote)
  
  for (lote in 1:num_lotes) {
    cat("  Lote", lote, "de", num_lotes, "\n")
    
    inicio <- (lote - 1) * tamano_lote + 1
    fin    <- min(lote * tamano_lote, total_puntos)
    
    coords_origen_lote  <- coords_origen[inicio:fin, , drop = FALSE]
    coords_destino_lote <- coords_destino[inicio:fin, , drop = FALSE]
    claves_lote         <- claves[inicio:fin]
    
    rutas_list <- future_lapply(
      seq_len(nrow(coords_origen_lote)),
      function(i) {
        cat("    Ruta", i + inicio - 1, "/", total_puntos, "\n")
        origen_coords  <- as.numeric(coords_origen_lote[i, ])
        destino_coords <- as.numeric(coords_destino_lote[i, ])
        
        ruta <- tryCatch(
          shortestPath(conductancia_corr,
                       origen_coords,
                       destino_coords,
                       output = "SpatialLines"),
          error = function(e) {
            cat("    Error en clave:", claves_lote[i], "-", e$message, "\n")
            return(NULL)
          }
        )
        
        if (!is.null(ruta)) {
          ruta_sf <- st_as_sf(ruta)
          ruta_sf$clave <- claves_lote[i]
          return(ruta_sf)
        } else {
          return(NULL)
        }
      },
      future.seed = TRUE
    )
    
    rutas_lote_sf <- do.call(rbind, rutas_list)
    
    if (!is.null(rutas_lote_sf)) {
      if (lote == 1 && !file.exists(output_rutas)) {
        st_write(rutas_lote_sf, output_rutas, delete_layer = TRUE, quiet = TRUE)
      } else {
        st_write(rutas_lote_sf, output_rutas, append = TRUE, quiet = TRUE)
      }
    }
    
    gc()
  }
  
  cat("Cálculo de rutas LCP completado para este escenario.\n")
}

########################################
# 3. Configurar paralelización GLOBAL
########################################

cat("\nConfigurando paralelización (plan multisession)...\n")
plan(multisession, workers = 30)
options(future.globals.maxSize = 5 * 1024^3)

########################################
# 4. LOOP PRINCIPAL POR AÑO (2018-2022)
########################################

for (year in years) {
  cat("\n\n############################################\n")
  cat("### Procesando año", year, "...\n")
  cat("############################################\n")
  
  t_ini <- Sys.time()
  
  #------------------------------------------------
  # 4.1. Definir rutas de entrada / salida para este año
  #------------------------------------------------
  
  path_origen_year <- file.path(
    root_dir,
    "01_origin",
    "output",
    paste0("centroids_", year, "_gt20ha.shp")
  )
  
  path_destino_year <- file.path(
    root_dir,
    "02_exit_points",
    "04_normalization",
    "output_V2",
    paste0("seiz_", year, "_pop_wp_p99.shp")
  )
  
  # Carpeta base de salida para este año
  output_dir_year <- file.path(output_root, paste0("LCP_", year))
  if (!dir.exists(output_dir_year)) {
    dir.create(output_dir_year, recursive = TRUE)
  }
  
  # Carpeta de escenarios para este año
  escenarios_dir_year <- file.path(output_dir_year, "escenarios")
  if (!dir.exists(escenarios_dir_year)) {
    dir.create(escenarios_dir_year, recursive = TRUE)
  }
  
  # Salidas comunes por año
  path_asignaciones <- file.path(output_dir_year,
                                 paste0("Asignaciones_Origen_Destino_", year, ".shp"))
  path_destinos_id  <- file.path(output_dir_year,
                                 paste0("puntos_destino_con_id_", year, ".shp"))
  path_rutas_csv    <- file.path(output_dir_year,
                                 paste0("Rutas_Origen_Destino_", year, ".csv"))
  path_freq_csv     <- file.path(output_dir_year,
                                 paste0("Frecuencia_Origen_Destino_", year, ".csv"))
  
  #------------------------------------------------
  # 4.2. Cargar orígenes y destinos para este año
  #------------------------------------------------
  
  cat("Cargando orígenes y destinos para", year, "...\n")
  
  if (!file.exists(path_origen_year)) {
    warning("No se encontró el shapefile de orígenes para ", year, ": ", path_origen_year)
    next
  }
  
  if (!file.exists(path_destino_year)) {
    warning("No se encontró el shapefile de destinos para ", year, ": ", path_destino_year)
    next
  }
  
  puntos_origen  <- st_read(path_origen_year, quiet = TRUE)
  puntos_destino <- st_read(path_destino_year, quiet = TRUE)
  
  # Filtrar solo cultivo == "Cocaína" (si existe la columna)
  if ("cultivo" %in% names(puntos_destino)) {
    puntos_destino <- puntos_destino %>%
      filter(cultivo == "Cocaína")
  } else {
    warning("El shapefile de destinos para ", year, " no tiene columna 'cultivo'. Se usan todos los puntos.")
  }
  
  cat("  * Número de orígenes:", nrow(puntos_origen), "\n")
  cat("  * Número de destinos (Cocaína):", nrow(puntos_destino), "\n")
  
  if (nrow(puntos_origen) == 0 || nrow(puntos_destino) == 0) {
    warning("Sin orígenes o destinos para ", year, ". Se salta el año.")
    next
  }
  
  #------------------------------------------------
  # 4.3. Asignar IDs y alinear CRS origen/destino
  #------------------------------------------------
  
  puntos_origen$ID_ORIGEN <- seq_len(nrow(puntos_origen))
  puntos_destino$ID_DEST  <- seq_len(nrow(puntos_destino))
  
  crs_origen <- st_crs(puntos_origen)
  if (st_crs(puntos_destino) != crs_origen) {
    puntos_destino <- st_transform(puntos_destino, crs_origen)
  }
  
  #------------------------------------------------
  # 4.4. Asignar 20 destinos más cercanos a cada origen
  #------------------------------------------------
  
  cat("Calculando distancias y asignando 20 destinos más cercanos por origen...\n")
  dist_matrix <- st_distance(puntos_origen, puntos_destino)
  
  asignaciones_directas <- do.call(rbind, lapply(1:nrow(puntos_origen), function(i) {
    n_dest <- min(20, nrow(puntos_destino))
    destinos_idx <- order(dist_matrix[i, ])[1:n_dest]
    data.frame(
      ID_ORIGEN = puntos_origen$ID_ORIGEN[i],
      ID_DEST   = puntos_destino$ID_DEST[destinos_idx]
    )
  }))
  
  # Shapefile de asignaciones
  asignaciones_sf <- merge(puntos_origen, asignaciones_directas, by = "ID_ORIGEN")
  st_write(asignaciones_sf, path_asignaciones, delete_layer = TRUE, quiet = TRUE)
  
  # Destinos con ID
  st_write(puntos_destino, path_destinos_id, delete_layer = TRUE, quiet = TRUE)
  
  cat("  * Asignaciones y puntos_destino_con_id_", year, ".shp creados.\n", sep = "")
  
  #------------------------------------------------
  # 4.5. Construir tabla de rutas y frecuencias
  #------------------------------------------------
  
  cat("Construyendo tabla de coordenadas y frecuencias...\n")
  
  asignaciones_directas <- st_read(path_asignaciones, quiet = TRUE)
  puntos_destino_id     <- st_read(path_destinos_id, quiet = TRUE)
  
  # Coordenadas de origen
  origen_df <- asignaciones_directas %>%
    mutate(
      clave    = paste0(ID_ORIGEN, "_", ID_DEST),
      origen_x = st_coordinates(.)[, "X"],
      origen_y = st_coordinates(.)[, "Y"]
    ) %>%
    st_drop_geometry()
  
  # Coordenadas de destino
  destino_coords <- puntos_destino_id %>%
    mutate(
      destino_x = st_coordinates(.)[, "X"],
      destino_y = st_coordinates(.)[, "Y"]
    ) %>%
    st_drop_geometry() %>%
    select(ID_DEST, destino_x, destino_y)
  
  rutas_directas_df <- origen_df %>%
    left_join(destino_coords, by = "ID_DEST") %>%
    select(clave, ID_ORIGEN, ID_DEST, origen_x, origen_y, destino_x, destino_y)
  
  write.csv(rutas_directas_df, path_rutas_csv, row.names = FALSE)
  
  # Frecuencias (generalizable)
  frecuencia_directa <- rutas_directas_df %>%
    group_by(clave) %>%
    summarize(
      frecuencia = n(),
      ID_ORIGEN  = first(ID_ORIGEN),
      ID_DEST    = first(ID_DEST),
      origen_x   = first(origen_x),
      origen_y   = first(origen_y),
      destino_x  = first(destino_x),
      destino_y  = first(destino_y),
      .groups    = "drop"
    )
  
  write.csv(frecuencia_directa, path_freq_csv, row.names = FALSE)
  
  cat("  * Rutas_Origen_Destino_", year, ".csv y Frecuencia_Origen_Destino_", year,
      ".csv guardadas.\n", sep = "")
  
  rutas_directas <- read.csv(path_freq_csv)
  
  #------------------------------------------------
  # 4.6. Loop de escenarios para este año usando conductancia precomputada
  #------------------------------------------------
  
  for (esc in escenarios) {
    cat("\n===========================\n")
    cat("AÑO", year, "- ESCENARIO", esc, "\n")
    cat("===========================\n")
    
    conductancia_corr_i <- conductancia_list[[paste0("esc_", esc)]]
    
    # Salida de rutas LCP para este escenario y año
    path_rutas_lcp_i <- file.path(
      escenarios_dir_year,
      paste0("rutas_LCP_", year, "_escenario_", esc, ".shp")
    )
    
    cat("Lanzando cálculo de rutas LCP para año", year,
        "escenario", esc, "...\n")
    
    calcular_rutas_paralelo_lote_opt(
      conductancia_corr = conductancia_corr_i,
      coords_origen     = rutas_directas[, c("origen_x", "origen_y")],
      coords_destino    = rutas_directas[, c("destino_x", "destino_y")],
      claves            = rutas_directas$clave,
      output_rutas      = path_rutas_lcp_i,
      tamano_lote       = 50
    )
    
    cat("??? Escenario", esc, "para el año", year, "finalizado.\n")
  }
  
  #------------------------------------------------
  # 4.7. Registrar tiempo para este año
  #------------------------------------------------
  
  t_fin <- Sys.time()
  elapsed_min <- as.numeric(difftime(t_fin, t_ini, units = "mins"))
  
  linea_tiempo <- sprintf(
    "Año %d: %.2f minutos (inicio: %s, fin: %s)\n",
    year, elapsed_min, format(t_ini), format(t_fin)
  )
  
  cat("\n", linea_tiempo, "\n")
  cat(linea_tiempo, file = path_tiempos, append = TRUE)
}

cat("\n===========================================\n")
cat("FIN: Todos los años 2018-2022 procesados (escenarios 1-14).\n")
cat("Revisa las carpetas LCP_YYYY dentro de:\n", output_root, "\n")
cat("Y el log de tiempos en:\n", path_tiempos, "\n")
cat("===========================================\n")
