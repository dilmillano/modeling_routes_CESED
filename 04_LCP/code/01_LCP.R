#############################################################################################
# SCRIPT: Rutas LCP 2017 con 10 escenarios de conductancia compuesta (categorías + pendiente)
# AUTORA: Diana Millan
#
# ENTRADA:
#  - Raster categórico: cost_surface_1km_fondo.tif (0-8)
#  - Raster continuo: slope_1km_4326.tif
#  - Orígenes: centroids_2017_gt20ha.shp
#  - Destinos: seiz_2017_pop_wp_p99.shp (filtrando cultivo == "Cocaína")
#
# SALIDA:
#  - Asignaciones origen-destino (20 destinos más cercanos por origen)
#  - CSV de rutas y frecuencias
#  - Para cada escenario 1-10 (en carpeta /escenarios):
#       * Raster de resistencia categórica
#       * Raster de resistencia compuesta (categoría + pendiente)
#       * Shapefile de rutas LCP 2017
#############################################################################################

library(raster)
library(gdistance)
library(sf)
library(dplyr)
library(future.apply)
library(tibble)

###########################
# 0. Rutas de entrada / salida
###########################

# Rasters
path_cost_cat <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/output/cost_surface_1km_fondo.tif"
path_slope    <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/output/slope_1km_4326.tif"

# Orígenes y destinos
path_origen  <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/01_origin/output/centroids_2017_gt20ha.shp"
path_destino <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/02_exit_points/04_normalization/output_V2/seiz_2017_pop_wp_p99.shp"

# Carpeta base de salida
output_dir_base <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/04_LCP/output/test_2017"

if (!dir.exists(output_dir_base)) {
  dir.create(output_dir_base, recursive = TRUE)
}

# Carpeta específica para escenarios
escenarios_dir <- file.path(output_dir_base, "escenarios")
if (!dir.exists(escenarios_dir)) {
  dir.create(escenarios_dir, recursive = TRUE)
}

# Archivos de salida comunes (no dependen de escenarios)
path_asignaciones <- file.path(output_dir_base, "Asignaciones_Origen_Destino_2017.shp")
path_destinos_id  <- file.path(output_dir_base, "puntos_destino_con_id_2017.shp")
path_rutas_csv    <- file.path(output_dir_base, "Rutas_Origen_Destino_2017.csv")
path_freq_csv     <- file.path(output_dir_base, "Frecuencia_Origen_Destino_2017.csv")

###########################
# 1. Cargar y alinear rasters (categórico + pendiente)
###########################

cat("Cargando rasters categórico y de pendiente...\n")
cost_cat <- raster(path_cost_cat)
slope    <- raster(path_slope)

# Alinear CRS: si son distintos, reproyectar slope al CRS del raster categórico
if (!compareCRS(cost_cat, slope)) {
  cat("CRS diferente: reproyectando slope al CRS del raster categórico...\n")
  slope <- projectRaster(slope, cost_cat, method = "bilinear")
}

# Alinear grilla (extent, filas, columnas, resolución)
if (!compareRaster(cost_cat, slope,
                   extent = TRUE, rowcol = TRUE, res = TRUE,
                   stopiffalse = FALSE)) {
  cat("Grillas distintas: reamostrando slope para que coincida con el raster categórico...\n")
  slope <- resample(slope, cost_cat, method = "bilinear")
}

cat("Listo: cost_cat y slope ahora tienen misma grilla y CRS.\n")

# Normalizar pendiente entre 0 y 1 (una sola vez, sirve para todos los escenarios)
cat("Normalizando pendiente (una sola vez)...\n")
s_min <- cellStats(slope, "min", na.rm = TRUE)
s_max <- cellStats(slope, "max", na.rm = TRUE)
slope_norm <- (slope - s_min) / (s_max - s_min)
slope_norm[slope_norm < 0] <- 0
slope_norm[slope_norm > 1] <- 1

# Peso de la pendiente (mismo para todos los escenarios por ahora)
alpha <- 1  # factor_pend = 1..2

###########################
# 2. Orígenes, destinos y asignaciones (común a todos los escenarios)
###########################

cat("Cargando orígenes y destinos 2017...\n")
puntos_origen  <- st_read(path_origen, quiet = TRUE)
puntos_destino <- st_read(path_destino, quiet = TRUE)

# Filtrar solo cultivo == "Cocaína"
puntos_destino <- puntos_destino %>% 
  filter(cultivo == "Cocaína")

cat("Número de orígenes:", nrow(puntos_origen), "\n")
cat("Número de destinos (Cocaína):", nrow(puntos_destino), "\n")

# Asignar IDs
puntos_origen$ID_ORIGEN <- seq_len(nrow(puntos_origen))
puntos_destino$ID_DEST  <- seq_len(nrow(puntos_destino))

# Unificar CRS
crs_origen <- st_crs(puntos_origen)
if (st_crs(puntos_destino) != crs_origen) {
  puntos_destino <- st_transform(puntos_destino, crs_origen)
}

cat("Calculando distancias y asignando 20 destinos más cercanos a cada origen...\n")
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

cat("Asignaciones origen-destino y puntos_destino_con_id_2017.shp creados en:\n",
    output_dir_base, "\n")

###########################
# 3. Tabla de coordenadas y frecuencias
###########################

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

# Unir
rutas_directas_df <- origen_df %>%
  left_join(destino_coords, by = "ID_DEST") %>%
  select(clave, ID_ORIGEN, ID_DEST, origen_x, origen_y, destino_x, destino_y)

write.csv(rutas_directas_df, path_rutas_csv, row.names = FALSE)

# Frecuencia (aquí será 1 por par, pero generalizamos)
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

cat("Tablas Rutas_Origen_Destino_2017.csv y Frecuencia_Origen_Destino_2017.csv guardadas.\n")

# Data frame para usar en los escenarios
rutas_directas <- read.csv(path_freq_csv)

###########################
# 4. Paralelización y función para rutas LCP
###########################

cat("Configurando paralelización...\n")
# Tienes 32 logical processors, usamos 30 para dejar algo libre
plan(multisession, workers = 30)
options(future.globals.maxSize = 5 * 1024^3)

calcular_rutas_paralelo_lote_opt <- function(conductancia_corr,
                                             coords_origen,
                                             coords_destino,
                                             claves,
                                             output_rutas,
                                             tamano_lote = 50) {
  cat("Iniciando el cálculo de rutas LCP...\n")
  
  total_puntos <- nrow(coords_origen)
  num_lotes <- ceiling(total_puntos / tamano_lote)
  
  for (lote in 1:num_lotes) {
    cat("  Lote", lote, "de", num_lotes, "\n")
    
    inicio <- (lote - 1) * tamano_lote + 1
    fin <- min(lote * tamano_lote, total_puntos)
    
    coords_origen_lote  <- coords_origen[inicio:fin, , drop = FALSE]
    coords_destino_lote <- coords_destino[inicio:fin, , drop = FALSE]
    claves_lote         <- claves[inicio:fin]
    
    # AQUÍ SÍ USAMOS FUTURE_LAPPLY PARA PARALELIZAR
    rutas_list <- future_lapply(
      seq_len(nrow(coords_origen_lote)),
      function(i) {
        cat("    Ruta", i + inicio - 1, "/", total_puntos, "\n")
        origen_coords  <- as.numeric(coords_origen_lote[i, ])
        destino_coords <- as.numeric(coords_destino_lote[i, ])
        
        ruta <- tryCatch(
          shortestPath(conductancia_corr, origen_coords, destino_coords, output = "SpatialLines"),
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
      if (lote == 1) {
        st_write(rutas_lote_sf, output_rutas, delete_layer = TRUE, quiet = TRUE)
      } else {
        st_write(rutas_lote_sf, output_rutas, append = TRUE, quiet = TRUE)
      }
    }
    
    gc()
  }
  
  cat("Cálculo de rutas LCP completado para este escenario.\n")
}

###########################
# 5. Definición de escenarios (reclasificación categórica)
###########################

# Valores base del ESCENARIO 1 (el que ya modelaste)
base_vals <- c(
  "0" = 950,  # fondo
  "1" = 10,   # ríos
  "2" = 40,   # vías primarias
  "3" = 30,   # vías secundarias
  "4" = 10,   # vías terciarias
  "5" = 10,   # caminos
  "6" = 600,  # cuerpos de agua
  "7" = 800,  # batallones
  "8" = 10    # puertos
)

cats_faciles  <- c("1", "2", "3", "4", "5", "8")  # deben quedar < 200
cats_restrict <- c("0", "6", "7")                 # siempre muy altas

n_esc <- 10
set.seed(123)  # para reproducibilidad

###########################
# 6. Loop sobre los 10 escenarios
###########################

for (esc in 1:n_esc) {
  cat("\n===========================\n")
  cat("Procesando ESCENARIO", esc, "...\n")
  cat("===========================\n")
  
  # -----------------------------
  # 6.1. Definir valores de resistencia por categoría
  # -----------------------------
  
  vals <- base_vals
  
  if (esc == 1) {
    # Escenario 1 = EXACTAMENTE el que ya modelaste (sin cambios)
    cat("Escenario 1: usando valores base sin variación.\n")
    
  } else {
    cat("Escenario", esc, ": generando variaciones aleatorias controladas.\n")
    
    # Fondo (0): alrededor del valor base, pero siempre muy alto
    vals["0"] <- base_vals["0"] * runif(1, 0.9, 1.1)    # ±10%
    vals["0"] <- max(vals["0"], 700)                    # mínimo 700
    
    # Categorías fáciles: variación pero siempre < 200
    vals[cats_faciles] <- base_vals[cats_faciles] * runif(length(cats_faciles), 0.7, 1.5)
    vals[cats_faciles] <- pmax(vals[cats_faciles], 1)    # no dejar en 0
    vals[cats_faciles] <- pmin(vals[cats_faciles], 200)  # tope 200
    
    # Cuerpos de agua (6) y batallones (7): siempre altos
    vals["6"] <- base_vals["6"] * runif(1, 0.8, 1.2)    # ±20%
    vals["7"] <- base_vals["7"] * runif(1, 0.8, 1.2)    # ±20%
    vals[c("6", "7")] <- pmax(vals[c("6", "7")], 400)   # mínimo 400
  }
  
  # Redondear a enteros
  vals <- round(vals)
  print(vals)
  
  # Matriz de reclasificación para este escenario
  reclass_matrix_i <- matrix(c(
    0, vals["0"],
    1, vals["1"],
    2, vals["2"],
    3, vals["3"],
    4, vals["4"],
    5, vals["5"],
    6, vals["6"],
    7, vals["7"],
    8, vals["8"]
  ), ncol = 2, byrow = TRUE)
  
  # -----------------------------
  # 6.2. Reclasificar a resistencia base y aplicar pendiente
  # -----------------------------
  
  cat("Reclasificando raster categórico a resistencia base (escenario", esc, ")...\n")
  resist_cat_i <- reclassify(cost_cat, reclass_matrix_i, include.lowest = TRUE, right = NA)
  
  # Factor de pendiente de este escenario (mismo alpha para todos)
  factor_pend_i <- 1 + alpha * slope_norm
  
  cat("Calculando resistencia compuesta (categoría + pendiente) para escenario", esc, "...\n")
  resist_comp_i <- resist_cat_i * factor_pend_i
  
  # Opcional: por seguridad, asegurar que clases fáciles no superen 200 después de la pendiente
  cat_vals  <- getValues(cost_cat)
  comp_vals <- getValues(resist_comp_i)
  idx_facil <- cat_vals %in% as.integer(cats_faciles)
  comp_vals[idx_facil & comp_vals > 200] <- 200
  resist_comp_i <- setValues(resist_comp_i, comp_vals)
  
  # Guardar rasters de este escenario
  path_resist_cat_i  <- file.path(escenarios_dir, paste0("resistencia_categorica_escenario_", esc, ".tif"))
  path_resist_comp_i <- file.path(escenarios_dir, paste0("resistencia_compuesta_escenario_", esc, ".tif"))
  
  writeRaster(resist_cat_i,  path_resist_cat_i,  overwrite = TRUE)
  writeRaster(resist_comp_i, path_resist_comp_i, overwrite = TRUE)
  
  # -----------------------------
  # 6.3. Conductancia y LCP para este escenario
  # -----------------------------
  
  cat("Calculando conductancia para escenario", esc, "...\n")
  conductancia_i <- transition(1 / resist_comp_i, transitionFunction = mean, directions = 8)
  conductancia_corr_i <- geoCorrection(conductancia_i, type = "c", multpl = FALSE)
  
  # Salida de rutas LCP para este escenario
  path_rutas_lcp_i <- file.path(escenarios_dir, paste0("rutas_LCP_2017_escenario_", esc, ".shp"))
  
  cat("Lanzando cálculo de rutas LCP para escenario", esc, "...\n")
  calcular_rutas_paralelo_lote_opt(
    conductancia_corr = conductancia_corr_i,
    coords_origen     = rutas_directas[, c("origen_x", "origen_y")],
    coords_destino    = rutas_directas[, c("destino_x", "destino_y")],
    claves            = rutas_directas$clave,
    output_rutas      = path_rutas_lcp_i,
    tamano_lote       = 50
  )
  
  cat("??? Escenario", esc, "finalizado. Rutas guardadas en:\n", path_rutas_lcp_i, "\n")
}

cat("\n?????? Todos los escenarios (1-10) han sido procesados y guardados en:\n", escenarios_dir, "\n")
