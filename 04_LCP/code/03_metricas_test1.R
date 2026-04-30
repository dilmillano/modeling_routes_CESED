############################################################
# CORREDORES + MÉTRICAS ACUMULADAS ENTRE ESCENARIOS (1..20)
# - Construye buffers y corredores (con unión) por escenario
# - Usa paralelización para los buffers
# - Calcula:
#     * Jaccard ensemble(1..k-1) vs escenario k
#     * Hausdorff aproximado (por muestreo de puntos)
#     * Área nueva relativa
# - Genera gráfica: Jaccard vs área nueva relativa
############################################################

library(sf)
library(raster)
library(dplyr)
library(future.apply)
library(ggplot2)

#----------------------------------------------------------
# 0. Rutas y parámetros
#----------------------------------------------------------

# Carpeta base de salida
output_dir_base <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/04_LCP/output/test_2017"

# Carpeta donde están / estarán los escenarios
escenarios_dir <- file.path(output_dir_base, "escenarios")

# Raster categórico de referencia (para CRS)
path_cost_cat <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/03_cost_surface/output/cost_surface_1km_fondo.tif"

# Número total de escenarios
n_esc <- 20
escenarios <- 1:n_esc

# Distancia de buffer para construir los corredores (en unidades del CRS, idealmente metros)
buffer_dist <- 1000  # 1 km

# Archivo de salida con las métricas acumuladas
path_metricas_union <- file.path(output_dir_base, "metricas_ensemble_union_2017.csv")

#----------------------------------------------------------
# 1. CRS de referencia desde el raster categórico
#----------------------------------------------------------

cost_cat <- raster(path_cost_cat)
crs_ref  <- st_crs(crs(cost_cat))  # convierto CRS del raster a formato sf

cat("CRS de referencia:\n")
print(crs_ref)

#----------------------------------------------------------
# 2. Función: generar buffer + corredor para UN escenario
#    - Lee rutas_LCP_2017_escenario_k.shp
#    - **NO transforma** coordenadas: solo asigna CRS de referencia
#    - Hace buffer
#    - Guarda:
#        * buffer_escenario_k.shp
#        * corredor_escenario_k.shp (unión)
#    - Devuelve solo la geometría del corredor (sfc)
#----------------------------------------------------------

build_buffers_corridor <- function(k) {
  shp_rutas <- file.path(
    escenarios_dir,
    paste0("rutas_LCP_2017_escenario_", k, ".shp")
  )
  cat("\n=============================\n")
  cat("ESCENARIO", k, " - leyendo rutas:\n  ", shp_rutas, "\n")
  
  rutas <- st_read(shp_rutas, quiet = TRUE)
  
  # Importante:
  # Asumimos que las coordenadas ya están en el mismo sistema que el raster,
  # pero el shapefile no tiene CRS bien definido -> solo etiquetamos el CRS.
  cat("  - Asignando CRS de referencia a las rutas (sin transformar coordenadas)...\n")
  rutas <- st_set_crs(rutas, crs_ref)
  
  # Buffer
  cat("  - Generando buffer de", buffer_dist, "unidades...\n")
  rutas_buf <- st_buffer(rutas, dist = buffer_dist)
  
  # Guardar buffers por escenario
  path_buf <- file.path(
    escenarios_dir,
    paste0("buffer_escenario_", k, ".shp")
  )
  st_write(rutas_buf, path_buf, delete_layer = TRUE, quiet = TRUE)
  cat("  - Buffer guardado en:\n     ", path_buf, "\n")
  
  # Unión interna = corredor del escenario
  cat("  - Haciendo unión (corredor) dentro del escenario...\n")
  corredor <- suppressWarnings(st_union(rutas_buf))
  corredor <- st_make_valid(corredor)
  
  # Guardar corredor por escenario como shapefile
  corredor_sf <- st_sf(id = k, geometry = corredor)
  path_corr <- file.path(
    escenarios_dir,
    paste0("corredor_escenario_", k, ".shp")
  )
  st_write(corredor_sf, path_corr, delete_layer = TRUE, quiet = TRUE)
  cat("  - Corredor guardado en:\n     ", path_corr, "\n")
  
  return(corredor)  # sfc
}

#----------------------------------------------------------
# 3. Construir buffers + corredores para TODOS los escenarios (EN PARALELO)
#----------------------------------------------------------

cat("\nConfigurando paralelización para construir buffers/corredores...\n")
plan(multisession, workers = 30)   # ajusta si quieres usar menos núcleos
options(future.globals.maxSize = 5 * 1024^3)

cat("Construyendo buffers y corredores para", n_esc, "escenarios en paralelo...\n")

corredores_list <- future_lapply(
  escenarios,
  function(k) {
    build_buffers_corridor(k)
  },
  future.seed = TRUE
)

names(corredores_list) <- paste0("esc_", escenarios)

cat("\n??? Buffers y corredores construidos y guardados para todos los escenarios.\n")

# Volvemos a modo secuencial para el bucle acumulado
plan(sequential)

#----------------------------------------------------------
# 4. Función: Hausdorff aproximado por muestreo de puntos
#----------------------------------------------------------

hausdorff_aprox <- function(g1, g2, n = 500) {
  g1 <- st_make_valid(g1)
  g2 <- st_make_valid(g2)
  
  if (st_is_empty(g1) || st_is_empty(g2)) return(NA_real_)
  
  # Muestra de puntos dentro de cada polígono
  pts1 <- try(st_sample(g1, size = n, type = "regular"), silent = TRUE)
  pts2 <- try(st_sample(g2, size = n, type = "regular"), silent = TRUE)
  
  if (inherits(pts1, "try-error") || inherits(pts2, "try-error") ||
      length(pts1) == 0 || length(pts2) == 0) {
    return(NA_real_)
  }
  
  d12 <- st_distance(pts1, g2)
  d21 <- st_distance(pts2, g1)
  
  h12 <- max(apply(d12, 1, min))
  h21 <- max(apply(d21, 1, min))
  
  as.numeric(max(h12, h21))
}

#----------------------------------------------------------
# 5. Loop acumulado: ENSEMBLE(1..k-1) vs ESCENARIO k
#    Métricas:
#     - Jaccard
#     - Hausdorff aprox
#     - Área nueva relativa
#----------------------------------------------------------

ensemble <- corredores_list[[1]]  # empezamos con el corredor del escenario 1
area_ens <- sum(as.numeric(st_area(ensemble)), na.rm = TRUE)

resultados_union <- data.frame(
  escenario       = integer(),
  jaccard         = numeric(),
  hausdorff       = numeric(),
  area_rel_nueva  = numeric(),
  stringsAsFactors = FALSE
)

for (k in 2:n_esc) {
  cat("\n--------------------------------------------------\n")
  cat("Comparando ENSEMBLE(1..", k - 1, ") vs ESCENARIO", k, "\n")
  cat("--------------------------------------------------\n")
  
  gk <- corredores_list[[k]]  # corredor del escenario k
  
  # Unión e intersección entre ensemble y escenario k
  union_ab <- suppressWarnings(st_union(ensemble, gk))
  union_ab <- st_make_valid(union_ab)
  inter_ab <- suppressWarnings(st_intersection(ensemble, gk))
  
  area_union <- sum(as.numeric(st_area(union_ab)), na.rm = TRUE)
  area_inter <- if (length(inter_ab) == 0) 0 else sum(as.numeric(st_area(inter_ab)), na.rm = TRUE)
  
  # Índice de Jaccard
  jacc <- if (area_union == 0) NA_real_ else area_inter / area_union
  
  # Área nueva aportada por el escenario k
  area_new     <- area_union - area_ens
  area_rel_new <- if (area_union == 0) NA_real_ else area_new / area_union
  
  # Hausdorff aproximado
  hdist <- hausdorff_aprox(ensemble, gk)
  
  cat("  - Jaccard: ", jacc, "\n")
  cat("  - Hausdorff (aprox): ", hdist, "\n")
  cat("  - Área nueva relativa: ", area_rel_new, "\n")
  
  resultados_union <- rbind(
    resultados_union,
    data.frame(
      escenario      = k,
      jaccard        = jacc,
      hausdorff      = hdist,
      area_rel_nueva = area_rel_new
    )
  )
  
  # Actualizar ensemble (sumar el escenario k al conjunto acumulado)
  ensemble <- union_ab
  area_ens <- area_union
}

#----------------------------------------------------------
# 6. Guardar métricas y graficar
#----------------------------------------------------------

write.csv(resultados_union, path_metricas_union, row.names = FALSE)

cat("\n??? Métricas de ENSEMBLE (por unión) guardadas en:\n",
    path_metricas_union, "\n\n")

print(resultados_union)

# Gráfica en la pestaña Plots:
# Jaccard y área nueva relativa vs número de escenario

ggplot(resultados_union, aes(x = escenario)) +
  geom_line(aes(y = jaccard, colour = "Jaccard"), linewidth = 1) +
  geom_point(aes(y = jaccard, colour = "Jaccard"), size = 2) +
  geom_line(
    aes(y = area_rel_nueva, colour = "Área nueva relativa"),
    linetype = "dashed", linewidth = 1
  ) +
  geom_point(
    aes(y = area_rel_nueva, colour = "Área nueva relativa"),
    size = 2
  ) +
  scale_colour_manual(values = c(
    "Jaccard"             = "#440154FF",
    "Área nueva relativa" = "#21908CFF"
  )) +
  labs(
    x = "Escenario",
    y = "Valor (Jaccard / Área nueva)",
    colour = "Métrica",
    title = "Estabilidad del ensemble de rutas por número de escenarios"
  ) +
  theme_minimal(base_size = 13)

