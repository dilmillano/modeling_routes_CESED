############################################################
# ESTABILIDAD DEL ENSEMBLE DE RUTAS 2017 (ESCENARIOS 1..20)
# Métricas acumuladas (ensemble 1..k-1 vs escenario k):
#   1) Área nueva relativa (novedad de corredor)
#   2) NAL - Normalized Added Length (cambio de perímetro)
#   3) MMD - Mean Minimum Distance (distancia media de rutas nuevas al ensemble)
#
# Además:
#   - Normalización de métricas
#   - Índice combinado S
#   - Punto de corte automático y gráfica
############################################################

library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)

#----------------------------------------------------------
# 0. Rutas base y parámetros
#----------------------------------------------------------

base_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/04_LCP/output/test_2017"
esc_dir  <- file.path(base_dir, "escenarios")

n_esc    <- 20
escenarios <- 1:n_esc

# Aquí guardaremos:
path_metricas_base <- file.path(base_dir, "metricas_estabilidad_ensemble_2017.csv")
path_metricas_mmd  <- file.path(base_dir, "metricas_estabilidad_ensemble_2017_MMD.csv")

#----------------------------------------------------------
# 1. Leer buffers y hacer unión interna por escenario
#    -> buffers_union[[k]] = corredor poligonal del escenario k
#----------------------------------------------------------

buffers_union <- vector("list", n_esc)

for (i in escenarios) {
  buf_path <- file.path(esc_dir, paste0("buffer_escenario_", i, ".shp"))
  cat("Leyendo buffer del escenario", i, "...\n")
  
  bi <- st_read(buf_path, quiet = TRUE)
  
  if (i == 1) {
    crs_ref <- st_crs(bi)  # CRS de referencia
  } else {
    if (st_crs(bi) != crs_ref) {
      bi <- st_transform(bi, crs_ref)
    }
  }
  
  buffers_union[[i]] <- st_make_valid(st_union(bi))
}

cat("??? Buffers cargados y unificados correctamente.\n\n")

#----------------------------------------------------------
# 2. Área nueva relativa + NAL (Normalized Added Length)
#    ensemble(1..k-1) vs escenario k
#----------------------------------------------------------

# Función NAL: cambio relativo del perímetro del ensemble al incorporar S_k
nal_fun <- function(ensemble_prev, escenario_new) {
  b_prev <- st_boundary(ensemble_prev)
  b_new  <- st_boundary(st_union(ensemble_prev, escenario_new))
  
  L_prev <- as.numeric(st_length(b_prev))
  L_new  <- as.numeric(st_length(b_new))
  
  if (L_prev == 0) return(NA_real_)
  (L_new - L_prev) / L_prev
}

ensemble <- buffers_union[[1]]                         # ensemble inicial = escenario 1
area_ens <- as.numeric(st_area(ensemble))             # área del ensemble inicial

metricas <- data.frame(
  escenario      = integer(),
  area_rel_nueva = numeric(),
  NAL            = numeric(),
  stringsAsFactors = FALSE
)

for (k in 2:n_esc) {
  cat("\n--------------------------------------------------\n")
  cat("Comparando ensemble (1..", k - 1, ") vs escenario", k, "\n")
  cat("--------------------------------------------------\n")
  
  gk <- buffers_union[[k]]
  
  # Unión ensemble + escenario k
  new_union   <- st_union(ensemble, gk)
  area_union  <- as.numeric(st_area(new_union))
  
  # Área nueva relativa (novedad)
  area_rel <- if (area_union == 0) NA_real_ else (area_union - area_ens) / area_union
  
  # NAL
  nal_k <- nal_fun(ensemble, gk)
  
  metricas <- rbind(
    metricas,
    data.frame(
      escenario      = k,
      area_rel_nueva = area_rel,
      NAL            = nal_k
    )
  )
  
  # Actualizar ensemble para el siguiente paso
  ensemble <- new_union
  area_ens <- area_union
}

write.csv(metricas, path_metricas_base, row.names = FALSE)
cat("\n??? Métricas base (Área nueva relativa + NAL) guardadas en:\n",
    path_metricas_base, "\n\n")
print(metricas)

#----------------------------------------------------------
# 3. MMD (Mean Minimum Distance) entre rutas nuevas y ensemble
#    - ensemble(1..k-1) = corredor acumulado
#    - rutas_k = rutas del escenario k
#----------------------------------------------------------

MMD_vals <- rep(NA_real_, n_esc)   # MMD[1] queda NA

# ensemble para MMD arranca igual: solo escenario 1
ensemble <- buffers_union[[1]]

for (k in 2:n_esc) {
  cat("\n--- MMD para escenario", k, "---\n")
  
  # Leer rutas del escenario k
  rutas_path <- file.path(esc_dir, paste0("rutas_LCP_2017_escenario_", k, ".shp"))
  rutas_k <- st_read(rutas_path, quiet = TRUE)
  
  # NO reproyectamos: solo asignamos CRS de referencia
  rutas_k <- st_set_crs(rutas_k, crs_ref)
  
  # Asegurarnos de que la geometría sea LINESTRING
  geom_lines <- st_cast(st_geometry(rutas_k), "LINESTRING")
  
  # Muestrear puntos a lo largo de las rutas (~1 punto cada 5 km)
  pts_raw <- st_line_sample(geom_lines, density = 1/5000)
  pts_k   <- st_cast(pts_raw, "POINT")
  
  if (length(pts_k) == 0) {
    cat("  * Escenario", k, "sin puntos de muestreo; MMD = NA\n")
    MMD_vals[k] <- NA_real_
  } else {
    # Distancia de cada punto al ensemble anterior
    d_mat <- st_distance(pts_k, ensemble)
    
    # Para cada punto: distancia mínima al ensemble (ignorando NA)
    d_min <- apply(d_mat, 1, function(x) {
      vx <- as.numeric(x)
      if (all(is.na(vx))) NA_real_ else min(vx, na.rm = TRUE)
    })
    
    if (all(is.na(d_min))) {
      cat("  * Todas las distancias son NA; MMD = NA\n")
      MMD_vals[k] <- NA_real_
    } else {
      MMD_vals[k] <- mean(d_min[is.finite(d_min)], na.rm = TRUE)
      cat("  * MMD (m):", MMD_vals[k], "\n")
    }
  }
  
  # Actualizar ensemble con el corredor del escenario k
  ensemble <- st_make_valid(st_union(ensemble, buffers_union[[k]]))
}

# Normalizar MMD entre 0 y 1 (ignorando NA)
max_MMD <- max(MMD_vals, na.rm = TRUE)
MMD_norm_all <- MMD_vals / max_MMD

#----------------------------------------------------------
# 4. Unir métricas, normalizar NAL y guardar tabla extendida
#----------------------------------------------------------

metricas_ext <- metricas %>%
  mutate(
    # Normalización interna de NAL (0-1, respecto al máximo)
    NAL_norm = NAL / max(NAL, na.rm = TRUE),
    # Alinear MMD_norm con el número de escenario
    MMD_norm = MMD_norm_all[escenario]
  )

write.csv(metricas_ext, path_metricas_mmd, row.names = FALSE)
cat("\n??? Métricas extendidas (Área nueva relativa + NAL + MMD) guardadas en:\n",
    path_metricas_mmd, "\n\n")
print(metricas_ext)

#----------------------------------------------------------
# 5. Normalización final + índice combinado S
#----------------------------------------------------------

resultados <- metricas_ext %>%
  mutate(
    # Normalizar área nueva relativa con su máximo
    area_rel_nueva_norm = area_rel_nueva / max(area_rel_nueva, na.rm = TRUE),
    # Cortar negativos de NAL y MMD a 0
    NAL_norm_aj = pmax(NAL_norm, 0),
    MMD_norm_aj = pmax(MMD_norm, 0),
    # Índice combinado S: promedio de las 3 métricas normalizadas
    S_indice = (area_rel_nueva_norm + NAL_norm_aj + MMD_norm_aj) / 3
  )

#----------------------------------------------------------
# 6. Punto de corte automático
#    Primer escenario k donde S_k, S_{k+1}, S_{k+2} < umbral
#----------------------------------------------------------

umbral <- 0.05
S_vec  <- resultados$S_indice
esc    <- resultados$escenario

cut_idx <- NA_integer_

if (length(S_vec) >= 3) {
  for (i in 1:(length(S_vec) - 2)) {
    if (all(S_vec[i:(i + 2)] < umbral, na.rm = TRUE)) {
      cut_idx <- i
      break
    }
  }
}

if (is.na(cut_idx)) {
  corte_escenario <- NA
  mensaje_corte <- "??? No se encontró un punto de corte con 3 escenarios consecutivos bajo el umbral."
} else {
  corte_escenario <- esc[cut_idx]
  mensaje_corte <- paste0(
    "??? Punto de corte sugerido: escenario ",
    corte_escenario,
    " (S_k < ", umbral,
    " durante al menos 3 escenarios consecutivos)."
  )
}

cat(mensaje_corte, "\n")

#----------------------------------------------------------
# 7. Gráfica conjunta (Área relativa, NAL, MMD, S)
#----------------------------------------------------------

datos_long <- resultados %>%
  select(
    escenario,
    `Área nueva relativa` = area_rel_nueva_norm,
    `NAL normalizado`     = NAL_norm_aj,
    `MMD normalizado`     = MMD_norm_aj,
    `Índice combinado S`  = S_indice
  ) %>%
  pivot_longer(
    cols      = -escenario,
    names_to  = "Metrica",
    values_to = "valor"
  )

p <- ggplot(datos_long, aes(x = escenario, y = valor, colour = Metrica)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c(
      "Área nueva relativa" = "#1b9e77",
      "NAL normalizado"     = "#d95f02",
      "MMD normalizado"     = "#7570b3",
      "Índice combinado S"  = "#e7298a"
    )
  ) +
  labs(
    x = "Escenario",
    y = "Valor relativo / normalizado",
    title = "Estabilidad del ensemble de rutas por número de escenarios",
    colour = "Métrica"
  ) +
  theme_minimal(base_size = 13)

if (!is.na(corte_escenario)) {
  p <- p +
    geom_vline(xintercept = corte_escenario,
               linetype   = "dashed",
               colour     = "grey30") +
    annotate(
      "text",
      x     = corte_escenario,
      y     = 1.02,
      label = paste0("Corte ~ ", corte_escenario),
      vjust = -0.3,
      hjust = 0.5,
      size  = 4
    )
}

print(p)
