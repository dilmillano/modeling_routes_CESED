############################################################
# CALCULAR MMD (Mean Minimum Distance) Y GRAFICAR
#  - Usa buffers y rutas ya guardadas en disco
#  - Añade MMD a metricas_estabilidad_ensemble_2017.csv
#  - Genera gráfica con: área nueva relativa, NAL y MMD
############################################################

library(sf)
library(dplyr)
library(ggplot2)

#------------------------------------------
# 1. Rutas y parámetros
#------------------------------------------

output_dir_base <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/04_LCP/output/test_2017"
escenarios_dir  <- file.path(output_dir_base, "escenarios")

n_esc <- 20
escenarios <- 1:n_esc

#------------------------------------------
# 2. Leer buffers y construir corredor por escenario
#    (unión interna, igual que antes)
#------------------------------------------

buffers_union <- vector("list", n_esc)

for (i in escenarios) {
  path_buf <- file.path(escenarios_dir, paste0("buffer_escenario_", i, ".shp"))
  cat("Leyendo buffer del escenario", i, "...\n")
  b_i <- st_read(path_buf, quiet = TRUE)
  
  if (i == 1) {
    crs_ref <- st_crs(b_i)   # CRS de referencia = CRS de los buffers
  } else {
    if (st_crs(b_i) != crs_ref) {
      b_i <- st_transform(b_i, crs_ref)
    }
  }
  
  buffers_union[[i]] <- st_make_valid(st_union(b_i))
}

#------------------------------------------
# 3. Calcular MMD acumulado:
#    ensemble(1..k-1) vs rutas del escenario k
#------------------------------------------

MMD_vals <- rep(NA_real_, n_esc)  # MMD[1] se queda en NA

# ensemble inicial = corredor del escenario 1
ensemble <- buffers_union[[1]]

for (k in 2:n_esc) {
  cat("\n--- MMD para escenario", k, "---\n")
  
  # Leer rutas del escenario k
  rutas_path <- file.path(escenarios_dir, paste0("rutas_LCP_2017_escenario_", k, ".shp"))
  rutas_k <- st_read(rutas_path, quiet = TRUE)
  
  # NO transformamos, solo asignamos CRS del buffer
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
    
    # Para cada punto, distancia mínima al ensemble (ignorando NA)
    d_min <- apply(d_mat, 1, function(x) {
      vx <- as.numeric(x)
      if (all(is.na(vx))) NA_real_ else min(vx, na.rm = TRUE)
    })
    
    # Si todas las distancias son NA, MMD = NA
    if (all(is.na(d_min))) {
      cat("  * Todas las distancias son NA; MMD = NA\n")
      MMD_vals[k] <- NA_real_
    } else {
      MMD_vals[k] <- mean(d_min[is.finite(d_min)], na.rm = TRUE)
      cat("  * MMD (m):", MMD_vals[k], "\n")
    }
  }
  
  # Actualizar ensemble = union(ensemble, corredor_k)
  ensemble <- st_make_valid(st_union(ensemble, buffers_union[[k]]))
}


# Normalizar MMD entre 0 y 1
max_MMD <- max(MMD_vals, na.rm = TRUE)
MMD_norm_all <- MMD_vals / max_MMD

#------------------------------------------
# 4. Unir con las métricas previas y normalizar NAL
#------------------------------------------

path_prev <- file.path(output_dir_base, "metricas_estabilidad_ensemble_2017.csv")
metricas_prev <- read.csv(path_prev)

# metricas_prev tiene escenarios 2..20
metricas_3 <- metricas_prev %>%
  mutate(
    NAL_norm  = NAL / max(NAL, na.rm = TRUE),
    MMD_norm  = MMD_norm_all[escenario]
  )

# (Opcional) guardar versión extendida con MMD
path_out_3 <- file.path(output_dir_base, "metricas_estabilidad_ensemble_2017_MMD.csv")
write.csv(metricas_3, path_out_3, row.names = FALSE)
cat("\n??? Métricas extendidas (con MMD) guardadas en:\n", path_out_3, "\n\n")

print(metricas_3)

#------------------------------------------
# 5. Gráfica con las 3 métricas
#    - Área nueva relativa
#    - NAL normalizado
#    - MMD normalizado
#------------------------------------------

ggplot(metricas_3, aes(x = escenario)) +
  geom_line(aes(y = area_rel_nueva, colour = "Área nueva relativa"), linewidth = 1) +
  geom_point(aes(y = area_rel_nueva, colour = "Área nueva relativa"), size = 2) +
  geom_line(aes(y = NAL_norm, colour = "NAL normalizado"), linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = NAL_norm, colour = "NAL normalizado"), size = 2) +
  geom_line(aes(y = MMD_norm, colour = "MMD normalizado"), linetype = "dotted", linewidth = 1) +
  geom_point(aes(y = MMD_norm, colour = "MMD normalizado"), size = 2) +
  scale_colour_manual(values = c(
    "Área nueva relativa" = "#1b9e77",
    "NAL normalizado"     = "#d95f02",
    "MMD normalizado"     = "#7570b3"
  )) +
  labs(
    x = "Escenario",
    y = "Valor relativo / normalizado",
    colour = "Métrica",
    title = "Evolución de métricas de estabilidad por escenario"
  ) +
  theme_minimal(base_size = 13)



#########################################################################################

library(dplyr)
library(ggplot2)
library(tidyr)

# 1. Load metrics with MMD already calculated
path_out_3 <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/04_LCP/output/test_2017/metricas_estabilidad_ensemble_2017_MMD.csv"
resultados <- read.csv(path_out_3)

# 2. Normalize / adjust metrics
resultados <- resultados %>%
  mutate(
    rel_new_area_norm = area_rel_nueva / max(area_rel_nueva, na.rm = TRUE),
    NAL_norm_adj      = pmax(NAL_norm, 0),
    MMD_norm_adj      = pmax(MMD_norm, 0),
    S_index           = (rel_new_area_norm + NAL_norm_adj + MMD_norm_adj) / 3
  )

# ---------- CUTPOINT ----------
threshold <- 0.05
S_vec  <- resultados$S_index
esc    <- resultados$escenario

cut_idx <- NA_integer_
if (length(S_vec) >= 3) {
  for (i in 1:(length(S_vec) - 2)) {
    if (all(S_vec[i:(i+2)] < threshold, na.rm = TRUE)) {
      cut_idx <- i
      break
    }
  }
}
cutoff_scenario <- if (!is.na(cut_idx)) esc[cut_idx] else NA

# ---------- LONG FORMAT FOR PLOT ----------
datos_long <- resultados %>%
  select(
    scenario               = escenario,
    `Combined index S`     = S_index,
    `Normalized MMD`       = MMD_norm_adj,
    `Normalized NAL`       = NAL_norm_adj,
    `Relative new area`    = rel_new_area_norm
  ) %>%
  pivot_longer(
    cols      = -scenario,
    names_to  = "Metric",
    values_to = "value"
  )

# ---------- PLOT WITH SHAPES + LINESTYLES ----------

p <- ggplot(datos_long, aes(x = scenario, y = value,
                            color = Metric, shape = Metric, linetype = Metric)) +
  
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8, alpha = 0.7) +   # símbolos más pequeños y suaves
  
  scale_color_manual(values = c(
    "Combined index S"  = "#e7298a",
    "Normalized MMD"    = "#7570b3",
    "Normalized NAL"    = "#d95f02",
    "Relative new area" = "#1b9e77"
  )) +
  
  scale_shape_manual(values = c(
    "Combined index S"  = 16, # circle
    "Normalized MMD"    = 15, # square
    "Normalized NAL"    = 17, # triangle
    "Relative new area" = 18  # diamond
  )) +
  
  scale_linetype_manual(values = c(
    "Combined index S"  = "solid",
    "Normalized MMD"    = "dashed",
    "Normalized NAL"    = "dotted",
    "Relative new area" = "longdash"
  )) +
  
  labs(
    x = "Scenario",
    y = "Normalized / Relative Value",
    title = "Evolution of Stability Metrics Across Scenarios",
    color = "Metric",
    shape = "Metric",
    linetype = "Metric"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18),
    legend.key.size = unit(0.8, "lines")   # legend more compact
  )

# Cutoff line
if (!is.na(cutoff_scenario)) {
  p <- p +
    geom_vline(xintercept = cutoff_scenario,
               linetype = "dashed",
               colour   = "grey30") +
    annotate(
      "text",
      x = cutoff_scenario,
      y = 1.05,
      label = paste0("Cutoff ~ ", cutoff_scenario),
      vjust = -0.4,
      hjust = 0.5,
      size  = 4
    )
}

print(p)
