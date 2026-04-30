############################################################
# MÉTRICAS DE ESTABILIDAD DEL ENSEMBLE DE RUTAS
# - Usa buffers YA EXISTENTES en la carpeta de escenarios
# - Calcula:
#      1) Área nueva relativa (Novedad)
#      2) NAL - Normalized Added Length
#      3) BDI - Boundary Displacement Index
############################################################

library(sf)
library(dplyr)
library(ggplot2)

#----------------------------------------------------------
# 0. RUTAS Y PARÁMETROS
#----------------------------------------------------------
base_dir <- "C:/Users/d.millanorduz/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/04_LCP/output/test_2017"
esc_dir  <- file.path(base_dir, "escenarios")
n_esc    <- 20

# Archivo donde guardaremos resultados
path_out <- file.path(base_dir, "metricas_estabilidad_ensemble_2017.csv")

#----------------------------------------------------------
# 1. CARGAR TODOS LOS BUFFERS EXISTENTES
#----------------------------------------------------------

buffers_union <- vector("list", n_esc)

for (i in 1:n_esc) {
  buf_path <- file.path(esc_dir, paste0("buffer_escenario_", i, ".shp"))
  cat("Leyendo buffer del escenario", i, "...\n")
  
  bi <- st_read(buf_path, quiet = TRUE)
  
  # Guardar el CRS del primer escenario
  if (i == 1) {
    crs_ref <- st_crs(bi)
  } else {
    # Asegurar coherencia de CRS
    if (st_crs(bi) != crs_ref) {
      bi <- st_transform(bi, crs_ref)
    }
  }
  
  # Unión interna del escenario
  buffers_union[[i]] <- st_make_valid(st_union(bi))
}

cat("??? Buffers cargados y unificados correctamente.\n\n")

#----------------------------------------------------------
# 2. MÉTRICAS ADICIONALES
#----------------------------------------------------------

# ---- NAL: Normalized Added Length ----
nal_fun <- function(ensemble_prev, escenario_new) {
  b_prev <- st_boundary(ensemble_prev)
  b_new  <- st_boundary(st_union(ensemble_prev, escenario_new))
  
  L_prev <- as.numeric(st_length(b_prev))
  L_new  <- as.numeric(st_length(b_new))
  
  if (L_prev == 0) return(NA_real_)
  return((L_new - L_prev) / L_prev)
}

# ---- BDI: Boundary Displacement Index ----
bdi_fun <- function(ensemble_prev, escenario_new, n = 150) {
  g_union <- st_union(ensemble_prev, escenario_new)
  
  pts <- st_sample(g_union, size = n, type = "regular")
  d_prev <- as.numeric(st_distance(pts, ensemble_prev))
  d_new  <- as.numeric(st_distance(pts, escenario_new))
  
  mean(abs(d_prev - d_new), na.rm = TRUE)
}

#----------------------------------------------------------
# 3. LOOP ACUMULADO (1..k-1) vs k
#----------------------------------------------------------

ensemble <- buffers_union[[1]]
area_ens <- as.numeric(st_area(ensemble))

resultados <- data.frame(
  escenario      = integer(),
  area_rel_nueva = numeric(),
  NAL            = numeric(),
  BDI            = numeric(),
  stringsAsFactors = FALSE
)

for (k in 2:n_esc) {
  cat("\n--------------------------------------------------\n")
  cat("Comparando ensemble (1..", k - 1, ") vs escenario", k, "\n")
  cat("--------------------------------------------------\n")
  
  gk <- buffers_union[[k]]
  
  # Unión
  new_union <- st_union(ensemble, gk)
  area_union <- as.numeric(st_area(new_union))
  
  # Área nueva relativa
  area_rel <- (area_union - area_ens) / area_union
  
  # NAL
  nal_k <- nal_fun(ensemble, gk)
  
  # BDI
  bdi_k <- bdi_fun(ensemble, gk)
  
  resultados <- rbind(
    resultados,
    data.frame(
      escenario      = k,
      area_rel_nueva = area_rel,
      NAL            = nal_k,
      BDI            = bdi_k
    )
  )
  
  # Actualizar ensemble
  ensemble  <- new_union
  area_ens  <- area_union
}

#----------------------------------------------------------
# 4. GUARDAR RESULTADOS
#----------------------------------------------------------

write.csv(resultados, path_out, row.names = FALSE)

cat("\n??? Métricas guardadas en:\n", path_out, "\n\n")
print(resultados)


#######################################################


library(ggplot2)
library(dplyr)
library(tidyr)

# Cargar resultados (si ya los tienes en memoria, puedes omitir esta línea)
# resultados <- read.csv(path_out)

df_plot <- resultados %>%
  mutate(
    BDI_norm = (BDI - min(BDI)) / (max(BDI) - min(BDI))  # normalizado 0-1
  ) %>%
  select(escenario, area_rel_nueva, NAL, BDI_norm) %>%
  pivot_longer(
    cols = c(area_rel_nueva, NAL, BDI_norm),
    names_to = "metrica",
    values_to = "valor"
  )

ggplot(df_plot, aes(x = escenario, y = valor, color = metrica)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "area_rel_nueva" = "#1b9e77",
      "NAL"            = "#d95f02",
      "BDI_norm"       = "#7570b3"
    ),
    labels = c(
      "area_rel_nueva" = "Área nueva relativa",
      "NAL"            = "NAL (Normalized Added Length)",
      "BDI_norm"       = "BDI normalizado"
    )
  ) +
  labs(
    title = "Evolución de métricas de estabilidad por escenario",
    x = "Escenario",
    y = "Valor normalizado / relativo",
    color = "Métrica"
  ) +
  theme_minimal(base_size = 14)
