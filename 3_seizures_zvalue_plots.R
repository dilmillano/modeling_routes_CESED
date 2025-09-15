library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# Definir la ruta del shapefile
shapefile_path <- "C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca_GPWZ_PT_4326.shp"

# Cargar shapefile
incautaciones <- st_read(shapefile_path)

# Preparar datos
data_long <- incautaciones %>%
  select(cultivo, Z_2017, Z_2018, Z_2019, Z_2020, Z_2021, Z_2022) %>%
  pivot_longer(cols = starts_with("Z_"), names_to = "year", values_to = "Z_value") %>%
  filter(!is.na(Z_value), Z_value != 0) %>%
  mutate(
    cultivo = case_when(
      cultivo == "Base de Coca" ~ "Cocaine base",
      cultivo == "Cocaína" ~ "Cocaine",         # 👈 Aquí corregimos bien
      cultivo == "Hoja de Coca" ~ "Coca leaf",
      cultivo == "Pasta de Coca" ~ "Coca paste",
      TRUE ~ as.character(cultivo)
    ),
    Z_value_cbrt = Z_value^(1/3)
  ) %>%
  filter(!is.na(Z_value_cbrt), is.finite(Z_value_cbrt)) %>%   # Quitar valores raros
  mutate(
    cultivo = factor(cultivo, levels = c("Coca leaf", "Coca paste", "Cocaine base", "Cocaine"))
  )

# Definir colores
colores_cultivos <- c(
  "Coca leaf" = "#ff7f00",     # Naranja
  "Coca paste" = "#2f66ff",    # Azul
  "Cocaine base" = "#ff2bf4",  # Fucsia
  "Cocaine" = "gray"           # Gris
)

# Calcular percentil 80
percentiles_80 <- data_long %>%
  group_by(cultivo) %>%
  summarise(p80 = quantile(Z_value_cbrt, 0.8, na.rm = TRUE))

# Crear histograma
ggplot(data_long, aes(x = Z_value_cbrt, fill = cultivo)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~cultivo, scales = "free") +
  geom_vline(data = percentiles_80, aes(xintercept = p80, linetype = "P80"), color = "red", size = 1) +
  scale_fill_manual(values = colores_cultivos) +
  scale_linetype_manual(name = "", values = c("P80" = "dashed")) +
  labs(
    title = "",
    x = "Z Value",
    y = "Frequency",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )

# Crear boxplot
ggplot(data_long, aes(x = cultivo, y = Z_value_cbrt, fill = cultivo)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(data = percentiles_80, aes(yintercept = p80, linetype = "P80"), color = "red", size = 1) +
  scale_fill_manual(values = colores_cultivos) +
  scale_linetype_manual(name = "", values = c("P80" = "dashed")) +
  labs(
    title = "",
    x = "Crop Type",
    y = "Z Value",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )
########################################################################### UNIDOS PARA QUE QUEDEN MEJOR
library(patchwork)

# Función corregida para eliminar fill en boxplot y dejarlo solo en histograma
graficar_combina_cultivo <- function(cultivo_nombre) {
  
  datos_cultivo <- data_long %>%
    filter(cultivo == cultivo_nombre)
  
  percentil_cultivo <- percentiles_80 %>%
    filter(cultivo == cultivo_nombre)
  
  # Boxplot SIN leyenda de fill
  box <- ggplot(datos_cultivo, aes(y = "", x = Z_value_cbrt, fill = cultivo)) +
    geom_boxplot(alpha = 0.7, width = 0.4) +
    geom_vline(aes(xintercept = percentil_cultivo$p80, linetype = "P80"), color = "red", size = 1) +
    scale_fill_manual(values = colores_cultivos, guide = "none") +  # 👈 no guia para fill aquí
    scale_linetype_manual(name = "", values = c("P80" = "dashed")) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  # Histograma CON leyenda de fill
  hist <- ggplot(datos_cultivo, aes(x = Z_value_cbrt, fill = cultivo)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    geom_vline(aes(xintercept = percentil_cultivo$p80, linetype = "P80"), color = "red", size = 1) +
    scale_fill_manual(values = colores_cultivos) +  # 👈 sí queremos leyenda aquí
    scale_linetype_manual(name = "", values = c("P80" = "dashed")) +
    labs(x = "Z Value", y = "Count", fill = " ") +
    theme_minimal() +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Unir boxplot arriba + histograma abajo
  (box / hist) + plot_layout(heights = c(1, 3)) 
}

# Crear gráficos individuales
grafico_coca_leaf <- graficar_combina_cultivo("Coca leaf")
grafico_coca_paste <- graficar_combina_cultivo("Coca paste")
grafico_cocaine_base <- graficar_combina_cultivo("Cocaine base")
grafico_cocaine <- graficar_combina_cultivo("Cocaine")

# Unir todos
final_plot <- (grafico_coca_leaf | grafico_coca_paste) / (grafico_cocaine_base | grafico_cocaine) +
  plot_layout(guides = "collect") &   # Recolectar una sola vez la leyenda
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Mostrar el gráfico
final_plot
