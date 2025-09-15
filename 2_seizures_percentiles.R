#######################################
#######INCAUTACIONES ################
#################################

# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(openxlsx)
# Cargar el archivo Excel
ruta_archivo <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/data/outliers_gasolina_inc/Incautaciones_GPW.xlsx"
data <- read_excel(ruta_archivo)

# Separar los conjuntos de datos según la columna "cultivo"
base_coca <- data %>% filter(cultivo == "Base de Coca")
cocaina <- data %>% filter(cultivo == "Cocaína")
hoja_coca <- data %>% filter(cultivo == "Hoja de Coca")
pasta_coca <- data %>% filter(cultivo == "Pasta de Coca")

# Cargar librerías necesarias
library(dplyr)
# Cargar librerías necesarias
library(dplyr)
library(openxlsx)

# Función para calcular los cuartiles y percentiles eliminando los ceros
calcular_cuartiles_percentiles <- function(data, cultivo) {
  # Seleccionar solo las columnas "Z_" de 2017 a 2022
  data_z <- data %>% select(starts_with("Z_2017"), starts_with("Z_2018"), 
                            starts_with("Z_2019"), starts_with("Z_2020"), 
                            starts_with("Z_2021"), starts_with("Z_2022"))
  
  # Eliminar ceros y combinar todos los valores de "Z_" en una sola columna
  valores_z <- unlist(data_z)
  valores_z <- valores_z[valores_z != 0]
  
  # Calcular cuartiles y percentiles
  resumen <- data.frame(
    Cultivo = cultivo,
    Q1 = quantile(valores_z, 0.25, na.rm = TRUE),
    Q2 = quantile(valores_z, 0.50, na.rm = TRUE),
    Q3 = quantile(valores_z, 0.75, na.rm = TRUE),
    P90 = quantile(valores_z, 0.90, na.rm = TRUE),
    P95 = quantile(valores_z, 0.95, na.rm = TRUE),
    P99 = quantile(valores_z, 0.99, na.rm = TRUE)
  )
  
  return(resumen)
}

# Calcular para cada conjunto de datos
cuartiles_percentiles_base_coca <- calcular_cuartiles_percentiles(base_coca, "Base de Coca")
cuartiles_percentiles_cocaina <- calcular_cuartiles_percentiles(cocaina, "Cocaína")
cuartiles_percentiles_hoja_coca <- calcular_cuartiles_percentiles(hoja_coca, "Hoja de Coca")
cuartiles_percentiles_pasta_coca <- calcular_cuartiles_percentiles(pasta_coca, "Pasta de Coca")

# Combinar todos los resultados en un solo DataFrame
resultados <- bind_rows(cuartiles_percentiles_base_coca, cuartiles_percentiles_cocaina, 
                        cuartiles_percentiles_hoja_coca, cuartiles_percentiles_pasta_coca)

# Crear el archivo Excel y guardar los resultados en una sola hoja
ruta_salida <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/data/outliers_gasolina_inc/Incautaciones_cuartiles_percentiles.xlsx"
write.xlsx(resultados, file = ruta_salida, sheetName = "Cuartiles_y_Percentiles", overwrite = TRUE)


#######################################
####### GASOLINA ################
#################################


# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(openxlsx)

# Cargar el archivo Excel
ruta_archivo <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/data/outliers_gasolina_inc/Gasolina_GPW.xlsx"
data <- read_excel(ruta_archivo)
colnames(data)

# Función para calcular los cuartiles y percentiles eliminando los ceros
calcular_cuartiles_percentiles <- function(data) {
  # Seleccionar solo las columnas "Z_VP_" de 2010 a 2023
  data_z <- data %>% select(starts_with("Z_VP_2010"), starts_with("Z_VP_2011"), 
                            starts_with("Z_VP_2012"), starts_with("Z_VP_2013"), 
                            starts_with("Z_VP_2014"), starts_with("Z_VP_2015"), 
                            starts_with("Z_VP_2016"), starts_with("Z_VP_2017"), 
                            starts_with("Z_VP_2018"), starts_with("Z_VP_2019"), 
                            starts_with("Z_VP_2020"), starts_with("Z_VP_2021"), 
                            starts_with("Z_VP_2022"), starts_with("Z_VP_2023"))
  
  # Eliminar ceros y combinar todos los valores de "Z_VP_" en una sola columna
  valores_z <- unlist(data_z)
  valores_z <- valores_z[valores_z != 0 & !is.na(valores_z)]
  
  # Calcular cuartiles y percentiles
  resumen <- data.frame(
    Q1 = quantile(valores_z, 0.25, na.rm = TRUE),
    Q2 = quantile(valores_z, 0.50, na.rm = TRUE),
    Q3 = quantile(valores_z, 0.75, na.rm = TRUE),
    P90 = quantile(valores_z, 0.90, na.rm = TRUE),
    P95 = quantile(valores_z, 0.95, na.rm = TRUE),
    P99 = quantile(valores_z, 0.99, na.rm = TRUE)
  )
  
  return(resumen)
}

# Calcular cuartiles y percentiles para toda la base de datos
resultados <- calcular_cuartiles_percentiles(data)

# Crear el archivo Excel y guardar los resultados
ruta_salida <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/data/outliers_gasolina_inc/Gasolina_cuartiles_percentiles.xlsx"
write.xlsx(resultados, file = ruta_salida, sheetName = "Cuartiles_y_Percentiles", overwrite = TRUE)

# Mostrar los resultados
print(resultados)
