# Cargar librer?as
library(sf)
library(dplyr)

# Leer el shapefile
shapefile <- st_read("C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca_GPWZ_PT.shp")

colnames(shapefile)

############################################
############################################
# Cargar librerías
library(sf)
library(dplyr)

# Leer el shapefile
shapefile <- st_read("C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca_GPWZ_PT.shp")

colnames(shapefile)

# Lista de países de interés
paises <- c("Ecuador", "Perú", "Brasil", "Venezuela")

# Paso 2: Crear datasets por cada país y tipo de cultivo
cultivo_datasets <- list()

for (pais in paises) {
  for (cultivo_tipo in unique(shapefile$cultivo)) {
    # Filtrar por país y tipo de cultivo
    cultivo_dataset <- shapefile %>%
      filter(country == pais & cultivo == cultivo_tipo)
    
    # Seleccionar las columnas que contienen los valores "Z_2017" a "Z_2022"
    cultivo_dataset <- cultivo_dataset %>%
      select(cultivo, Z_2017, Z_2018, Z_2019, Z_2020, Z_2021, Z_2022)
    
    # Eliminar los ceros en todas las columnas "Z_2017" a "Z_2022"
    valores <- c(cultivo_dataset$Z_2017, cultivo_dataset$Z_2018, cultivo_dataset$Z_2019, 
                 cultivo_dataset$Z_2020, cultivo_dataset$Z_2021, cultivo_dataset$Z_2022)
    
    # Filtrar los valores diferentes de cero
    valores_sin_ceros <- valores[valores != 0]
    
    # Verificar si hay suficientes datos para calcular quintiles y cuartiles (mínimo 4 datos)
    if (length(valores_sin_ceros) >= 4) {
      # Calcular los quintiles
      quintiles <- quantile(valores_sin_ceros, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
      
      # Calcular los cuartiles
      cuartiles <- quantile(valores_sin_ceros, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
      
      # Almacenar los resultados en una lista con el formato 'país_cultivo'
      cultivo_datasets[[paste(pais, cultivo_tipo, sep = "_")]] <- list(quintiles = quintiles, cuartiles = cuartiles)
    } else {
      # Si no hay suficientes datos, asignar NA
      cultivo_datasets[[paste(pais, cultivo_tipo, sep = "_")]] <- list(quintiles = NA, cuartiles = NA)
    }
  }
}

# Paso 3: Mostrar los resultados
for (key in names(cultivo_datasets)) {
  cat("\nResultados para:", key, "\n")
  
  # Verificar si el cálculo de quintiles fue realizado
  if (!is.null(cultivo_datasets[[key]]$quintiles)) {
    cat("Quintiles:\n")
    print(cultivo_datasets[[key]]$quintiles)
    
    cat("Cuartiles:\n")
    print(cultivo_datasets[[key]]$cuartiles)
  } else {
    cat("No hay suficientes datos para calcular quintiles o cuartiles.\n")
  }
}
############################################
######POR GRUPOS DE FRONTERAS######
############################################

# Cargar librerías
library(sf)
library(dplyr)

# Leer el shapefile
shapefile <- st_read("C:/Users/diana/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca_GPWZ_PT.shp")

colnames(shapefile)

# Definir los grupos de países
grupo1 <- c("Ecuador", "Perú")
grupo2 <- c("Brasil", "Venezuela")

# Paso 2: Crear datasets por cada grupo de países y tipo de cultivo
cultivo_datasets <- list()

for (grupo in list(grupo1, grupo2)) {
  # Nombrar el grupo
  grupo_nombre <- ifelse(identical(grupo, grupo1), "Ecuador_Perú", "Brasil_Venezuela")
  
  for (cultivo_tipo in unique(shapefile$cultivo)) {
    # Filtrar por grupo de países y tipo de cultivo
    cultivo_dataset <- shapefile %>%
      filter(country %in% grupo & cultivo == cultivo_tipo)
    
    # Seleccionar las columnas que contienen los valores "Z_2017" a "Z_2022"
    cultivo_dataset <- cultivo_dataset %>%
      select(cultivo, Z_2017, Z_2018, Z_2019, Z_2020, Z_2021, Z_2022)
    
    # Eliminar los ceros en todas las columnas "Z_2017" a "Z_2022"
    valores <- c(cultivo_dataset$Z_2017, cultivo_dataset$Z_2018, cultivo_dataset$Z_2019, 
                 cultivo_dataset$Z_2020, cultivo_dataset$Z_2021, cultivo_dataset$Z_2022)
    
    # Filtrar los valores diferentes de cero
    valores_sin_ceros <- valores[valores != 0]
    
    # Verificar si hay suficientes datos para calcular quintiles y cuartiles (mínimo 4 datos)
    if (length(valores_sin_ceros) >= 4) {
      # Calcular los quintiles
      quintiles <- quantile(valores_sin_ceros, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
      
      # Calcular los cuartiles
      cuartiles <- quantile(valores_sin_ceros, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
      
      # Almacenar los resultados en una lista con el formato 'grupo_cultivo'
      cultivo_datasets[[paste(grupo_nombre, cultivo_tipo, sep = "_")]] <- list(quintiles = quintiles, cuartiles = cuartiles)
    } else {
      # Si no hay suficientes datos, asignar NA
      cultivo_datasets[[paste(grupo_nombre, cultivo_tipo, sep = "_")]] <- list(quintiles = NA, cuartiles = NA)
    }
  }
}

# Paso 3: Mostrar los resultados
for (key in names(cultivo_datasets)) {
  cat("\nResultados para:", key, "\n")
  
  # Verificar si el cálculo de quintiles fue realizado
  if (!is.null(cultivo_datasets[[key]]$quintiles)) {
    cat("Quintiles:\n")
    print(cultivo_datasets[[key]]$quintiles)
    
    cat("Cuartiles:\n")
    print(cultivo_datasets[[key]]$cuartiles)
  } else {
    cat("No hay suficientes datos para calcular quintiles o cuartiles.\n")
  }
}

############################################
############################################
############################################
# Paso 2: Crear datasets por cada tipo de la columna "cultivo"
tipos_cultivo <- unique(shapefile$cultivo)

# Crear una lista para almacenar los datasets por tipo de cultivo
cultivo_datasets <- list()

for (cultivo_tipo in tipos_cultivo) {
  # Filtrar por cada tipo de cultivo
  cultivo_dataset <- shapefile %>% filter(cultivo == cultivo_tipo)
  
  # Seleccionar las columnas que contienen los valores "Z_2017" a "Z_2022"
  cultivo_dataset <- cultivo_dataset %>% select(cultivo, Z_2017, Z_2018, Z_2019, Z_2020, Z_2021, Z_2022)
  
  # Eliminar los ceros en todas las columnas "Z_2017" a "Z_2022"
  valores <- c(cultivo_dataset$Z_2017, cultivo_dataset$Z_2018, cultivo_dataset$Z_2019, 
               cultivo_dataset$Z_2020, cultivo_dataset$Z_2021, cultivo_dataset$Z_2022)
  
  # Filtrar los valores diferentes de cero
  valores_sin_ceros <- valores[valores != 0]
  
  # Verificar si hay suficientes datos para calcular quintiles y cuartiles (m?nimo 4 datos)
  if (length(valores_sin_ceros) >= 4) {
    # Calcular los quintiles
    quintiles <- quantile(valores_sin_ceros, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
    
    # Calcular los cuartiles
    cuartiles <- quantile(valores_sin_ceros, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
    
    # Almacenar los resultados
    cultivo_datasets[[cultivo_tipo]] <- list(quintiles = quintiles, cuartiles = cuartiles)
  } else {
    # Si no hay suficientes datos, asignar NA
    cultivo_datasets[[cultivo_tipo]] <- list(quintiles = NA, cuartiles = NA)
  }
}

# Paso 3: Mostrar los resultados
for (cultivo_tipo in tipos_cultivo) {
  cat("\nResultados para:", cultivo_tipo, "\n")
  
  # Verificar si el c?lculo de quintiles fue realizado
  if (!is.null(cultivo_datasets[[cultivo_tipo]]$quintiles)) {
    cat("Quintiles:\n")
    print(cultivo_datasets[[cultivo_tipo]]$quintiles)
    
    cat("Cuartiles:\n")
    print(cultivo_datasets[[cultivo_tipo]]$cuartiles)
  } else {
    cat("No hay suficientes datos para calcular quintiles o cuartiles.\n")
  }
}


#########################################################################
# Filtrar el cultivo de "Coca?na"
shapefile_cocaina <- shapefile %>%
  filter(cultivo == "Coca?na")

# Lista de a?os
years <- 2017:2022

# Crear nuevas columnas 'OUT_XX' para cada a?o y aplicar las condiciones
for (year in years) {
  year_str <- substr(as.character(year), 3, 4) # '17', '18', etc.
  
  # Crear la nueva columna OUT_XX con las condiciones
  shapefile_cocaina <- shapefile_cocaina %>%
    mutate(!!paste0("OUT_", year_str) := ifelse(
      (lugar == "TIERRA" & !!sym(paste0("Z_", year)) > 10) | 
        (lugar == "MAR" & cantidd > 1000),
      1, 0
    ))
}

colnames(shapefile_cocaina)
summary(shapefile_cocaina)

# Guardar el nuevo shapefile con las columnas adicionales
st_write(shapefile_cocaina, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/outliers/Incautaciones_cocaina.shp")


#########################################################################
# Leer el shapefile
shapefile <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca_GPWZ_PT.shp")

# Funci?n para procesar el shapefile y aplicar condiciones
process_cultivo <- function(cultivo_name, z_values, output_path) {
  
  # Filtrar el cultivo espec?fico
  shapefile_filtered <- shapefile %>%
    filter(cultivo == cultivo_name)
  
  # Lista de a?os
  years <- 2017:2022
  
  # Crear nuevas columnas 'OUT_XX' para cada a?o
  for (year in years) {
    year_str <- substr(as.character(year), 3, 4) # '17', '18', etc.
    
    # Inicializar la columna OUT_XX con 0
    shapefile_filtered[[paste0("OUT_", year_str)]] <- 0
    
    # Obtener el valor de Z correspondiente al cultivo y a?o
    z_value <- z_values[[as.character(year)]]
    
    # Filtrar por a?o espec?fico
    shapefile_filtered_year <- shapefile_filtered %>%
      filter(year == !!year)
    
    # Verificamos si las columnas necesarias existen en el shapefile
    if (paste0("Z_", year) %in% names(shapefile_filtered_year) & "cantidd" %in% names(shapefile_filtered_year)) {
      
      # Aplicar condiciones para TIERRA y MAR
      shapefile_filtered_year <- shapefile_filtered_year %>%
        mutate(!!paste0("OUT_", year_str) := ifelse(
          (lugar == "TIERRA" & !!sym(paste0("Z_", year)) > z_value) | 
            (lugar == "MAR" & cantidd > 1000),
          1, 0
        ))
      
      # Agregar los resultados al shapefile original
      shapefile_filtered[[paste0("OUT_", year_str)]][shapefile_filtered$year == year] <- shapefile_filtered_year[[paste0("OUT_", year_str)]]
    }
  }
  
  # Guardar el nuevo shapefile con las columnas adicionales
  st_write(shapefile_filtered, output_path)
  
  # Imprimir un mensaje de ?xito
  print(paste("Shapefile guardado correctamente en", output_path))
}

# Valores de Z para cada cultivo
z_values_base_de_coca <- list("2017" = -0.01, "2018" = -0.01, "2019" = 0, "2020" = -0.01, "2021" = -0.01, "2022" = -0.01)
z_values_cocaina <- list("2017" = 10, "2018" = 10, "2019" = 10, "2020" = 10, "2021" = 10, "2022" = 10)
z_values_hoja_de_coca <- list("2017" = 0.06, "2018" = 0.06, "2019" = 0.06, "2020" = 0.06, "2021" = 0.06, "2022" = 0.06)
z_values_pasta_de_coca <- list("2017" = -0.009, "2018" = -0.009, "2019" = -0.009, "2020" = -0.009, "2021" = -0.009, "2022" = -0.009)

# Procesar los shapefiles para cada cultivo y guardarlos
process_cultivo("Base de Coca", z_values_base_de_coca, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total_v2/outliers/Incautaciones_base_de_coca.shp")
process_cultivo("Coca?na", z_values_cocaina, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total_v2/outliers/Incautaciones_cocaina.shp")
process_cultivo("Hoja de Coca", z_values_hoja_de_coca, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total_v2/outliers/Incautaciones_hoja_de_coca.shp")
process_cultivo("Pasta de Coca", z_values_pasta_de_coca, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total_v2/outliers/Incautaciones_pasta_de_coca.shp")


###########################################################################

######################################
############PAR AEL RESTO##########
######################################



# Cargar librer?as
library(sf)
library(dplyr)

# Leer el shapefile original
shapefile <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca_GPWZ_PT.shp")

# Funci?n para crear shapefiles con las condiciones para cada cultivo
process_cultivo <- function(cultivo_name, tierra_value, output_path) {
  
  # Filtrar el cultivo espec?fico
  shapefile_filtered <- shapefile %>%
    filter(cultivo == cultivo_name)
  
  # Lista de a?os
  years <- 2017:2022
  
  # Crear nuevas columnas 'OUT_XX' para cada a?o y aplicar las condiciones
  for (year in years) {
    year_str <- substr(as.character(year), 3, 4) # '17', '18', etc.
    
    # Crear la nueva columna OUT_XX con las condiciones
    shapefile_filtered <- shapefile_filtered %>%
      mutate(!!paste0("OUT_", year_str) := ifelse(
        (lugar == "TIERRA" & !!sym(paste0("Z_", year)) > tierra_value) | 
          (lugar == "MAR" & cantidd > 1000),
        1, 0
      ))
  }
  
  # Guardar el nuevo shapefile con las columnas adicionales
  st_write(shapefile_filtered, output_path)
}

# Procesar los shapefiles para cada cultivo

# Coca?na (ya procesado anteriormente, incluido aqu? por completitud)
process_cultivo("Coca?na", 10, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/outliers/Incautaciones_cocaina.shp")

# Hoja de Coca
process_cultivo("Hoja de Coca", 0.06, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/outliers/Incautaciones_hoja_de_coca.shp")

# Base de Coca
process_cultivo("Base de Coca", -0.01, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/outliers/Incautaciones_base_de_coca.shp")

# Pasta de Coca
process_cultivo("Pasta de Coca", -0.009, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/outliers/Incautaciones_pasta_de_coca.shp")




######################################
###########OUTLLIERS GASOLINA #######
##########################################



# Cargar librer?as
library(sf)
library(dplyr)

# Leer el shapefile
shapefile <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/gasolina/FINALES/N/GASOLINA_GPW_VIAS.shp")

# Lista de a?os
years <- 2017:2022

# Crear nuevas columnas 'OUT_XX' para cada a?o y aplicar las condiciones
for (year in years) {
  year_str <- substr(as.character(year), 3, 4) # '17', '18', etc.
  
  # Crear la nueva columna OUT_XX con las condiciones
  shapefile <- shapefile %>%
    mutate(!!paste0("OUT_", year_str) := ifelse(
      !!sym(paste0("Z_VP_", year)) > 0.02, 1, 0
    ))
}
colnames(shapefile)

# Guardar el nuevo shapefile con las columnas adicionales
output_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/outliers/GASOLINA_GPW_VIAS_OUT.shp"
st_write(shapefile, output_path)



