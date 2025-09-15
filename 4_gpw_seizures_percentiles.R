#############################################
#### REESTABLECER CÓDIGO INICIAL Y NA = 0 ###
#############################################

# 1. Cargar las librerías necesarias
library(sf)
library(dplyr)
library(terra)
library(parallel)

# 2. Cargar el shapefile de los polígonos
shapefile_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca.shp"
poligonos <- st_read(shapefile_path)

# Ver las columnas actuales del shapefile
colnames(poligonos)

# 3. Cargar el raster con las 24 bandas (Population_Count_2000_2023.tif)
raster_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL/Population_Count_2000_2023_V2.tif"

# Reorganizar las bandas según el orden proporcionado
bandas_indices <- c(1, 6, 11, 16, 21, 2, 3, 4, 5, 7, 8, 9, 10, 12, 13, 14, 15, 17, 18, 19, 20, 22, 23, 24)
total_bandas <- length(bandas_indices)  # Total de bandas

# 4. Configurar la paralelización
num_cores <- detectCores() - 1  # Utilizar todos los núcleos menos uno
cl <- makeCluster(num_cores)

# Asegurarse de que las librerías necesarias se carguen en cada worker
clusterEvalQ(cl, {
  library(terra)
  library(sf)
})

# 5. Función para procesar cada banda (con NA tratados como ceros)
procesar_banda <- function(i) {
  # Cargar el shapefile dentro de cada nodo
  poligonos <- st_read(shapefile_path)
  
  # Cargar solo la banda necesaria dentro de cada nodo
  raster_poblacion <- rast(raster_path)
  banda_actual <- raster_poblacion[[bandas_indices[i]]]
  
  # Reemplazar valores NA por cero en la banda actual
  banda_actual[is.na(banda_actual)] <- 0
  
  # Extraer la suma de los valores de la población en cada polígono (tratando NA como 0)
  poblacion_suma <- terra::extract(banda_actual, poligonos, fun = sum, na.rm = TRUE)
  
  # Imprimir mensaje de progreso con porcentaje
  porcentaje <- round((i / total_bandas) * 100, 2)
  cat("Banda", i, "de", total_bandas, "procesada (", porcentaje, "% completado)\n")
  
  return(poblacion_suma[, 2])
}

# 6. Exportar las variables al cluster
clusterExport(cl, c("procesar_banda", "total_bandas", "shapefile_path", "raster_path", "bandas_indices"))

# 7. Ejecutar la función en paralelo
poblacion_suma_list <- parLapply(cl, 1:total_bandas, procesar_banda)

# 8. Finalizar el cluster
stopCluster(cl)

# 9. Convertir la lista de resultados en un dataframe
poblacion_suma_df <- do.call(cbind, poblacion_suma_list)

# 10. Verificar el número de columnas en el dataframe resultante
num_columnas <- ncol(poblacion_suma_df)
print(paste("Número de columnas en poblacion_suma_df:", num_columnas))  # Imprime el número de columnas para verificar

# 11. Asignar nombres de columnas solo si 'poblacion_suma_df' tiene el número esperado de columnas
nombres_esperados <- paste0("pob_", c(2000:2023))

if (num_columnas == length(nombres_esperados)) {
  colnames(poblacion_suma_df) <- nombres_esperados
} else {
  warning("El número de columnas en 'poblacion_suma_df' no coincide con el número de nombres de columnas esperados.")
  colnames(poblacion_suma_df) <- paste0("pob_", seq_len(num_columnas))
}

# 12. Verificar si las columnas a combinar ya existen para evitar sobreescribir
columnas_a_completar <- setdiff(nombres_esperados, colnames(poligonos))

if (length(columnas_a_completar) > 0) {
  # Combinar solo las columnas que no existen en poligonos
  poligonos <- cbind(poligonos, poblacion_suma_df[, columnas_a_completar])
  print("Combinación exitosa de poligonos y las nuevas columnas de pob_.")
} else {
  stop("Error: Las columnas pob_ ya existen en poligonos. Verifica que no se sobrescriban las columnas originales.")
}

# 13. Guardar el shapefile actualizado
output_shapefile_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_coca_GPWZ.shp"
st_write(poligonos, output_shapefile_path)

print("El shapefile ha sido actualizado con los valores de población sumados para cada polígono.")



colnames(poligonos)


################################
#### CALCULAR PER CAPITA#######
#############################

#  2. Definir los años a calcular
anios <- 2017:2022

# 3. Verificar y convertir los datos a tipo numérico o double si es necesario
if (!is.numeric(poligonos$cantidd)) {
  poligonos$cantidd <- as.numeric(poligonos$cantidd)
}

# Convertir las columnas pob_2017 a pob_2022 a numérico si es necesario
for (anio in anios) {
  columna_pob <- paste0("pob_", anio)
  
  if (!is.numeric(poligonos[[columna_pob]])) {
    poligonos[[columna_pob]] <- as.numeric(poligonos[[columna_pob]])
  }
}

# 4. Generar las nuevas columnas "IP_" dividiendo "cantidd" por "pob_" para cada año
for (anio in anios) {
  columna_ip <- paste0("IP_", anio)  # Cambiado a "IP_"
  columna_pob <- paste0("pob_", anio)
  
  # Inicializar la columna con NA
  poligonos[[columna_ip]] <- NA
  
  # Calcular IP_ solo para las filas correspondientes al año
  indices <- which(poligonos$year == anio)
  if (length(indices) > 0) {
    poligonos[[columna_ip]][indices] <- poligonos$cantidd[indices] / poligonos[[columna_pob]][indices]
    
    # Reemplazar Inf por NA
    poligonos[[columna_ip]][is.infinite(poligonos[[columna_ip]])] <- NA
    
    # Imprimir un mensaje de depuración
    cat("Año:", anio, "\n")
    cat("Valores de", columna_ip, ":\n")
    print(summary(poligonos[[columna_ip]]))
  } else {
    cat("Advertencia: No se encontraron datos para el año", anio, ".\n")
  }
}

# 5. Calcular las desviaciones estándar para "IP_" y guardarlas en "Z_"
for (anio in anios) {
  columna_ip <- paste0("IP_", anio)  # Cambiado a "IP_"
  columna_z <- paste0("Z_", anio)    # Cambiado a "Z_"
  
  # Inicializar la columna con NA
  poligonos[[columna_z]] <- NA
  
  if (columna_ip %in% colnames(poligonos)) {
    valores_ip <- poligonos[[columna_ip]]
    
    if (length(unique(na.omit(valores_ip))) > 1) {
      # Calcular Z_ solo si hay más de un valor distinto
      poligonos[[columna_z]] <- scale(valores_ip, center = TRUE, scale = TRUE)
    } else {
      # Asignar NA si no se puede calcular la desviación estándar
      poligonos[[columna_z]] <- NA
    }
    
    # Imprimir un mensaje de depuración
    cat("Valores de", columna_z, ":\n")
    print(summary(poligonos[[columna_z]]))
  } else {
    cat("Advertencia: No se pudo calcular Z_ para el año", anio, "porque falta la columna IP_ correspondiente.\n")
  }
}


colnames(poligonos)

# 6. Guardar el shapefile actualizado
output_shapefile_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL/Incautaciones_voronoi_GPW_Z.shp"
st_write(poligonos, output_shapefile_path)

print("El shapefile ha sido actualizado con las nuevas columnas 'IP_' y 'Z_' calculadas para cada año.")


#################################
####PERCENTILES POR GRUPO ########
##################################


# 1. Evitar notación científica
options(scipen = 999)

# 2. Definir los años de las columnas "Z_" que vamos a utilizar
anios <- 2017:2022

# 3. Crear una lista vacía para almacenar los resultados
resultados_percentiles <- list()

# 4. Iterar sobre los grupos de 'cultivo'
for (grupo_cultivo in unique(poligonos$cultivo)) {
  
  # Filtrar los datos para cada grupo de 'cultivo'
  subset_poligonos <- poligonos[poligonos$cultivo == grupo_cultivo, ]
  
  # Concatenar todos los valores de las columnas "Z_" para este grupo y eliminar los NA y ceros
  valores_z <- unlist(subset_poligonos[grepl("^Z_", colnames(subset_poligonos))], use.names = FALSE)
  valores_z <- valores_z[!is.na(valores_z) & valores_z != 0]
  
  # Si el grupo tiene valores válidos, calcular percentiles y cuartiles
  if (length(valores_z) > 0) {
    percentiles <- quantile(valores_z, probs = c(0.25, 0.5, 0.75, 0.90, 0.95, 0.99))
    
    # Guardar los resultados en la lista con el nombre del cultivo
    resultados_percentiles[[grupo_cultivo]] <- data.frame(
      Cultivo = grupo_cultivo,
      Percentil = c("25th", "50th (Median)", "75th", "90th", "95th", "99th"),
      Valor = format(percentiles, digits = 6, scientific = FALSE)  # Formatear como decimal
    )
  } else {
    cat("Advertencia: No hay suficientes datos válidos para el grupo de cultivo", grupo_cultivo, "\n")
  }
}

# 5. Combinar todos los resultados en un solo DataFrame
resultados_percentiles_df <- do.call(rbind, resultados_percentiles)

# 6. Mostrar los resultados
print("Percentiles y cuartiles calculados por grupo de cultivo:")
print(resultados_percentiles_df)

# Si quieres guardar los resultados en un archivo, puedes hacerlo con:
# write.csv(resultados_percentiles_df, "resultados_percentiles_cultivo.csv")


##################################################################################################################




# Cargar el shapefile
shapefile_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/INCAUTACIONES_ALL2/Incautaciones_voronoi_GPW_Z.shp"
poligonos <- st_read(shapefile_path)

# Verificar que las columnas 'cantidd', 'cultivo' y 'tipo' existan
if (!all(c("cantidd", "cultivo", "tipo") %in% colnames(poligonos))) {
  stop("Las columnas 'cantidd', 'cultivo', o 'tipo' no están presentes en el shapefile.")
}

# Calcular los percentiles diferenciando por 'cultivo' y 'tipo'
resultados_percentiles <- poligonos %>%
  group_by(cultivo, tipo) %>%
  summarise(
    percentil_25 = quantile(cantidd, probs = 0.25, na.rm = TRUE),
    percentil_50 = quantile(cantidd, probs = 0.50, na.rm = TRUE), # Mediana
    percentil_75 = quantile(cantidd, probs = 0.75, na.rm = TRUE),
    percentil_90 = quantile(cantidd, probs = 0.90, na.rm = TRUE),
    percentil_95 = quantile(cantidd, probs = 0.95, na.rm = TRUE),
    percentil_99 = quantile(cantidd, probs = 0.99, na.rm = TRUE)
  )

# Imprimir los resultados
print(resultados_percentiles)

# Si deseas guardar los resultados en un archivo CSV
write.csv(resultados_percentiles, "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/resultados_percentiles_cultivo_tipo.csv")




