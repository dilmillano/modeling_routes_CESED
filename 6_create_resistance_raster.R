# Cargar las bibliotecas necesarias
library(sf)
library(raster)
library(dplyr)

# Paso 1: Cargar los shapefiles
area <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total/area_total.shp")
puntos_origen <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total/Centroides_Cluster_OR.shp")
puntos_destino <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total/Centroides_Cocaina_DES.shp")
rios <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/total/Rios_totales_buffer.shp")
vias <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/vias.shp")
batallones <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/test/batallones_buffer.shp")
gao <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/test/GAO_buffer.shp")
gasolina <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/test/gasolina_buffer.shp")
incautaciones <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/test/IncautacionesBuffer_test.shp")


# Paso 2: Verificar y reproyectar todos los shapefiles al mismo sistema de coordenadas (EPSG:4326)
crs_esperado <- 4326
area <- st_transform(area, crs_esperado)
puntos_origen <- st_transform(puntos_origen, crs_esperado)
puntos_destino <- st_transform(puntos_destino, crs_esperado)
rios <- st_transform(rios, crs_esperado)
vias <- st_transform(vias, crs_esperado)
batallones <- st_transform(batallones, crs_esperado)
gao <- st_transform(gao, crs_esperado)
gasolina <- st_transform(gasolina, crs_esperado)
incautaciones <- st_transform(incautaciones, crs_esperado)

# Paso 3: Crear el raster guía con la extensión del shapefile del área
# Definir la resolución del raster (por ejemplo, 250x250 metros aprox.)
res_metros <- 250 / 111000  # Aproximadamente 0.00225 grados

raster_guide <- raster(extent(area), res = c(res_metros, res_metros), crs = crs(area))

# Recortar el raster guía al área de estudio
raster_guide <- crop(raster_guide, area)
raster_guide <- mask(raster_guide, area)

# Paso 4: Rasterizar las capas y asignar valores

# A. Rasterizar los ríos con el valor de la columna "ORD_FLOW"
rios_sp <- as(rios, "Spatial")
r_raster_rios <- rasterize(
  rios_sp,             # Shapefile de ríos con buffer
  raster_guide,        # Raster guía
  field = "ORD_FLOW",  # Campo del shapefile que contiene el valor a asignar
  fun = 'last',        # Tomar el último valor
  background = 0,      # Asignar 0 a los píxeles sin intersección
  update = TRUE        # Actualizar el raster con los valores asignados
)

# B. Rasterizar las vías con reclasificación del campo "TIPO_VIA"
# Reclasificación de los valores del campo "TIPO_VIA"
vias_sp <- as(vias, "Spatial")
vias_reclass_matrix <- cbind(c(1, 2, 3, 4, 5, 6, 7, 10), c(11, 12, 13, 14, 15, 16, 17, 20))  # Reclasificación de las vías

r_raster_vias <- rasterize(
  vias_sp,
  raster_guide,
  field = "TIPO_VIA",
  fun = 'last',
  background = 0,
  update = TRUE
)

# Reclasificar el raster de las vías según la matriz definida
r_raster_vias_reclass <- reclassify(r_raster_vias, vias_reclass_matrix)

# C. Rasterizar las capas poligonales (incautaciones, gasolina, GAO, batallones) con valores fijos

# Incautaciones = 21
r_raster_incautaciones <- rasterize(
  incautaciones_sp,       # El shapefile de los polígonos
  raster_guide,           # El raster guía
  fun = 'first',          # Asignamos el valor 21 en los píxeles que toquen el polígono
  background = 0,         # Asignar 0 a los píxeles sin intersección
  update = TRUE
)

# Asignamos el valor 21 a las celdas que contienen polígonos de incautaciones
r_raster_incautaciones[r_raster_incautaciones > 0] <- 21

# Gasolina = 22
gasolina_sp <- as(gasolina, "Spatial")

# Rasterizar los polígonos de gasolina sin campo específico y luego asignar el valor 22
r_raster_gasolina <- rasterize(
  gasolina_sp,        # El shapefile de gasolina
  raster_guide,       # El raster guía
  fun = 'first',      # Asignamos los píxeles tocados por el polígono
  background = 0,     # Asignar 0 donde no hay intersección
  update = TRUE
)

# Asignar el valor 22 a los píxeles que intersecan los polígonos de gasolina
r_raster_gasolina[r_raster_gasolina > 0] <- 22


# GAO = 23
gao_sp <- as(gao, "Spatial")

# Rasterizar los polígonos de GAO sin campo específico y luego asignar el valor 23
r_raster_gao <- rasterize(
  gao_sp,            # El shapefile de GAO
  raster_guide,      # El raster guía
  fun = 'first',     # Asignamos los píxeles tocados por el polígono
  background = 0,    # Asignar 0 donde no hay intersección
  update = TRUE
)

# Asignar el valor 23 a los píxeles que intersecan los polígonos de GAO
r_raster_gao[r_raster_gao > 0] <- 23


# Batallones = 24
batallones_sp <- as(batallones, "Spatial")

# Rasterizar los polígonos de Batallones sin campo específico y luego asignar el valor 24
r_raster_batallones <- rasterize(
  batallones_sp,     # El shapefile de Batallones
  raster_guide,      # El raster guía
  fun = 'first',     # Asignamos los píxeles tocados por el polígono
  background = 0,    # Asignar 0 donde no hay intersección
  update = TRUE
)

# Asignar el valor 24 a los píxeles que intersecan los polígonos de Batallones
r_raster_batallones[r_raster_batallones > 0] <- 24

plot(r_raster_batallones)

# Paso para guardar el ráster en el archivo especificado
output_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RASTER/r_raster_gao.tif"

# Exportar el ráster
writeRaster(r_raster_gao, output_path, format = "GTiff", overwrite = TRUE)


#####################################################################
# Paso 5: Combinar los rasters en un solo raster total

# Asegúrate de que ya has generado los rasters individuales para incautaciones, gasolina, GAO, batallones, ríos y vías:
# r_raster_incautaciones, r_raster_gasolina, r_raster_gao, r_raster_batallones, r_raster_rios, r_raster_vias_reclass

# Paso 1: Combinar las capas de incautaciones, gasolina, GAO, y batallones
# Estas capas tendrán prioridad y no serán sobrescritas por ríos o vías
raster_prioritario <- merge(r_raster_incautaciones, r_raster_gasolina, r_raster_gao, r_raster_batallones)

# Paso 2: Asignar los valores de ríos SOLO en los píxeles que aún no tienen valores asignados
# Usamos cover() para que los valores de ríos se asignen solo donde no hay valores en raster_prioritario
raster_con_rios <- cover(raster_prioritario, r_raster_rios)

# Paso 3: Asignar los valores de vías SOLO en los píxeles que aún no tienen valores asignados
# Igual que con los ríos, usamos cover() para agregar los valores de vías donde aún no hay valores
raster_final <- cover(raster_con_rios, r_raster_vias_reclass)

# Paso 4: Asegurarse de que todos los valores sin asignación tengan un valor de 0
raster_final[is.na(raster_final)] <- 0

# Visualización opcional para verificar el resultado final
plot(raster_final, main = "Raster Final con Asignación Priorizada")

# Paso 5: Guardar el raster final
output_path <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RASTER/raster_final_priorizado22.tif"
writeRaster(raster_final, output_path, format = "GTiff", overwrite = TRUE)

# Confirmación opcional
cat("Raster final guardado en:", output_path)





####################################################################
#################### HACER LA CONDUCTANCIA Y CONECTIVIDAD ########
#################################################################

# Cargar las bibliotecas necesarias
library(raster)
library(sf)
library(gdistance)

# Paso 1: Cargar el raster final (que vamos a reclasificar para la capa de resistencia)
raster_final <- raster("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RASTER/raster_final_priorizado22.tif")

# Paso 2: Reclasificar el raster según las asignaciones de resistencia
reclass_matrix <- c(0, 950,
                    1, 10,
                    2, 20,
                    3, 30,
                    4, 40,
                    5, 50,
                    10, 10,
                    11, 500,
                    12, 350,
                    13, 300,
                    14, 250,
                    15, 200,
                    16, 150,
                    17, 100,
                    20, 500,
                    21, 20,
                    22, 20,
                    23, 20,
                    24, 800)

# Reclasificar el raster
raster_resistencia <- reclassify(raster_final, matrix(reclass_matrix, ncol=2, byrow=TRUE))

# Liberar memoria después de la reclasificación
gc()

############################################################################

# Paso 3: Cargar los puntos de origen y destino
puntos_origen <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/test/centroidescluster_O.shp")
puntos_destino <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/VECTOR/test/centroidescocaina_D.shp")

# Verificar el sistema de coordenadas (CRS) de ambos shapefiles
crs_origen <- st_crs(puntos_origen)
crs_destino <- st_crs(puntos_destino)

# Paso 4: Reproyectar si es necesario
if (crs_origen != crs_destino) {
  puntos_destino <- st_transform(puntos_destino, crs_origen)  # Reproyectar destino al CRS de origen
}

# Convertir a objetos Spatial para compatibilidad con spDists()
puntos_origen_sp <- as(puntos_origen, "Spatial")
puntos_destino_sp <- as(puntos_destino, "Spatial")

# Paso 5: Calcular la matriz de distancias euclidianas entre los puntos de origen y destino
distancias <- spDists(puntos_origen_sp, puntos_destino_sp)

# Asignar a cada punto de origen su destino más cercano
asignacion_destinos <- apply(distancias, 1, which.min)

# Extraer los puntos de destino asignados
puntos_destino_asignados <- puntos_destino[asignacion_destinos, ]

# Liberar memoria después de la asignación
gc()

############################################################################
# Paso 6: Crear la capa de conductancia a partir del raster de resistencia
conductancia <- transition(1 / raster_resistencia, transitionFunction=mean, directions=8)
conductancia_corr <- geoCorrection(conductancia, type="c", multpl=FALSE)

# Liberar memoria
gc()

############################################################################
# Paso 7: Calcular las rutas de menor costo para cada par origen-destino iterativamente

# Extraer las coordenadas de los puntos de origen y destino
coords_origen <- st_coordinates(puntos_origen)
coords_destino <- st_coordinates(puntos_destino_asignados)

# Crear un shapefile para almacenar las rutas una por una (de forma iterativa)
output_rutas <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RASTER/rutas_optimas.shp"

# Crear un ciclo para calcular y guardar las rutas de forma individual
for (i in 1:nrow(coords_origen)) {
  # Calcular la ruta de menor costo entre origen y destino
  ruta <- shortestPath(conductancia_corr, coords_origen[i,], coords_destino[i,], output="SpatialLines")
  
  # Convertir a objeto sf
  ruta_sf <- st_as_sf(ruta)
  
  # Guardar la ruta en el shapefile (usamos append para agregar cada ruta)
  if (i == 1) {
    st_write(ruta_sf, output_rutas, delete_layer=TRUE)  # Crear el shapefile la primera vez
  } else {
    st_write(ruta_sf, output_rutas, append=TRUE)  # Agregar las rutas siguientes
  }
  
  # Liberar memoria después de cada iteración
  gc()
  
  # Mensaje opcional para verificar progreso
  cat("Ruta", i, "de", nrow(coords_origen), "calculada y guardada.\n")
}

# Confirmación opcional
cat("Todas las rutas óptimas guardadas en:", output_rutas)


##############################################################
######################PARA RASTER CONECIVIDAD ###################
##############################################################


#1
# Cargar el shapefile de rutas óptimas ya calculadas
rutas_optimas <- st_read("C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RASTER/rutas_optimas.shp")

# Crear un buffer alrededor de las rutas con un radio de, por ejemplo, 0.01 grados (~1 km)
buffer_rutas <- st_buffer(rutas_optimas, dist = 0.01)

# Visualizar el buffer
plot(buffer_rutas, col = "lightblue", main = "Buffer alrededor de las Rutas Óptimas")


#2
# Rasterizar el buffer en el mismo espacio que el raster de resistencia
raster_buffer <- rasterize(buffer_rutas, raster_resistencia, field = 1, background = 0)

# Sumar el buffer al raster de flujo acumulado para representar áreas de posible flujo
raster_flujo_acumulado <- raster_flujo_acumulado + raster_buffer

# Visualizar el resultado
plot(raster_flujo_acumulado, main = "Flujo Acumulado con Buffers alrededor de las Rutas")

#3
# Crear un mapa de calor de la resistencia en torno a las rutas
plot(raster_resistencia, main = "Mapa de Resistencia - Áreas Alternativas")


#4
# Guardar el nuevo raster con las rutas y buffers
output_flujo_acumulado_buffer <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RASTER/raster_flujo_acumulado_buffer.tif"
writeRaster(raster_flujo_acumulado, output_flujo_acumulado_buffer, format = "GTiff", overwrite = TRUE)

cat("Raster con flujo acumulado y buffer guardado en:", output_flujo_acumulado_buffer)



#5

# Ruta donde quieres guardar el raster de resistencia
output_resistencia <- "C:/Users/d.millanorduz/OneDrive - Universidad de los andes/Diana_CESED/procesos/RUTAS/RASTER/raster_resistencia.tif"

# Guardar el raster de resistencia en formato GeoTIFF
writeRaster(raster_resistencia, output_resistencia, format = "GTiff", overwrite = TRUE)

# Confirmación opcional
cat("Raster de resistencia guardado en:", output_resistencia)
