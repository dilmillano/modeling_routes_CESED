// Definir la geometría
var geometry2 = 
    ee.Geometry.Polygon(
        [[[-110.93978323513761, 22.321855787826443],
          [-110.93978323513761, -11.294531986318608],
          [-59.61165823513761, -11.294531986318608],
          [-59.61165823513761, 22.321855787826443]]], null, false);
Map.addLayer(geometry2)
// Definir la visualización de la capa
var raster_vis = {
  'max': 1000.0,
  'palette': [
    'ffffe7',
    '86a192',
    '509791',
    '307296',
    '2c4484',
    '000066'
  ],
  'min': 0.0
};

// Lista de años originales disponibles
var originalYears = [2000, 2005, 2010, 2015, 2020];

// Crear una colección de imágenes con las bandas correspondientes a cada año original
var collection = ee.ImageCollection(
  originalYears.map(function(year) {
    var startDate = year + '-01-01';
    var endDate = year + '-12-31'; // Captura todo el año
    
    var dataset = ee.ImageCollection('CIESIN/GPWv411/GPW_Population_Count')
                    .filterDate(startDate, endDate)
                    .filterBounds(geometry2)
                    .first()
                    .select('population_count')
                    .rename('Population_' + year) // Renombrar la banda con el año
                    .toFloat() // Asegurar que todas las bandas sean Float32
                    .set('year', year);
    
    print('Imagen cargada para el año:', year, dataset);
    return dataset;
  })
);

// Visualizar las imágenes originales
collection.toList(collection.size()).evaluate(function(images) {
  images.forEach(function(image) {
    var eeImage = ee.Image(image.id); // Obtener la imagen a partir del ID
    var year = image.properties.year; // Obtener el año directamente desde la propiedad
    Map.addLayer(eeImage.clip(geometry2), raster_vis, 'Population Count ' + year);
  });

  // Función para interpolar entre dos imágenes
  function interpolateImages(previous, next, year) {
    var previousYear = ee.Number(previous.get('year'));
    var nextYear = ee.Number(next.get('year'));
    
    var fraction = ee.Image.constant(ee.Number(year).subtract(previousYear)
                   .divide(nextYear.subtract(previousYear)));
    
    return previous.add(fraction.multiply(next.subtract(previous)))
                   .rename('Population_' + year) // Renombrar la banda con el año
                   .toFloat() // Asegurar que todas las bandas sean Float32
                   .set('year', year);
  }

  // Interpolar años intermedios
  function interpolateYears(startYear, endYear) {
    var previous = collection.filter(ee.Filter.eq('year', startYear)).first();
    var next = collection.filter(ee.Filter.eq('year', endYear)).first();
    
    var interpolatedImages = [];
    
    for (var year = startYear + 1; year < endYear; year++) {
      var interpolatedImage = interpolateImages(previous, next, year);
      interpolatedImages.push(interpolatedImage);
    }
    
    return interpolatedImages;
  }

  // Aplicar interpolación para los años intermedios y proyectados
  var allImages = collection.toList(collection.size());

  allImages = allImages.cat(interpolateYears(2000, 2005));
  allImages = allImages.cat(interpolateYears(2005, 2010));
  allImages = allImages.cat(interpolateYears(2010, 2015));
  allImages = allImages.cat(interpolateYears(2015, 2020));

  // Función para proyectar después de 2020
  function extrapolate(year) {
    var lastAvailable = collection.filter(ee.Filter.eq('year', 2020)).first();
    var prevAvailable = collection.filter(ee.Filter.eq('year', 2015)).first();
    
    var growthRate = lastAvailable.subtract(prevAvailable).divide(5); // Cambio anual promedio entre 2015 y 2020
    
    var yearsSince2020 = ee.Number(year).subtract(2020);
    
    return lastAvailable.add(growthRate.multiply(yearsSince2020))
                        .rename('Population_' + year) // Renombrar la banda con el año
                        .toFloat() // Asegurar que todas las bandas sean Float32
                        .set('year', year);
  }

  // Proyectar y agregar los años 2021-2023
  for (var year = 2021; year <= 2023; year++) {
    var projectedImage = extrapolate(year);
    allImages = allImages.add(projectedImage);
  }

  // Combinar todas las imágenes en un solo raster con 23 bandas
  var combinedImage = ee.ImageCollection(allImages).toBands();

  // Exportar la imagen combinada
  Export.image.toDrive({
    image: combinedImage.clip(geometry2),
    description: 'Population_Count_2000_2023',
    folder: 'GPW',
    fileNamePrefix: 'Population_Count_2000_2023',
    region: geometry2,
    scale: 1000, // Mantener la escala de 1 km
    maxPixels: 1e13,
    crs: 'EPSG:4326'
  });

});
