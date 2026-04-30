// =================== CONFIGURACIÓN ===================
var geometry2 = ee.Geometry.Polygon(
  [[[-110.93978323513761, 22.321855787826443],
    [-110.93978323513761, -11.294531986318608],
    [-59.61165823513761, -11.294531986318608],
    [-59.61165823513761, 22.321855787826443]]], null, false);

var wp = ee.ImageCollection('WorldPop/GP/100m/pop');

var raster_vis = {
  min: 0, max: 50,
  palette: ['24126c','1fff4f','d4ff50']
};

// Utilidad: imagen WorldPop por año (o null si no hay)
function wpYearImage(y){
  y = ee.Number(y);
  var start = ee.Date.fromYMD(y,1,1);
  var end   = start.advance(1,'year');

  var col = wp.filterDate(start, end).filterBounds(geometry2);
  var has = col.size().gt(0); // boolean del servidor

  return ee.Image(ee.Algorithms.If(
    has,
    col.select('population').mosaic()
       .toFloat()
       .rename('population')
       .set('year', y),
    null
  ));
}

// =================== BASE REAL: 2000–2020 ===================
var baseYears = ee.List.sequence(2000, 2020);
var baseCol = ee.ImageCollection(
  baseYears.map(function(y){ return wpYearImage(y); })
).filter(ee.Filter.notNull(['year']));

// =================== EXTRAPOLACIÓN SOLO 2021–2023 ===================
// Usamos 2015→2020 para la tasa si 2015 existe; si no, 2019→2020.
var img2020 = wpYearImage(2020);

// ¿Existe 2015?
var has2015 = wp.filterDate('2015-01-01', '2016-01-01')
                .filterBounds(geometry2)
                .size().gt(0);

// Elige 2015 si existe; de lo contrario 2019
var baseGrowthYear = ee.Number(ee.Algorithms.If(has2015, 2015, 2019));
var imgBase = wpYearImage(baseGrowthYear);
var yearsBetween = ee.Number(2020).subtract(baseGrowthYear);

// growthRate = (2020 - base) / años
var growthRate = ee.Image(img2020).subtract(imgBase).divide(yearsBetween).toFloat();

// Proyecta 2021–2023
var projYears = ee.List([2021, 2022, 2023]);
var projCol = ee.ImageCollection(projYears.map(function(y){
  y = ee.Number(y);
  var yearsSince2020 = y.subtract(2020);
  return ee.Image(img2020)
           .add(growthRate.multiply(yearsSince2020))
           .toFloat()
           .rename('population')
           .set('year', y);
}));

// =================== COLECCIÓN FINAL ===================
var allIC = baseCol.merge(projCol);

// (Opcional) ver algunas capas
Map.centerObject(geometry2, 5);
Map.addLayer(geometry2, {color:'red'}, 'Área', false);
[2000, 2010, 2020].forEach(function(y){
  var img = allIC.filter(ee.Filter.eq('year', y)).first();
  Map.addLayer(img.clip(geometry2), raster_vis, 'WorldPop ' + y, false);
});

// =================== EXPORTAR 1 RASTER POR AÑO ===================
// Exporta 2000–2023 (2021–2023 son proyectados)
var yearsAll = ee.List.sequence(2000, 2023).getInfo(); // cliente

yearsAll.forEach(function(y){
  var imgY = allIC.filter(ee.Filter.eq('year', y)).first();
  if (imgY) {
    imgY = ee.Image(imgY).clip(geometry2).unmask(0).toFloat();

    Export.image.toDrive({
      image: imgY,
      description: 'WorldPop_Population_' + y,
      folder: 'WorldPop',
      fileNamePrefix: 'WorldPop_Population_' + y,
      region: geometry2,
      scale: 100,             // WorldPop = 100 m
      maxPixels: 1e13,
      crs: 'EPSG:4326'
    });
  }
});
