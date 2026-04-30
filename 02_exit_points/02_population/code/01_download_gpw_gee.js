// =================== CONFIGURACIÓN ===================
var geometry2 = ee.Geometry.Polygon(
  [[[-110.93978323513761, 22.321855787826443],
    [-110.93978323513761, -11.294531986318608],
    [-59.61165823513761, -11.294531986318608],
    [-59.61165823513761, 22.321855787826443]]], null, false);

var raster_vis = {
  min: 0, max: 1000,
  palette: ['ffffe7','86a192','509791','307296','2c4484','000066']
};

var originalYears = ee.List([2000, 2005, 2010, 2015, 2020]);

// =================== CARGA AÑOS ANCLA (banda fija 'population') ===================
var baseCol = ee.ImageCollection(originalYears.map(function(year){
  year = ee.Number(year);
  var start = ee.Date.fromYMD(year,1,1);
  var end   = start.advance(1,'year');
  var img = ee.ImageCollection('CIESIN/GPWv411/GPW_Population_Count')
              .filterDate(start, end)
              .filterBounds(geometry2)
              .first()
              .select('population_count')
              .toFloat()
              .rename('population')     // <--- nombre fijo
              .set('year', year);
  return img;
}));

// =================== INTERPOLACIÓN (lineal) ===================
function interpolateImages(prev, next, year){
  var y0 = ee.Number(prev.get('year'));
  var y1 = ee.Number(next.get('year'));
  var frac = ee.Image.constant(
    ee.Number(year).subtract(y0).divide(y1.subtract(y0))
  );
  return ee.Image(prev)
    .add(frac.multiply(ee.Image(next).subtract(prev)))
    .toFloat()
    .rename('population')               // <--- nombre fijo
    .set('year', year);
}

function interpolateRange(startY, endY){
  startY = ee.Number(startY); endY = ee.Number(endY);
  var prev = baseCol.filter(ee.Filter.eq('year', startY)).first();
  var next = baseCol.filter(ee.Filter.eq('year', endY)).first();
  var years = ee.List.sequence(startY.add(1), endY.subtract(1));
  return ee.ImageCollection(years.map(function(y){
    return interpolateImages(prev, next, y);
  }));
}

var interp_00_05 = interpolateRange(2000, 2005);
var interp_05_10 = interpolateRange(2005, 2010);
var interp_10_15 = interpolateRange(2010, 2015);
var interp_15_20 = interpolateRange(2015, 2020);

// =================== EXTRAPOLACIÓN (2021–2023) ===================
var img2015 = baseCol.filter(ee.Filter.eq('year', 2015)).first();
var img2020 = baseCol.filter(ee.Filter.eq('year', 2020)).first();
var growthRate = ee.Image(img2020).subtract(img2015).divide(5).toFloat();

var extraYears = ee.List([2021, 2022, 2023]);
var extrapCol = ee.ImageCollection(extraYears.map(function(y){
  var yearsSince2020 = ee.Number(y).subtract(2020);
  var proj = ee.Image(img2020).add(growthRate.multiply(yearsSince2020))
              .toFloat()
              .rename('population')     // <--- nombre fijo
              .set('year', y);
  return proj;
}));

// =================== COLECCIÓN COMPLETA (2000–2023) ===================
var allIC = baseCol
  .merge(interp_00_05)
  .merge(interp_05_10)
  .merge(interp_10_15)
  .merge(interp_15_20)
  .merge(extrapCol);

// (Opcional) ver algunos años
Map.centerObject(geometry2, 5);
Map.addLayer(geometry2, {color:'red'}, 'Área', false);
[2000, 2005, 2010, 2015, 2020].forEach(function(y){
  var imgShow = allIC.filter(ee.Filter.eq('year', y)).first().clip(geometry2);
  Map.addLayer(imgShow, raster_vis, 'GPW ' + y, false);
});

// =================== EXPORTAR 1 RASTER POR AÑO ===================
var yearsAll = ee.List.sequence(2000, 2023).getInfo(); // cliente

yearsAll.forEach(function(y){
  var imgY = allIC.filter(ee.Filter.eq('year', y)).first()
                  .clip(geometry2)
                  .unmask(0)
                  .toFloat();

  Export.image.toDrive({
    image: imgY,
    description: 'GPW_Population_' + y,
    folder: 'GPW',
    fileNamePrefix: 'GPW_Population_' + y,
    region: geometry2,
    scale: 1000,        // GPW ~1 km
    maxPixels: 1e13,
    crs: 'EPSG:4326'
  });
});