// Import the 'loadAllSF' module from the 'OEEL' library
var oeel = require('users/OEEL/lib:loadAllSF');

// Load the MODIS NDVI Image Collection and select the 'NDVI' band
var col = ee.ImageCollection('MODIS/061/MOD13A2').select('NDVI');

// Add a 'doy' (Day of Year) property to each image in the collection
col = col.map(function(im) {
  return im.set("doy", im.date().getRelative('day', 'year'));
});

// Apply a moving window reducer to compute median NDVI values over a 16-day period
var colYear = oeel.ImageCollection.movingWindow({
  collection: col,
  filter: ee.Filter.equals("doy", null, "doy"),
  reducer: ee.Reducer.median(),
  copyProperties: true,
  estimationCollection: oeel.FeatureCollection.fromList({
    list: ee.List.sequence(0, 365, 16),
    propertyName: "doy"
  })
});

// Add a band to each image containing latitude values multiplied by 100 and rounded
colYear = colYear.map(function(im) {
  return im.addBands(ee.Image.pixelLonLat().select("latitude").multiply(1).round());
});

// Define a function to compute NDVI per latitude and return a FeatureCollection
function NDVIPerLat(im) {
  var x = im.reduceRegion({
    reducer: ee.Reducer.median().group(1),
    geometry: region, // Assuming 'region' is defined elsewhere
    scale: 1000,
    maxPixels: 10e9
  });

  return ee.FeatureCollection(ee.List(x.get("groups")).map(function(f) {
    f = ee.Dictionary(f);
    return ee.Feature(null, {
      doy: im.get("doy"),
      lat: f.getNumber("group"),
      ndvi: f.getNumber("median")
    });
  }));
}

// Apply the NDVIPerLat function to each image in colYear and flatten the result
var result = colYear.map(NDVIPerLat).flatten();

// Export the result table to Google Drive
Export.table.toDrive({
  collection: result,
  description: "result",
  // folder:,
  // fileNamePrefix:,
  // fileFormat:,
  selectors: ["doy","lat","ndvi"],
  // maxVertices:,
});
