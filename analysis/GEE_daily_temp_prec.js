
// Define the location (point)
var location_breeding = ee.Geometry.Point([28.76857325, -22.72463163])
var location_wintering =  ee.Geometry.Point([28.051387619881584, 8.435382674874639]);
var location = location_breeding//location_breeding; // location_wintering

// Define the time range
var startDate = '1979-01-02';
var endDate = '2020-12-31';

// Load the ERA5 daily data
var era5Daily = ee.ImageCollection('ECMWF/ERA5/DAILY')
  .filterBounds(location)
  .filterDate(startDate, endDate);

// Function to calculate total precipitation and mean 2m air temperature
var calculateMetrics = function(image) {
  var im = image.select(['total_precipitation','mean_2m_air_temperature']);
  
  var data = im.reduceRegion({
    reducer:ee.Reducer.first(),
    geometry:location,
    scale: 1000
  });

  data=ee.Dictionary(data)
  
  return ee.Feature(location, {
    'date': ee.Date(image.date().millis()).format('YYYY-MM-dd'),
    'total_precipitation': data.get('total_precipitation'),
    'mean_2m_air_temperature': data.get('mean_2m_air_temperature')
  });
};

//print(calculateMetrics(era5Daily.first()))


// Map the function over the ImageCollection
var metricsFeatures = era5Daily.map(calculateMetrics);

// Convert the resulting FeatureCollection to a table
var metricsTable = ee.FeatureCollection(metricsFeatures);

// Export the table to Google Drive as a CSV file
Export.table.toDrive({
  collection: metricsTable,
  description: 'era5_breeding',
  fileFormat: 'CSV'
});

print("Exporting... Please check the 'Tasks' tab for the export status.");
