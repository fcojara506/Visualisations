// Load ERA5 dataset
var era5 = ee.ImageCollection('ECMWF/ERA5/MONTHLY');

// Define time range
var startYear = 1980;
var endYear = 2020;

// Define temperature and precipitation bands
var temperatureBand = 'mean_2m_air_temperature'; // in Kelvin
var precipitationBand = 'total_precipitation'; // in meters

// Function to process data yearly
function processYear(year) {
  var startDate = ee.Date.fromYMD(year, 1, 1);
  var endDate = ee.Date.fromYMD(year, 12, 31);

  var era5_year = era5.filterDate(startDate, endDate);

  var temp_mean = era5_year.select(temperatureBand).mean();
  var precip_sum = era5_year.select(precipitationBand).mean();

  var output = temp_mean.addBands(precip_sum);
  return output.set('system:time_start', startDate.millis());
}

// Create an image collection with processed yearly data
var yearlyData = ee.ImageCollection(
  ee.List.sequence(startYear, endYear).map(processYear)
);

// Calculate mean temperature and total precipitation over the entire period
var meanTemperature = yearlyData.select(temperatureBand).mean();
var totalPrecipitation = yearlyData.select(precipitationBand).mean();
print(totalPrecipitation)
// Display the results
Map.addLayer(meanTemperature, {min: 230, max: 310, palette: 'blue,green,red'}, 'Mean Temperature (K)');
Map.addLayer(totalPrecipitation, {min: 0, max: 0.5, palette: 'white,blue,red'}, 'Total Precipitation (m)');
Map.setCenter(0, 0, 2); // Set map center and zoom level
