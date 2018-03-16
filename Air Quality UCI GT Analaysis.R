############### Air Quality Ground True Features Analysis ###############

# Activating the necessary packages
library(openair)


dataset = read.csv2('AirQualityUCI.csv', stringsAsFactors = FALSE)
# Removing the last 114 rows and last 2 columns, as all of them are null values along with the date
tail(dataset, n = 115)
dataset = dataset[1:9357, 1:15]

# Replacing -200 value in the dataset with NA
dataset[dataset == -200] = NA

# Changing the date and time format and concatenating them into a single column
dataset = within(dataset, {DateTime = format(as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H.%M.%S"))})
dataset = dataset[, c(16, 3:15)]
dataset$DateTime = as.POSIXct(dataset$DateTime)

# Rearranging the columns according to the pollutants true and targeted sensor response
dataset = dataset[, c(1:3, 4, 6, 5, 7:14)]

# Changing the column names
dataset_columns = c('date', 'co', 'co_s', 'NMHC', 'NMHC_s', 'c6h6',
                    'nox', 'nox_s', 'no2', 'no2_s', 'o3_s','temp', 'RH%', 'AH')
colnames(dataset) = dataset_columns

# Summary Plot of the dataset
summaryPlot(dataset, period = 'months')

# Trendlevel plot
trendLevel(dataset, pollutant = 'AH', auto.text = TRUE, main = 'AH')
trendLevel(dataset, pollutant = 'no2_s', auto.text = TRUE, main = 'no2_s')

# Calender plot
calendarPlot(dataset, pollutant = 'co', year = 2004)
calendarPlot(dataset, pollutant = 'co', year = 2005)
calendarPlot(dataset, pollutant = 'NMHC', year = 2004)
calendarPlot(dataset, pollutant = 'NMHC', year = 2005)
calendarPlot(dataset, pollutant = 'c6h6', year = 2004)
calendarPlot(dataset, pollutant = 'c6h6', year = 2005)
calendarPlot(dataset, pollutant = 'nox', year = 2004)
calendarPlot(dataset, pollutant = 'nox', year = 2005)
calendarPlot(dataset, pollutant = 'no2', year = 2004)
calendarPlot(dataset, pollutant = 'no2', year = 2005)

# Timevariation plot
timeVariation(dataset, pollutant = 'co', auto.text = TRUE, main = 'CO TimeVariation plot')
timeVariation(dataset, pollutant = 'c6h6', auto.text = TRUE, main = 'C6H6 TimeVariation plot')
timeVariation(dataset, pollutant = 'nox', auto.text = TRUE, main = 'NOx TimeVariation plot')
timeVariation(dataset, pollutant = 'no2', auto.text = TRUE, main = 'NO2 TimeVariation plot')

# Trendlevel
trendLevel(mydata = dataset, pollutant = 'co', auto.text = TRUE, main = 'CO TrendLevel plot')
trendLevel(mydata = dataset, pollutant = 'c6h6', auto.text = TRUE, main = 'C6H6 TrendLevel plot')
trendLevel(mydata = dataset, pollutant = 'nox', auto.text = TRUE, main = 'NOx TrendLevel plot')
trendLevel(mydata = dataset, pollutant = 'no2', auto.text = TRUE, main = 'NO2 TrendLevel plot')





