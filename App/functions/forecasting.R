# This file contains forecasting methods
#### Neural Network forward feed time series forecast function ####
createNuralNetworkTSForecast <- function(countrySelected){
  # 1. Load and format data
  data <- countrySelected
  df <- ts(data$daily)
  
  # 2. Train model
  numDaysToForecast <- 7
  fit <- nnetar(df, repeats = 20)
  fcast <- forecast(fit, h = numDaysToForecast )
  
  # 3. Create dataframe for forecasted values
  newDates <- data$formatedDate[nrow(data)] + 1
  for(i in 1:numDaysToForecast){
    newDates[i] <- data$formatedDate[nrow(data)] + i
  }
  forcast<- as.integer(fcast$mean)
  dfForecastedCases <- data.frame(newDates, forcast)
  
  return(dfForecastedCases)
}

# ARIMA MODEL - not used
#t <- createTimeSeiresForCountry("Poland")
#rownames(t) <- t[,1]
#data  <- t  %>% 
#  select(daily)

#tsData = ts(data$daily)
#data$clean = tsclean(tsData)
#data$movingAvg = ma(data$clea, order= 7)

#finalTsData <- ts(na.omit(data$movingAvg), frequency = 7)
#decomp = stl(finalTsData, s.window = "periodic")

#### DICOMPSITION OF THE DATA - Seasonality, trend, cycle ###
# Calculate seasonal 
#count_ma = ts(na.omit(forecastData$cnt_ma), frequency = 7)
#decomp = stl(count_ma, s.window = "periodic")

#desesonal_cnt <- seasadj(decomp)
#count_d1 = diff(desesonal_cnt, differences = 1)


#x <- stl(log(AirPassengers), "per")
#hc <- hchart(x)

#### Other functions ####
# Normalize function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# Normalize data
#data$daily[1] <- 1 # add missing value, gives error without it.
#data$daily_norm <- normalize(data$daily)

