# This file contains forecasting methods
#### Neural Network forward feed time series forecast function ####
createNuralNetworkTSForecast <- function(countrySelected,daysToForecast){
  # Default days to forecast 
  if(missing(daysToForecast)){
    daysToForecast = 14
  }
  # 1. Load and format data
  data <- countrySelected
  df <- ts(data$daily)
  
  # 2. Train model
  fit <- nnetar(df, repeats = 20)
  fcast <- forecast(fit, h = daysToForecast, level = 95)

  # 3. Create df for forecast values
  newDates <- data$formatedDate[nrow(data)] + 1
  for(i in 1:daysToForecast){
    newDates[i] <- data$formatedDate[nrow(data)] + i
  }
  forcast<- as.integer(fcast$mean)
  dfForecastedCases <- data.frame(newDates, forcast)
  
  # 4. Check the accuracy of the model
  squereError <- na.omit(fit$residuals) ^ 2
  meanSquereError <- mean(squereError)
  rootMeanSquaredError <- sqrt(meanSquereError)
  absoluteForecastError <- abs(floor(fit$residuals))
  
  # Remove NA and zero
  cleanabsoluteForecastError <- c()
  for(i in 1 : length(absoluteForecastError)){
    if((is.na(absoluteForecastError[i]) | fit$x[i]== 0)){
      cleanabsoluteForecastError[i] <- 0
    }
    else {
      cleanabsoluteForecastError[i] <- absoluteForecastError[i]/  fit$x[i] * 100
    }
  }
  meanAbsolutePercentError <- mean(cleanabsoluteForecastError)
  
  # 5. add accuracy results to forecast error
  dfForecastedCases$RMSE <- floor(rootMeanSquaredError)
  dfForecastedCases$MAFE <- floor(meanAbsolutePercentError)
  
  return(dfForecastedCases)
}

###ARIMA MODEL
#fit <- auto.arima(df)
#fcast <- forecast(fit, h = daysToForecast, level = 95)
#hchart(fcast)

### ARIMA MODEL
#plot(forecast(auto.arima(ts(df,frequency=7)),h=30))

### CROSTON MODEL
#plot(forecast(bats(ts(df,frequency=7), D= 1),h=30))

### NEURAL NETWORK
#plot(forecast(nnetar(ts(df,frequency=7)),h=30))

### ETS MODEL
#plot(forecast(ets(ts(df,frequency=7)),h=30))

#### Other functions ####
# Normalize function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Identify and replace outliers and missing values in a time series
#tsclean(df)

### test functions 
#countrySelected <- createTimeSeiresForCountry("United Kingdom", "cases")
