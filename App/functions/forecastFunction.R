# This file contains forecasting methods
#### Neural Network forward feed time series forecast function ####
createForecastModel <- function(countrySelected,daysToForecast, modelMethod,fittedReturn, testForecast){
  # Defaults
  if(missing(daysToForecast)){
    daysToForecast = 14
  }
  if(missing(modelMethod)){
    modelMethod = "NNETAR"
  }
  if(missing(fittedReturn)){
    fittedReturn = FALSE
  }
  if(missing(testForecast)){
    testForecast = FALSE
  }
  # 1. Load and format data
  data <- countrySelected
  
  # Check if the data should contain the test data (daysToForecast)
  if(testForecast == TRUE){
    data <- head(data, -daysToForecast)
  }
  df <- ts(data[,3])
  
  # 2. Train models
  #### NNETAR MODEL
  fitnnetar <- nnetar(ts(df,frequency=7), repeats = 20)
  fcastnnetar <- forecast(fitnnetar, h = daysToForecast)
  ### ARIMA MODEL
  fitarima <- auto.arima(ts(df,frequency=7))
  fcastarima <- forecast(fitarima, h = daysToForecast, level = 95)
  ### ETS MODEL
  fitets <- ets(ts(df,frequency=7))
  fcastets <- forecast(fitets, h = daysToForecast, level = 95)
  ### BATS MODEL
  fitbats <- bats(ts(df,frequency=7))
  fcastbats <- forecast(fitbats, h = daysToForecast, level = 95)
  
  # 3. Create df for forecast values
  newDates <- data$formatedDate[nrow(data)] + 1
  for(i in 1:daysToForecast){
    newDates[i] <- data$formatedDate[nrow(data)] + i
  }
  
  # 4. Select models forecast to return
  if(modelMethod == "NNETAR"){
    forcast<- as.integer(fcastnnetar$mean)
    dfForecastedCases <- data.frame(newDates, fcastnnetar)
  }
  else if(modelMethod == "ARIMA"){
    forcast<- as.integer(fcastarima$mean)
    dfForecastedCases <- data.frame(newDates, fcastarima)
  }
  else if(modelMethod == "ETS"){
    forcast<- as.integer(fcastets$mean)
    dfForecastedCases <- data.frame(newDates, fcastets)
  }
  else if(modelMethod == "BATS"){
    forcast<- as.integer(fcastbats$mean)
    dfForecastedCases <- data.frame(newDates, fcastbats)
  }
  
  # Check for negative values in forecast
  for(i in 1:length(dfForecastedCases$Point.Forecast)){
    if(dfForecastedCases$Point.Forecast[i] < 0){
      dfForecastedCases$Point.Forecast[i] <- 0
    }
  }
  
  # 5. Check the accuracy of the models
  nnetar <- modelAccuracyCheck(fitnnetar)
  arima <- modelAccuracyCheck(fitarima)
  ets <- modelAccuracyCheck(fitets)
  bats <- modelAccuracyCheck(fcastbats)
  allAccuracyCheck <- data.frame(nnetar,arima,ets,bats)
  
  # 6. add accuracy results to forecast 
  diff <- nrow(dfForecastedCases) - nrow(allAccuracyCheck)
  for(i in 1:diff - 1){
    allAccuracyCheck <- rbind(allAccuracyCheck, c(0,0,0,0))
  }
  returnData <- cbind(dfForecastedCases,allAccuracyCheck)
  
  #Check if fitted model should be returned
  if(fittedReturn == TRUE){
    if(modelMethod == "NNETAR"){
      fittedModel <- data.frame(data$formatedDate, fcastnnetar$fitted)
    }
    if(modelMethod == "ARIMA"){
      fittedModel <- data.frame(data$formatedDate, fcastarima$fitted)
    }
    if(modelMethod == "ETS"){
      fittedModel <- data.frame(data$formatedDate, fcastets$fitted)
    }
    if(modelMethod == "BATS"){
      fittedModel <- data.frame(data$formatedDate, fcastbats$fitted)
    }
    names(fittedModel)[1] <- "date"
    names(fittedModel)[2] <- "fitted"
    return(fittedModel)
  }
  else{
    return(returnData)
  }
}

modelAccuracyCheck <- function(fit){
  
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
  
  return(c(rootMeanSquaredError,meanAbsolutePercentError))
}

### test functions 
#countrySelected <- createTimeSeiresForCountry("Poland", "cases")
#createForecastModel(countrySelected, 14,"NNETAR",FALSE, TRUE )

#### Other functions ####
# Normalize function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

