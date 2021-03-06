### This file creates contry based plots and forecasting
#### Sources ####
source("data/covid19Data.R")
source("data/population.R")
source("functions/forecastFunction.R")
library(ggplot2)
library(TTR)
library(dplyr)
library(highcharter)

# 1. Format date
numOfCol <- ncol(casesDataSet)
colNames <- colnames(casesDataSet)
date <- colNames[5:numOfCol]
formatedDate <- as.Date(date, format = "%m/%d/%y")

# 2. Get country list
countries <- rbind("Global",casesDataSet[2])

# Countries accruing more then once
n_occur <- data.frame(table(casesDataSet[2]))
duplicatedCountries <- n_occur[n_occur$Freq > 1,]

createTimeSeiresForCountry <- function(country, dataSelector){
  if(country == "Global"){
    if(dataSelector == "cases"){
      data <- timeSeiresGlobalCases
    }
    else if(dataSelector == "deaths"){
      data <- timeSeiresGlobalDeaths
    }
    return(data)
  }
  else{
    # Check which data to load, Cases or Deaths
    if(dataSelector == "cases"){
      data <- casesDataSet
    }
    else if(dataSelector == "deaths"){
      data <- deathsDataSet
    }
    # Get data of selected country
    if(country %in% duplicatedCountries$Var1){
      df <- data %>% filter(data[2] == country)
      df<- data.frame( colSums(df[5:numOfCol]))
      names(df)[1] <- "df"
    }
    if(!country %in% duplicatedCountries$Var1) {
      df <- data %>% filter(data[2]== country)
      df <- df[5:numOfCol]
      df<- t(df)
    }
    # Process data 
    d <- data.frame(formatedDate, df)
    rownames(d) <- NULL
    # Add Daily change to data frame
    d$daily[1] <- 0
    for(i in 2:nrow(d)){
      d$daily[i] <- d$df[i] - d$df[i - 1] 
    }
    
    return(d)
  }
}

# 3. Return functions
retrunListOfCountries <- function(){
  return(countries)
}

returnSumCasesOfCountry <- function(df, countrySelcted){
  if(countrySelcted == "Global"){
    return(timeSeiresGlobalCases$df[nrow(timeSeiresGlobalCases)])
  }
  else{
    dF <- df
    total<-dF$df[nrow(dF)]
    return(total) 
  }
}

returnSumRecoveredOfCountry <- function(country){
  if(country == "Global"){
    return(timeSeiresGlobalRecovered$df[nrow(timeSeiresGlobalRecovered)])
  }
  else{
    # Combine country into one value if appear more then once
    if(country %in% duplicatedCountries$Var1){
      df <- recoveredDataSet %>% filter(recoveredDataSet[2] == country)
      df<- data.frame( colSums(df[5:numOfCol]))
      names(df)[1] <- "df"
      return(df$df[nrow(df)])
    }
    # Return country if appears once
    else if(!country %in% duplicatedCountries$Var1){
      df <- recoveredDataSet %>% 
        filter(recoveredDataSet[2] == country)
      return(df[ncol(df)])
    }
  }
  
}

returnSumDeathsOfCountry <- function(country){
  if(country == "Global"){
    return(timeSeiresGlobalDeaths$df[nrow(timeSeiresGlobalDeaths)])
  }
  else{
    # Combine country into one value if appear more then once
    if(country %in% duplicatedCountries$Var1){
      df <- deathsDataSet %>% filter(deathsDataSet[2] == country)
      df<- data.frame( colSums(df[5:numOfCol]))
      names(df)[1] <- "df"
      return(df$df[nrow(df)])
    }
    # Return country if appears once
    else if(!country %in% duplicatedCountries$Var1){
      df <- deathsDataSet %>% 
        filter(deathsDataSet[2] == country)
      return(df[ncol(df)])
    }
  }
}

returnActiveCases <- function(country){
  if(country == "Global"){
    return(totalActiveCases())
  }
  else {
    firstDay <- ncol(casesDataSet) - 13
    lastDay <- ncol(casesDataSet)
    # Combine country into one value if appear more then once
    if(country %in% duplicatedCountries$Var1){
      df <- casesDataSet %>% filter(casesDataSet[2] == country)
      fourTeenDaysCases <- sum(df[lastDay]) - sum(df[firstDay])
      return(fourTeenDaysCases)
    }
    # Return country if appears once
    else if(!country %in% duplicatedCountries$Var1){
      df <- casesDataSet %>% 
        filter(casesDataSet[2] == country)
      fourTeenDaysCases <- df[lastDay] - df[firstDay]
      return(fourTeenDaysCases)
    }
  }
}

returnPercentageOfPopulation <- function(casesNum,countryName){
  if(countryName == "Global"){
    population <- sum(pop$population)
    decimal <- casesNum/population
  }
  else{
    popOfCountry <- pop %>% filter(country == countryName)
    decimal <- casesNum/popOfCountry$population
  }
  percentage <- decimal * 100
  percentage <- format(round(percentage, 2), nsmall = 2)
  return(percentage)
}

returnTitleOfCountryPlots <- function(switchGraphType, switchData){
  if(switchGraphType == "bar" & switchData == "cases"){
    return("Daily cases with forecast")
  }
  else if(switchGraphType == "bar" & switchData == "deaths"){
    return("Daily deaths with forecast")
  }
  else if(switchGraphType == "line" & switchData == "cases"){
    return("Cumulative cases with forecast")
  }
  else if(switchGraphType == "line" & switchData == "deaths"){
    return("Cumulative deaths with forecast")
  }
}

returnPopulationOfSelctedCountry <- function(countryName){
  if(countryName == "Global"){
    return(sum(pop$population))
  }
  else{
    popOfCountry <- pop %>% filter(country == countryName )
    return(popOfCountry$population) 
  }
}

# 4. Interactive plots:
interactivePlotsMechanism <- function(countrySelected, plotType, ema, daysToForecast, switchData, model , fitControl, testForecast){
  if(switchData == "cases"){
    if(plotType == "bar"){
      return(dailyForecastPlotCases(countrySelected,ema,daysToForecast, model,fitControl,testForecast))
    }
    else if(plotType == "line"){
      return(cummulativePlotCases(countrySelected,daysToForecast, model))
    }
  }
  else if(switchData == "deaths"){
    if(plotType == "bar"){
      return(dailyForecastPlotDeaths(countrySelected,ema,daysToForecast, model,testForecast))
    }
    else if(plotType == "line"){
      return(cummulativePlotDeaths(countrySelected,daysToForecast , model))
    }
  }
} 

accurcyTable <- function(countrySelected,daysToForecast){
  forecastData <- createForecastModel(countrySelected,daysToForecast)
  Method <- c("Neural Network (ANN)", "Autoregressive integrated moving average (ARIMA)", "Exponential smoothing state space model (ETS)", 
              "Exponential smoothing with Box-Cox transformation, ARMA errors, Trend and Seasonal components (BATS)")
  rmse <- c( forecastData$nnetar[1], forecastData$arima[1], forecastData$ets[1], forecastData$bats[1] )
  mape <- c( paste(floor(forecastData$nnetar[2]), "%"),paste(floor(forecastData$arima[2]), "%"),
             paste(floor(forecastData$ets[2]), "%"),paste(floor(forecastData$bats[2]), "%") )
  df <- data.frame(Method,rmse,mape )
  names(df)[2] <- "RMSE"
  names(df)[3] <- "MAPE"
  
  return(df)
}
countrySelected <- createTimeSeiresForCountry("Poland","cases")
testForecast <- TRUE
daysToForecast <- 14

#### CASES ####
dailyForecastPlotCases <- function(countrySelected, ema, daysToForecast, model, fitted,testForecast){
  # Forecast data
  forecastData <- createForecastModel(countrySelected,daysToForecast,model,FALSE, testForecast)
  # Exponential Moving Average
  countrySelected$EMA <- TTR::EMA(countrySelected$daily, n = 7)
  # Plot the data
  plot <-countrySelected %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Observed") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, Point.Forecast), name = "Forecast", id = "forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  # Check if confidence level exists
  if("Lo.95" %in% colnames(forecastData)){
    plot <- plot %>% hc_add_series(forecastData, type = "arearange", hcaes(x = newDates, low = Lo.95, high = Hi.95), linkedTo = "forecast")
  }
  # Check if to apply moving average 
  if(isTRUE(ema)){
    plot <- plot %>% hc_add_series(countrySelected, "line", hcaes(formatedDate, EMA), name = "Exponential Moving Average")
  }
  # check if fitted model should be plotted
  if(isTRUE(fitted)){
    fittedModelData <- createForecastModel(countrySelected,daysToForecast,model, TRUE)
    plot <- plot %>% hc_add_series(fittedModelData, "line", hcaes(x = date,y = fitted), name = "Fitted Model")
  }
  
  return(plot)
}

cummulativePlotCases <- function(countrySelected,daysToForecast,model){
  # Forecast data
  forecastData <- createForecastModel(countrySelected,daysToForecast,model)
  # Change forecast to cumulative
  lastDataPoint <- countrySelected$df[nrow(countrySelected)]
  forecastData$Point.Forecast[1] <- lastDataPoint + forecastData$Point.Forecast[1]
  for(i in 2: nrow(forecastData)){
    forecastData$Point.Forecast[i] <- forecastData$Point.Forecast[i] + forecastData$Point.Forecast[i -1]
  }
  
  plot <-countrySelected %>% hchart("line", 
    hcaes(x = formatedDate , y = df), name = "Observed") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, Point.Forecast), name = "Forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  return(plot)
}

#### DEATHS ####
dailyForecastPlotDeaths <- function(countrySelected, ema, daysToForecast, model,testForecast){
  # Forecast data
  forecastData <- createForecastModel(countrySelected,daysToForecast, model,FALSE, testForecast)
  # Exponential Moving Average
  countrySelected$EMA <- TTR::EMA(countrySelected$daily, n = 7)
  # Plot the data
  plot <-countrySelected %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Observed") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, Point.Forecast), name = "Forecast", id = "forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Deaths"))
  # Check if confidence level exists
  if("Lo.95" %in% colnames(forecastData)){
    plot <- plot %>% hc_add_series(forecastData, type = "arearange", hcaes(x = newDates, low = Lo.95, high = Hi.95), linkedTo = "forecast")
  }
  # Check if to apply 
  if(isTRUE(ema)){
    plot <- plot %>% hc_add_series(countrySelected, "line", hcaes(formatedDate, EMA), name = "Exponential Moving Average")
  }
  
  return(plot)
}

cummulativePlotDeaths <- function(countrySelected,daysToForecast , model){
  # Forecast data
  forecastData <- createForecastModel(countrySelected,daysToForecast, model)
  # Change forecast to cumulative
  lastDataPoint <- countrySelected$df[nrow(countrySelected)]
  forecastData$Point.Forecast[1] <- lastDataPoint + forecastData$Point.Forecast[1]
  for(i in 2: nrow(forecastData)){
    forecastData$Point.Forecast[i] <- forecastData$Point.Forecast[i] + forecastData$Point.Forecast[i -1]
  }
  
  plot <-countrySelected %>% hchart("line", 
    hcaes(x = formatedDate , y = df), name = "Observed") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, Point.Forecast), name = "Forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Deaths"))
  
  return(plot)
}

### Test functions
decomposeDataOfSelectedCountry <- function(countrySelected){
  
  timeSeries <- ts(countrySelected$daily, frequency = 12)
  decomposed <- stl(timeSeries, s.window = 30)
  
  #t <- data.frame(countrySelected$formatedDate, decomposed$time.series[,1], decomposed$time.series[,2],  decomposed$time.series[,3] )
  #colnames(t)[1] <- "date"
  #colnames(t)[2] <- "seasonal"
  #colnames(t)[3] <- "trend"
  #colnames(t)[4] <- "remainder"
  
  plot(decomposed)
  return(hchart(decomposed))
  

}