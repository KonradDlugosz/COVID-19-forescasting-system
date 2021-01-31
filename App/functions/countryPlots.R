#This file creates contry based plots and forecasting
#### Sources ####
source("data/covid19Data.R")
source("functions/forecasting.R")
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
countries <- casesDataSet[2]
countries <- countries %>% 
  distinct()

createTimeSeiresForCountry <- function(country){
  #Get data of selected country
  df <- casesDataSet %>% 
    filter(casesDataSet[2]== country)
  df <- df[5:numOfCol]
  df<- t(df)
  d <- data.frame(formatedDate, df)
  rownames(d) <- NULL
  # Add Daily change to data frame
  d$daily[1] <- 0
  for(i in 2:nrow(d)){
    d$daily[i] <- d$df[i] - d$df[i - 1] 
  }
  
  return(d)
}
# 3. Return functions
retrunListOfCountries <- function(){
  return(countries)
}

returnSumCasesOfCountry <- function(df){
  dF <- df
  total<-dF$df[nrow(dF)]
  return(total)
}

returnSumRecoveredOfCountry <- function(country){
  df <- recoveredDataSet %>% 
    filter(recoveredDataSet[2] == country)
  return(df[ncol(df)])
}

returnSumDeathsOfCountry <- function(country){
  df <- deathsDataSet %>% 
    filter(deathsDataSet[2] == country)
  return(df[ncol(df)])
}

returnSumActiveCasesOfCountry <- function(country){
  c <- as.numeric(returnSumCasesOfCountry(createTimeSeiresForCountry(country)))
  r <- as.numeric(returnSumRecoveredOfCountry(country))
  d <- as.numeric(returnSumDeathsOfCountry(country))
  a <- c - (r + d)
  return(a)
}

# 4. Interactive plots:
interactivePlotsMechanism <- function(countrySelected, plotType, ema){
  if(plotType == "bar"){
    return(dailyForecastPlot(countrySelected,ema))
  }
  else if(plotType == "line"){
    return(cummulativePlot(countrySelected))
  }
  
}
#### CASES ####
dailyForecastPlot <- function(countrySelected, ema){
  # Forecast data
  forecastData <- createNuralNetworkTSForecast(countrySelected)
  # Exponential Moving Average
  countrySelected$EMA <- TTR::EMA(countrySelected$daily, n = 7)
  # Plot the data
  plot <-countrySelected %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Observed") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, forcast), name = "Forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  # Check if to apply 
  if(isTRUE(ema)){
    plot <- plot %>% hc_add_series(countrySelected, "line", hcaes(formatedDate, EMA), name = "Exponential Moving Average")
  }
  
  return(plot)
}

cummulativePlot <- function(countrySelected){
  # Forecast data
  forecastData <- createNuralNetworkTSForecast(countrySelected)
  # Change forecast to cumulative
  lastDataPoint <- countrySelected$df[nrow(countrySelected)]
  forecastData$forcast[1] <- lastDataPoint + forecastData$forcast[1]
  for(i in 2: nrow(forecastData)){
    forecastData$forcast[i] <- forecastData$forcast[i] + forecastData$forcast[i -1]
  }
  
  plot <-countrySelected %>% hchart("line", 
    hcaes(x = formatedDate , y = df), name = "Observed") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, forcast), name = "Forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  return(plot)
}
