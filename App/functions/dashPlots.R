### This file contains code used to generate plots of cases, deaths and recovered cases of COVID-19 DATA R script###
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

# 2. Cumulative time series function
cumulitiveTimeSeries <- function(dataSet){
  allCountriesData <- dataSet[5:numOfCol]
  total <- colSums(allCountriesData)
  timeSeiresCumulative <- data.frame(formatedDate, total)
  rownames(timeSeiresCumulative) <- NULL
  
  return(timeSeiresCumulative)
}

# Time Series Cumulative 
timeSeiresCasesCumulative <- cumulitiveTimeSeries(casesDataSet)
timeSeiresRecoveredCumulative <- cumulitiveTimeSeries(recoveredDataSet)
timeSeiresDeathsCumulative <- cumulitiveTimeSeries(deathsDataSet)

# 3. Daily time series function
dailyTimeSeries <- function(cumulativeTimeSeries){
  totalC <- cumulativeTimeSeries[,2]
  daily <- c()
  for(i in 1:length(totalC)-1){
    daily[i] <- totalC[i+1] - totalC[i]
  }
  formatedDate <- formatedDate[-1]
  dailyTimeSeries <- data.frame(formatedDate, daily )
  
  return(dailyTimeSeries)
}

# Time Series Daily
timeSeiresCasesDaily <- dailyTimeSeries(timeSeiresCasesCumulative)
timeSeiresRecoveredDaily <- dailyTimeSeries(timeSeiresRecoveredCumulative)
timeSeiresDeathsDaily <- dailyTimeSeries(timeSeiresDeathsCumulative)

#Plots
dailyCasesPlot <- function(){
  #Forecast Cases
  forecastData <- createNuralNetworkTSForecast(timeSeiresCasesDaily)
  
  plot <-timeSeiresCasesDaily %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Obsereved Cases") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, forcast), name = "Forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  return(plot)
}

dailyRecoveredPlot <- function(){
  #Forecast recovered
  forecastData <- createNuralNetworkTSForecast(timeSeiresRecoveredDaily)

  plot <-timeSeiresRecoveredDaily %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Recovered") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, forcast), name = "Forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Recovered"))
  
  return(plot)
}

dailyDeathsPlot <- function(){
  #Forecast deaths
  forecastData <- createNuralNetworkTSForecast(timeSeiresDeathsDaily)

  plot <-timeSeiresDeathsDaily %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Observed Deaths") %>% 
    hc_add_series(forecastData, "line", hcaes(newDates, forcast), name = "Forecast") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Deaths"))
  
  return(plot)
}
