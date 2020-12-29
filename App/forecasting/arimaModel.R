### Sources ####
source("data/httpRequest.R")
library(forecast)
library(TTR)
library(tseries)

dailyCasesARIMA <- function(movingAvgNum, h){
  # Data prep
  dailyCases <- getDailyCasesData("dailyCases")
  dailyCases <- dailyCases %>% 
    mutate(date = ymd(dailyCases$date), count = as.numeric(newCases)) %>%
    arrange(date)
  return(ARIMAModel(dailyCases, movingAvgNum, h, "daily cases"))
}

dailyDeathsARIMA <- function(movingAvgNum, h){
  
  # Data preparation
  dailyDeaths <- getDailyCasesData("dailyDeaths")
  dailyDeaths <- dailyDeaths %>% 
    mutate(date = ymd(dailyDeaths$date), count = as.numeric(newDeaths28DaysByDeathDate))%>%
    arrange(date)
  
  return(ARIMAModel(dailyDeaths, movingAvgNum, h, "daily deaths"))
}

ARIMAModel <- function(forecastData, movingAvgNum, h, stringDataType){
  # Time series object on new cases 
  count_TSObject = ts(forecastData$count)
  # Clean, replace outlines and missing values if exist 
  forecastData$clean_count = tsclean(count_TSObject)
  # Moving average 
  forecastData$cnt_ma <- ma(forecastData$clean_count, order= movingAvgNum)
  
  #### DICOMPSITION OF THE DATA - Seasonality, trend, cycle ###
  # Calculate seasonal 
  count_ma = ts(na.omit(forecastData$cnt_ma), frequency = 7)
  decomp = stl(count_ma, s.window = "periodic")
  desesonal_cnt <- seasadj(decomp)
  
  # Brings data closer together 
  count_d1 = diff(desesonal_cnt, differences = 1)
  #plot(count_d1) 
  
  fit2 <- arima(desesonal_cnt, order = c(1,1,6))
  # Plot
  #ggtsdisplay(residuals(fit2))
  
  # Forecast fit model
  fcast <- forecast(fit2, h=h)
  fcastPlot <- autoplot(fcast) + ggtitle("Forecasting prediction for n days")
  
  return(fcastPlot)

}
