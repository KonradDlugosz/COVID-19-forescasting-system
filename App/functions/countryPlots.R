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

# Countries accruing more then once
n_occur <- data.frame(table(casesDataSet[2]))
duplicatedCountries <- n_occur[n_occur$Freq > 1,]
  
createTimeSeiresForCountry <- function(country){
  # Get data of selected country
  if(country %in% duplicatedCountries$Var1){
    df <- casesDataSet %>% filter(casesDataSet[2] == country)
    df<- data.frame( colSums(df[5:numOfCol]))
    names(df)[1] <- "df"
  }
  if(!country %in% duplicatedCountries$Var1) {
    df <- casesDataSet %>% filter(casesDataSet[2]== country)
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

# incubation period Active cases
#t <- createTimeSeiresForCountry("Poland")
#n <- nrow(t)
#n14 <- nrow(t) - 14
#sum(t$daily[n:n14])


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

returnSumDeathsOfCountry <- function(country){
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

returnSumActiveCasesOfCountry <- function(country){
  c <- as.numeric(returnSumCasesOfCountry(createTimeSeiresForCountry(country)))
  r <- as.numeric(returnSumRecoveredOfCountry(country))
  d <- as.numeric(returnSumDeathsOfCountry(country))
  a <- c - (r + d)
  return(a)
}

# 4. Interactive plots:
interactivePlotsMechanism <- function(countrySelected, plotType, ema, daysToForecast){
  if(plotType == "bar"){
    return(dailyForecastPlot(countrySelected,ema,daysToForecast))
  }
  else if(plotType == "line"){
    return(cummulativePlot(countrySelected,daysToForecast))
  }
  
} 

accurcyOfForecast <- function(countrySelected,daysToForecast){
  forecastData <- createNuralNetworkTSForecast(countrySelected,daysToForecast)
  rootMeanSquaredError <- forecastData$rootMeanSquaredError[1]
  return(rootMeanSquaredError)
}

#### CASES ####
dailyForecastPlot <- function(countrySelected, ema, daysToForecast){
  # Forecast data
  forecastData <- createNuralNetworkTSForecast(countrySelected,daysToForecast)
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

cummulativePlot <- function(countrySelected,daysToForecast){
  # Forecast data
  forecastData <- createNuralNetworkTSForecast(countrySelected,daysToForecast)
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
