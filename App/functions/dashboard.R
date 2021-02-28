### This file contains code used to generate plots of cases, deaths and recovered cases of COVID-19 DATA R script###
#### Sources ####
source("data/covid19Data.R")
source("functions/forecastFunction.R")
library(ggplot2)
library(TTR)
library(dplyr)
library(highcharter)
library(outliers)

# 1. Format date
numOfCol <- ncol(casesDataSet)
colNames <- colnames(casesDataSet)
date <- colNames[5:numOfCol]
formatedDate <- as.Date(date, format = "%m/%d/%y")


timeSeriesGlobal <- function(dataSet){
  # Cumulative
  allCountriesData <- dataSet[5:numOfCol]
  total <- colSums(allCountriesData)
  
  # Daily
  daily <- c()
  for(i in 1:length(total)-1){
    daily[i] <- total[i+1] - total[i]
  }
  daily <- c(0, daily)
  
  # Combine and return
  ts <- data.frame(formatedDate, total, daily )
  rownames(ts) <- NULL
  names(ts)[2] <- "df"
  names(ts)[3] <- "daily"
  
  return(ts)
}

timeSeiresGlobalCases <- timeSeriesGlobal(casesDataSet)
timeSeiresGlobalRecovered <- timeSeriesGlobal(recoveredDataSet)
timeSeiresGlobalDeaths <- timeSeriesGlobal(deathsDataSet)

# Weekly Change 
weeklyCasesChange <- function(){
  data <- timeSeiresGlobalCases$daily
  index <- length(data) - 13
  twoWeeksData <- data[index:length(data)]
  
  firstWeek <- sum(twoWeeksData[1:7])
  secondWeek <- sum(twoWeeksData[8:14])
  
  change <- (firstWeek - secondWeek) / firstWeek * 100
  procentage <- format(round(change, 2), nsmall = 2)
  return(toString(procentage)) 
}

# Outlier detection and correction 
# CASES:
outlier <- outlier(timeSeiresGlobalCases$daily)
casesMean <- mean(timeSeiresGlobalCases$daily)
outlierCasesIndex <- match(outlier, timeSeiresGlobalCases$daily)
timeSeiresGlobalCases$daily[outlierCasesIndex] <- casesMean

# RECOVERED: 
outlier <- outlier(timeSeiresGlobalRecovered$daily)
casesMean <- mean(timeSeiresGlobalRecovered$daily)
outlierRecoveredIndex <- match(outlier, timeSeiresGlobalRecovered$daily)
timeSeiresGlobalRecovered$daily[outlierRecoveredIndex] <- casesMean

##### INTERACTVE PLOTS SELECTION ####
selectDashPlot <- function(dataSelceted, plotSelected){
  if(dataSelceted == "cases"){
    if(plotSelected == "bar"){
      return(dailyCasesPlot())
    }
    else if(plotSelected == "line"){
      return(cumulativeCasesPlot())
    }
  }
  
  if(dataSelceted == "recovered"){
    if(plotSelected == "bar"){
      return(dailyRecoveredPlot())
    }
    else if(plotSelected == "line"){
      return(cumulativeRecoveredPlot())
    }
  }
  
  if(dataSelceted == "deaths"){
    if(plotSelected == "bar"){
      return(dailyDeathsPlot())
    }
    else if(plotSelected == "line"){
      return(cumulativeDeathsPlot())
    }
  }
  
}
#### CASES #### 
dailyCasesPlot <- function(){
  # Daily plot
  plot <-timeSeiresGlobalCases %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Obsereved Cases") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  return(plot)
}

cumulativeCasesPlot <- function(){
  # Cumulative plot 
  plot <-timeSeiresGlobalCases %>% hchart("line", 
    hcaes(x = formatedDate , y = df), name = "Obsereved Cases") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  return(plot)
}
dailyRecoveredPlot <- function(){
  # Daily plot
  plot <-timeSeiresGlobalRecovered %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Recovered") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Recovered"))
  
  return(plot)
}

cumulativeRecoveredPlot <- function(){
  # Cumulative plot 
  plot <-timeSeiresGlobalRecovered %>% hchart("line", 
    hcaes(x = formatedDate , y = df), name = "Recovered") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Recovered"))
  
  return(plot)
}

dailyDeathsPlot <- function(){
  # Daily deaths plot
  plot <-timeSeiresGlobalDeaths %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Observed Deaths") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Deaths"))
  
  return(plot)
}
cumulativeDeathsPlot <- function(){
  # Cumulative deaths 
  plot <-timeSeiresGlobalDeaths %>% hchart("line", 
    hcaes(x = formatedDate , y = df), name = "Observed Deaths") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Deaths"))
  
  return(plot)
}

activePlot <- function() {
  # Load active cases data
  df <- activeCasesTimeSeries()

  plot <- df %>% hchart("line", 
    hcaes(x = date  , y = active), name = "Active cases") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Deaths"))
  
  return(plot)
}

