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

# Weekly Change 
weeklyCasesChange <- function(){
  data <- timeSeiresCasesDaily$daily
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
outlier <- outlier(timeSeiresCasesDaily$daily)
casesMean <- mean(timeSeiresCasesDaily$daily)
outlierCasesIndex <- match(outlier, timeSeiresCasesDaily$daily)
timeSeiresCasesDaily$daily[outlierCasesIndex] <- casesMean

# RECOVERED: 
outlier <- outlier(timeSeiresRecoveredDaily$daily)
casesMean <- mean(timeSeiresRecoveredDaily$daily)
outlierRecoveredIndex <- match(outlier, timeSeiresRecoveredDaily$daily)
timeSeiresRecoveredDaily$daily[outlierRecoveredIndex] <- casesMean

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
  plot <-timeSeiresCasesDaily %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Obsereved Cases") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  return(plot)
}

cumulativeCasesPlot <- function(){
  # Cumulative plot 
  plot <-timeSeiresCasesCumulative %>% hchart("line", 
    hcaes(x = formatedDate , y = total), name = "Obsereved Cases") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Cases"))
  
  return(plot)
}
dailyRecoveredPlot <- function(){
  # Daily plot
  plot <-timeSeiresRecoveredDaily %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Recovered") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Recovered"))
  
  return(plot)
}

cumulativeRecoveredPlot <- function(){
  # Cumulative plot 
  plot <-timeSeiresRecoveredCumulative %>% hchart("line", 
    hcaes(x = formatedDate , y = total), name = "Recovered") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Recovered"))
  
  return(plot)
}

dailyDeathsPlot <- function(){
  # Daily deaths plot
  plot <-timeSeiresDeathsDaily %>% hchart("line", 
    hcaes(x = formatedDate , y = daily), name = "Observed Deaths") %>% 
    hc_xAxis(title = list(text = "Dates")) %>% 
    hc_yAxis(title = list(text = "Deaths"))
  
  return(plot)
}
cumulativeDeathsPlot <- function(){
  # Cumulative deaths 
  plot <-timeSeiresDeathsCumulative %>% hchart("line", 
    hcaes(x = formatedDate , y = total), name = "Observed Deaths") %>% 
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

