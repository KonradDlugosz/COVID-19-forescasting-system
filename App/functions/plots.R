### This file contains code used to generate plots of cases, deaths and recovered cases of COVID-19 DATA R script###
#### Sources ####
source("data/covid19Data.R")
library(ggplot2)
library(TTR)
library(dplyr)
library(highcharter)

casesDataSet <- cases()
recoveredDataSet <- recovered()
deathsDataSet <- deaths()

# Format date
numOfCol <- ncol(casesDataSet)
colNames <- colnames(casesDataSet)
date <- colNames[5:numOfCol]
betterDates <- as.Date(date, format = "%m/%d/%y")

cumulitiveTimeSeries <- function(dataSet){
  
  allCountriesData <- dataSet[5:numOfCol]
  total <- colSums(allCountriesData)
  timeSeiresCumulative <- data.frame(betterDates, total)
  rownames(timeSeiresCumulative) <- NULL
  
  return(timeSeiresCumulative)
}

# Time Series Cumulative 
timeSeiresCasesCumulative <- cumulitiveTimeSeries(casesDataSet)
timeSeiresRecoveredCumulative <- cumulitiveTimeSeries(recoveredDataSet)
timeSeiresDeathsCumulative <- cumulitiveTimeSeries(deathsDataSet)

dailyTimeSeries <- function(cumulativeTimeSeries){
  totalC <- cumulativeTimeSeries[,2]
  daily <- c()
  for(i in 1:length(totalC)-1){
    daily[i] <- totalC[i+1] - totalC[i]
  }
  Date <- betterDates[-1]
  dailyTimeSeries <- data.frame(Date, daily )
  
  return(dailyTimeSeries)
}

# Time Series Daily
timeSeiresCasesDaily <- dailyTimeSeries(timeSeiresCasesCumulative)
timeSeiresRecoveredDaily <- dailyTimeSeries(timeSeiresRecoveredCumulative)
timeSeiresDeathsDaily <- dailyTimeSeries(timeSeiresDeathsCumulative)

################ Dashboard plots ################ 
#!!!!!!!!!!!!!!!!!!!!! COLOR OF PLOTS NEEDS CHANGING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dailyCasesPlot <- function(){
  # Correct column name 
  df <- timeSeiresCasesDaily %>% 
    rename(`Daily cases` = daily)
  
  hc <- df %>%
    hchart(
      "line", 
      hcaes(x = Date , y = `Daily cases`)
    )
  return(hc)
}

dailyRecoveredPlot <- function(){
  # Correct column name 
  df <- timeSeiresRecoveredDaily %>% 
    rename(`Daily recovered` = daily)
  
  hc <- df %>%
    hchart(
      "line", 
      hcaes(x = Date , y = `Daily recovered` )
    )
  return(hc)
}

dailyDeathsPlot <- function(){
  # Correct column name 
  df <- timeSeiresDeathsDaily %>% 
    rename(`Daily deaths` = daily)
  
  hc <- df %>%
    hchart(
      "line", 
      hcaes(x = Date , y = `Daily deaths`)
    )
  return(hc)
}

cumulativeCasesPlot <- function(){
  
  plot <-  ggplot(data = timeSeiresCasesCumulative, aes(x=betterDates, y=total)) +
    geom_smooth(color="#FF6B33", size = 1.2)+
    ggtitle("Commutative cases")+
    xlab("Date")+
    ylab("Cases") 
  #interactive plot : cumulativeCases <- ggplotly(cumulativeCases)
  return(plot)
}

cumulativeRecoveredPlot <- function(){
  
  plot <-  ggplot(data = timeSeiresCasesCumulative, aes(x=betterDates, y=total)) +
    geom_smooth(color="#FF6B33", size = 1.2)+
    ggtitle("Commutative Recovered")+
    xlab("Date")+
    ylab("Recovered") 
  
  return(plot)
}

cumulativeDeathsPlot <- function(){
  
  plot <-  ggplot(data = timeSeiresDeathsCumulative, aes(x=betterDates, y=total)) +
    geom_smooth(color="#FF6B33", size = 1.2)+
    ggtitle("Commutative deaths")+
    xlab("Date")+
    ylab("Deaths")
  
  return(plot)
}

################ Country based plots ################ 
countries <- casesDataSet[2]
countries <- countries %>% 
  distinct()

retrunListOfCountries <- function(){
  return(countries)
}

createTimeSeiresForCountry <- function(country){
  #Get data of selected country
  df <- casesDataSet %>% 
    filter(casesDataSet[2]== country)
  df <- df[5:numOfCol]
  df<- t(df)
  d <- data.frame(betterDates, df)
  rownames(d) <- NULL
  # Add Daily change to data frame
  d$daily[1] <- 0
  for(i in 2:nrow(d)){
    d$daily[i] <- d$df[i] - d$df[i - 1] 
  }

  return(d)
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

plotCummulativeForSelectedCountry <- function(countrySelected){
  plot <-  ggplot(data = countrySelected, aes(x=betterDates, y=df)) +
    geom_smooth(color="#FF6B33", size = 1.2)+
    xlab("Date")+
    ylab("Cases") +
    theme_minimal()
  
  return(plot)
}
