### This file contains code used to generate plots of cases, deaths and recovered cases of COVID-19 ###
#### Sources ####
source("data/covid19Data.R")
library(ggplot2)
library(TTR)
library(dplyr)

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
  dailyCases <- c()
  for(i in 1:length(totalC)-1){
    dailyCases[i] <- totalC[i+1] - totalC[i]
  }
  formatedDate <- betterDates[-1]
  dailyTimeSeries <- data.frame(formatedDate, dailyCases )
  
  return(dailyTimeSeries)
}

# Time Series Daily
timeSeiresCasesDaily <- dailyTimeSeries(timeSeiresCasesCumulative)
timeSeiresRecoveredDaily <- dailyTimeSeries(timeSeiresRecoveredCumulative)
timeSeiresDeathsDaily <- dailyTimeSeries(timeSeiresDeathsCumulative)

################ Dashboard plots ################ 
dailyCasesPlot <- function(){
  
  plotScale <- max(timeSeiresCasesDaily$dailyCases) + 10000
  
  plot <-  ggplot(data = timeSeiresCasesDaily, aes(x=formatedDate, y=dailyCases)) +
    ggtitle("Daily cases")+
    xlab("Date")+
    ylab("Cases")+
    geom_line(color="#ff6600", size = 1.2, alpha = 0.9 ) +
    geom_area(fill="#ffa366", alpha=0.5) +
    ylim(0,plotScale) +
    theme_minimal()
  return(plot)
}

dailyRecoveredPlot <- function(){
  
  plotScale <- max(timeSeiresRecoveredDaily$dailyCases) + 10000
  
  plot <-  ggplot(data = timeSeiresRecoveredDaily, aes(x=formatedDate, y=dailyCases)) +
    ggtitle("Daily recovered")+
    xlab("Date")+
    ylab("Recovered")+
    geom_line(color="#0066ff", size = 1.2, alpha = 0.9 ) +
    geom_area(fill="#66a3ff", alpha=0.5) +
    ylim(0,plotScale) +
    theme_minimal()
  return(plot)
}

dailyDeathsPlot <- function(){
  
  plotScale <- max(timeSeiresDeathsDaily$dailyCases) + 10000
  
  plot <-  ggplot(data = timeSeiresDeathsDaily, aes(x=formatedDate, y=dailyCases)) +
    ggtitle("Daily deaths")+
    xlab("Date")+
    ylab("Deaths")+
    geom_line(color="#ff0000", size = 1.2, alpha = 0.9 ) +
    geom_area(fill="#ff6666", alpha=0.5) +
    ylim(0,plotScale) +
    theme_minimal()
  return(plot)
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
  return(formatLargeNumber(total))
}

t <- returnSumCasesOfCountry(createTimeSeiresForCountry("Poland"))

plotDailyForSelectedCountry <- function(countrySelected){
  plot <-  ggplot(data = countrySelected, aes(x=betterDates, y=daily)) +
    xlab("Date")+
    ylab("New Cases")+
    geom_line(color="#FF6B33", size = 1.2, alpha = 0.9 ) +
    geom_area(fill="#FF8E64", alpha=0.9) +
    theme_minimal()
  
  return(plot)
}

plotCummulativeForSelectedCountry <- function(countrySelected){
  plot <-  ggplot(data = countrySelected, aes(x=betterDates, y=df)) +
    geom_smooth(color="#FF6B33", size = 1.2)+
    xlab("Date")+
    ylab("Cases") +
    theme_minimal()
  
  return(plot)
}

################ Neural network and achrtecture functions ################
data <- createTimeSeiresForCountry("Germany")

df <- ts(data$daily)

fit <- nnetar(df, repeats = 20)
fcast <- forecast(fit, h = 14 )
plot(fcast)

forcastedCases <- as.integer(fcast$mean)

for(i in 1:14){
  newDates[i] <- data$betterDates[nrow(data)] + i
}

fCases <- data.frame(newDates, forcastedCases)

plot <-  ggplot(NULL) +
  xlab("Date")+
  ylab("New Cases")+
  geom_line(data = data, aes(x=betterDates, y=daily),color="#FF6B33", size = 1.2, alpha = 0.9 ) +
  geom_area(data = data, aes(x=betterDates, y=daily),fill="#FF8E64", alpha=0.9) +
  geom_line(data = fCases,aes(x=newDates, y=forcastedCases), color="#cc6600" ,size = 1.2, alpha = 0.9 ) + 
  geom_area(data = fCases,aes(x=newDates, y=forcastedCases),fill="#ffb366", alpha=0.9)


#### Normalize function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# Normalize data
data$daily[1] <- 1 # add missing value, gives error without it.
data$daily_norm <- normalize(data$daily)

