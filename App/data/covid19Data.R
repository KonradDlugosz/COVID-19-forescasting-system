### DATA SOURCE FILE ###
#### Source for the data used: https://github.com/CSSEGISandData/COVID-19
#########################################################################
library (readr)
### Call data GLOBAL DATA FRAME ###
casesDataSet<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deathsDataSet<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveredDataSet<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

activeCases <- function(){
  firstDay <- ncol(casesDataSet) - 13
  lastDay <- ncol(casesDataSet)
  active <- casesDataSet[lastDay] -casesDataSet[firstDay]
  df <- data.frame(casesDataSet[1],casesDataSet[2], active)
  names(df)[1] <- "Province/State"
  names(df)[2] <- "Country/Region"
  names(df)[3] <- "Active"
  return(df)
}
totalActiveCases <- function(){
  df <- activeCases()
  return(sum(df$Active))
}

pieActiveCasesData <- function(){
  df <- activeCases()
  df <- df %>% group_by(`Country/Region`) %>% 
    summarise(n = sum(Active))
  return(df)
}
#### Global cases ####
todayCases <- function() {
  data <- casesDataSet
  todayCases <- data[ncol(data)] - data[ncol(data) -1]
  return(sum(todayCases))
}
# New Cases
newCasesWeekly <- function(){
  data <- casesDataSet
  endOfWeekDate <- ncol(data)
  startOfWeekDate <- endOfWeekDate - 7 
  newcases <- data[endOfWeekDate] - data[startOfWeekDate]
  return(newcases)
}

# Daily increase in % new cases 
dailyChange <- function(dataFrame){
  data <- dataFrame
  newDate <- ncol(data)
  oldDate <- newDate - 1
  twoDaysAllCounties <- data[oldDate:newDate]
  twoDaysData <- colSums(twoDaysAllCounties)
  change <- twoDaysData[2] - twoDaysData[1]
  procentage <- change / twoDaysData[1] * 100
  procentage <- format(round(procentage, 2), nsmall = 2)
  return(toString(procentage))
}

# Total cases 
totalCases <- function(){
  data <- casesDataSet
  total <- sum(data[ncol(data)])
  return(total)
}

#### Global deaths ####
todayDeaths <- function() {
  data <- deathsDataSet
  todayDeaths <- data[ncol(data)] - data[ncol(data) -1]
  return(sum(todayDeaths))
} 

newDeathsWeekly <- function(){
  data <- deathsDataSet
  endOfWeekDate <- ncol(data)
  startOfWeekDate <- endOfWeekDate - 7 
  newDeaths <-  data[endOfWeekDate] - data[startOfWeekDate]
  return(newDeaths)
}

# Total deaths 
totalDeaths <- function(){
  data <- deathsDataSet
  total <- sum(data[ncol(data)])
  formatedNumber<-formatLargeNumber(total)
  return(formatedNumber)
}

#### Global Recovered ####
todayRecovered <- function() {
  data <- recoveredDataSet
  todayRecovered <- data[ncol(data)] - data[ncol(data) -1]
  return(sum(todayRecovered))
} 

weeklyRecovered <- function(){
  data <- recoveredDataSet
  endOfWeekDate <- ncol(data)
  startOfWeekDate <- endOfWeekDate - 7 
  newRecovered <- data[endOfWeekDate] - data[startOfWeekDate]
  return(sum(newRecovered))
}

# Total recovered
totalRecovered <- function(){
  data <- recoveredDataSet
  total <- sum(data[ncol(data)])
  formatedNumber<-formatLargeNumber(total)
  return(formatedNumber)
}

# Cases, deaths, recovered in data frame
TotalStats <- function(){
  
  cases <- totalCases()
  deaths <- totalDeaths()
  recovered <- totalRecovered()
  
  title <- c("Cases", "Deaths", "Recovered")
  data <- c(cases,deaths, recovered)
  
  df <- data.frame(title, data)
  
  return(df)
}

#### Active cases ####
todayActiveCases <- function(){
  active <- todayCases() - todayDeaths() - todayRecovered()
  return(active)
}

weeklyActiveCases <- function(){
  activeWeekly <- sum(newCasesWeekly()) - sum(newDeathsWeekly()) - sum(weeklyRecovered())
  return(activeWeekly)
}

### Other function ### 
formatLargeNumber <- function(number){
  
  formatedNumber<-format(round(as.numeric(number), 1), nsmall=0, big.mark=",")
  return(formatedNumber)
  
}
