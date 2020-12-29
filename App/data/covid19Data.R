### DATA SOURCE FILE ###
#### Source for the data used: https://github.com/CSSEGISandData/COVID-19
#########################################################################
library (readr)
### Call data ###
time_series_covid19_confirmed_global<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
time_series_covid19_deaths_global<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

### Global cases ###
cases <- function(){
  return(time_series_covid19_confirmed_global)
}

# New Cases
newConfirmedCases <- function(){
  data <- cases()
  endOfWeekDate <- ncol(data)
  startOfWeekDate <- endOfWeekDate - 7 
  newcases <- data[endOfWeekDate] - data[startOfWeekDate]
  return(newcases)
}

# Total cases 
totalCases <- function(){
  data <- cases()
  total <- sum(data[ncol(data)])
  formatedNumber<-formatLargeNumber(total)
  return(formatedNumber)
}

# total new Cases
totalNewCases <- function(){
  data <- newConfirmedCases()
  total <-sum(data[,1])
  formatedNumber<-formatLargeNumber(total)
  return(formatedNumber)
  
}

### Global deaths ###
deaths <- function(){
  return(time_series_covid19_deaths_global)
}

# New deaths
newConfirmedDeaths <- function(){
  data <- deaths()
  endOfWeekDate <- ncol(data)
  startOfWeekDate <- endOfWeekDate - 7 
  newDeaths <-  data[endOfWeekDate] - data[startOfWeekDate]
  return(newDeaths)
}

# Total deaths 
totalDeaths <- function(){
  data <- deaths()
  total <- sum(data[ncol(data)])
  formatedNumber<-formatLargeNumber(total)
  return(formatedNumber)
}

# Total new deaths 
totalNewDeaths <- function(){
  data <- newConfirmedDeaths()
  total <-sum(data)
  formatedNumber<-formatLargeNumber(total)
  return(formatedNumber)
  
}

### Other function ### 
formatLargeNumber <- function(number){
  
  formatedNumber<-format(round(as.numeric(number), 1), nsmall=0, big.mark=",")
  return(formatedNumber)
  
}
