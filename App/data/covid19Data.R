### DATA SOURCE FILE ###
#### Source for the data used: https://github.com/CSSEGISandData/COVID-19
#########################################################################
library (readr)
### Call data ###
time_series_covid19_confirmed_global<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
time_series_covid19_deaths_global<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
time_series_covid19_recovered_global<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

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
# 7 day % change in New cases
sevenDayChange <- function(){
  data <- cases()
  n2 <- ncol(data)
  n1 <- s - 8
  sevenDaysDataAllCountries <- data[n1:n2]
  sevenDaysData <- colSums(sevenDaysDataAllCountries)
  newData <-c()
  difrenceInNewData <- c()
  for(i in 1:8){
    newData[i]<- sevenDaysData[i+1] - sevenDaysData[i]
  }
  for(i in 1:8){
    difrenceInNewData[i] <- newData[i+1] - newData[i] 
  }
  change <- sum(difrenceInNewData,na.rm=TRUE)
  changeInProcentage <- change/100
  ## Check if this is correct
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

### Global Recovered ###
recovered <- function(){
  return(time_series_covid19_recovered_global)
}

# Total recovered
totalRecovered <- function(){
  data <- recovered()
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

