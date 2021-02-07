### DATA SOURCE FILE ###
#### Source for the data used: https://github.com/CSSEGISandData/COVID-19
#########################################################################
library (readr)
### Call data GLOBAL DATA FRAME ###
casesDataSet<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deathsDataSet<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveredDataSet<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

activeCases <- function(n){
  firstDay <- ncol(casesDataSet) - n
  lastDay <- firstDay + 13
  active <- casesDataSet[lastDay] -casesDataSet[firstDay]
  df <- data.frame(casesDataSet[1],casesDataSet[2], active)
  names(df)[1] <- "Province/State"
  names(df)[2] <- "Country/Region"
  names(df)[3] <- "Active"
  return(df)
}
totalActiveCases <- function(){
  df <- activeCases(13)
  return(sum(df$Active))
}

activeCasesTimeSeries <- function(){
  # Process data
  firstDay <- ncol(casesDataSet) - 14
  lastDay <- ncol(casesDataSet)
  active <- colSums(casesDataSet[firstDay:lastDay])
  dailyActive <- c()
  time <- Sys.Date()
  # Get daily cases
  for(i in 2:length(active)){
    dailyActive[i - 1] <- active[i] - active[i - 1]
  }
  # Get dates
  for(i in length(dailyActive):1){
    time[i] <- Sys.Date() - i
  }
  # Create dataframe and name columns
  ts <- data.frame(sort(time), dailyActive)
  names(ts)[1] <- "date"
  names(ts)[2] <- "active"
  return(ts)
  
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
  oldDate <- newDate - 2
  twoDaysAllCounties <- data[oldDate:newDate]
  twoDaysData <- colSums(twoDaysAllCounties)
  newDay <- twoDaysData[3]- twoDaysData[2]
  oldDay <- twoDaysData[2]- twoDaysData[1]
  change <- newDay - oldDay
  percentage <- change / oldDay * 100
  percentage <- format(round(percentage, 2), nsmall = 2)
  return(toString(percentage))
}


dailyChangeActiveCases <- function(){
  old <- sum(activeCases(14)[3])
  new <- sum(activeCases(13)[3])
  change <- new - old
  percentage <- change / old * 100
  percentage <- format(round(percentage, 2), nsmall = 2)
  return(percentage)
}

changeIcon <- function(df, recovered){
  if(recovered == TRUE){
    if(df > 0){
      icon <- "sort-down"
    }
    else if(df < 0){
      icon <- "sort-up"
    }
  }
  else if(recovered == FALSE){
    if(df < 0){
      icon <- "sort-down"
    }
    else if(df > 0){
      icon <- "sort-up"
    }
  }
  return(icon)
}

changeIconID <- function(df,bool){
  if(changeIcon(df,bool) == "sort-down" ){
    id <- "sort_downIcon"
  }
  else if(changeIcon(df,bool) =="sort-up"){
    id <- "sort_upIcon"
  }
  return(id)
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
