#Receive data from this source
source("httpRequest.R")

totalDailyCasesUK <- function(){
  
  dailyCases <- getDailyCasesData("dailyCases")
  
  dailyCases <- dailyCases %>% 
    mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases))
  
  totalCases <- sum(dailyCases$newCases)
  
  return(totalCases)
}

totalDailyDeathsUK <- function(){
  
  dailyDeaths <- getDailyCasesData("dailyDeaths")
  
  dailyDeaths <- dailyDeaths %>% 
    mutate(date = ymd(dailyDeaths$date), newDeaths28DaysByDeathDate = as.numeric(newDeaths28DaysByDeathDate))
  
  totalDeaths <- sum(dailyDeaths$newDeaths28DaysByDeathDate)
  
  return(totalDeaths)
}

totalNumberOfTests <- function(){

  dailynewTestsByPublishDate <- getDailyCasesData("newTestsByPublishDate")
  
  dailynewTestsByPublishDate <- dailynewTestsByPublishDate %>% 
    mutate(date = ymd(dailynewTestsByPublishDate$date), newTestsByPublishDate = as.numeric(newTestsByPublishDate))
  
  totaldailynewTests <- sum(dailynewTestsByPublishDate$newTestsByPublishDate)
  
  
  return(totaldailynewTests)
}

