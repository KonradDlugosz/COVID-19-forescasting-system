totalDailyCasesUK <- function(){
  #Receive data from this source
  source("httpRequest.R")
  
  dailyCases <- getDailyCasesData("dailyCases")
  
  #correct date format:
  dailyCases <- dailyCases %>% 
    mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases))
  
  totalCases <- sum(dailyCases$newCases)
  
  return(totalCases)
}

totalDailyDeathsUK <- function(){
  #Receive data from this source
  source("httpRequest.R")
  
  dailyDeaths <- getDailyCasesData("dailyDeaths")
  
  #correct date format:
  dailyDeaths <- dailyDeaths %>% 
    mutate(date = ymd(dailyDeaths$date), newDeaths28DaysByDeathDate = as.numeric(newDeaths28DaysByDeathDate))
  
  totalDeaths <- sum(dailyDeaths$newDeaths28DaysByDeathDate)
  
  return(totalDeaths)
}

totalNumberOfTests <- function(){
  #Receive data from this source
  source("httpRequest.R")
  
  dailynewTestsByPublishDate <- getDailyCasesData("newTestsByPublishDate")
  
  #correct date format:
  dailynewTestsByPublishDate <- dailynewTestsByPublishDate %>% 
    mutate(date = ymd(dailynewTestsByPublishDate$date), newTestsByPublishDate = as.numeric(newTestsByPublishDate))
  
  totaldailynewTests <- sum(dailynewTestsByPublishDate$newTestsByPublishDate)
  
  
  return(totaldailynewTests)
}

