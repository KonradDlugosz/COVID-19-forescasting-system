### Sources ####
source("data/httpRequest.R")

totalDailyCasesUK <- function(){
  
  dailyCases <- getDailyCasesData("dailyCases")
  
  dailyCases <- dailyCases %>% 
    mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases))
  
  totalCases <- sum(dailyCases$newCases)
  
  return(formatLargeNumber(totalCases))
}

totalDailyDeathsUK <- function(){
  
  dailyDeaths <- getDailyCasesData("dailyDeaths")
  
  dailyDeaths <- dailyDeaths %>% 
    mutate(date = ymd(dailyDeaths$date), newDeaths28DaysByDeathDate = as.numeric(newDeaths28DaysByDeathDate))
  
  totalDeaths <- sum(dailyDeaths$newDeaths28DaysByDeathDate)
  
  return(formatLargeNumber(totalDeaths))
}

totalNumberOfTests <- function(){

  dailynewTestsByPublishDate <- getDailyCasesData("newTestsByPublishDate")
  
  dailynewTestsByPublishDate <- dailynewTestsByPublishDate %>% 
    mutate(date = ymd(dailynewTestsByPublishDate$date), newTestsByPublishDate = as.numeric(newTestsByPublishDate))
  
  totaldailynewTests <- sum(dailynewTestsByPublishDate$newTestsByPublishDate)
  
  
  return(formatLargeNumber(totaldailynewTests))
}

formatLargeNumber <- function(number){
  
  formatedNumber<-format(round(as.numeric(number), 1), nsmall=0, big.mark=",")
  return(formatedNumber)
  
}

