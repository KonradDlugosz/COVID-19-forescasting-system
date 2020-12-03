#Receive data from this source
source("httpRequest.R")
library(ggplot2)

dailyCasesTimeSeriesPlot <- function(){
  
  dailyCases <- getDailyCasesData("dailyCases")
  
  #correct date format:
  dailyCases <- dailyCases %>% 
    mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases))
  
  sortCasesDesc <- dailyCases %>%
    arrange(desc(newCases))
  
  scale <- max(dailyCases$newCases) + 1000
  
  # plot time Series NewCases:
  dailyCasesTimeSeriesPlot <-  ggplot(data = dailyCases, aes(x=date, y=newCases)) +
    geom_line(color="#FF6B33") +
    geom_area(fill="#FF8E64", alpha=0.7) +
    ylim(0,scale) +
    annotate(geom="text", x=as.Date(sortCasesDesc[1,"date"]), y=sortCasesDesc[1,"newCases"] + 3000, 
             label="Higest number of cases" , size= 5 ) +
    annotate(geom="point", x=as.Date(sortCasesDesc[1,"date"]), y=sortCasesDesc[1,"newCases"], size=5, shape=21, fill="transparent") +
    theme_ipsum()
  
  return(dailyCasesTimeSeriesPlot)
  
}

dailyDeathsTimeSeriesPlot <- function (){
  
  dailyDeaths <- getDailyCasesData("dailyDeaths")
  
  #correct date format:
  dailyDeaths <- dailyDeaths %>% 
    mutate(date = ymd(dailyDeaths$date), newDeaths28DaysByDeathDate = as.numeric(newDeaths28DaysByDeathDate))
  
  scale <- max(dailyDeaths$newDeaths28DaysByDeathDate) + 500
  
  dailyDeathsTimeSeriesPlot <-  ggplot(data = dailyDeaths, aes(x=date, y=newDeaths28DaysByDeathDate)) +
    geom_line(color="#FF3A30") +
    geom_area(fill="#FF655D", alpha=0.7) +
    ylim(0,scale) +
    theme_ipsum()
  
  return(dailyDeathsTimeSeriesPlot)
  
}