#Receive data from this source
source("httpRequest.R")
library(ggplot2)

### Global Variable ###
k = 7

dailyCasesTimeSeriesPlot <- function(checkBoxMA){
  
  # Data prep
  dailyCases <- getDailyCasesData("dailyCases")
  dailyCases <- dailyCases %>% 
    mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases))%>%
    arrange(date)
  
  sortCasesDesc <- dailyCases %>%
    arrange(desc(newCases))
  
  plotScale <- max(dailyCases$newCases) + 1000
  
  # Calculate predictions(Moving Average)
  dailyCases$Prediction <- TTR::SMA(dailyCases$newCases, n = k)
  
  # plot time Series NewCases:
  dailyCasesTimeSeriesPlot <-  ggplot(data = dailyCases, aes(x=date, y=newCases)) +
    ggtitle("Daily cases")+
    xlab("Date")+
    ylab("New Cases")+
    geom_line(color="#FF6B33", size = 1.2) +
    geom_area(fill="#FF8E64", alpha=0.9) +
    ylim(0,plotScale) +
    theme_ipsum()
  
  # Check if Moving average should be applied 
  if(checkBoxMA == TRUE){
    dailyCasesTimeSeriesPlot <- dailyCasesTimeSeriesPlot + geom_line(data = dailyCases, aes(x = date, y = Prediction), color = "blue", size = 1.2, alpha = 0.7)
  }
  
  return(dailyCasesTimeSeriesPlot)
  
}

dailyCasesLinear <- function(){
  
  #Data preparation
  dailyCases <- getDailyCasesData("dailyCases")
  dailyCases <- dailyCases %>% 
    arrange(date) %>%
    mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases))
  
  #Values for for loop
  value <- dailyCases$newCases
  sum=c()
  sum[1] = value[1]
  
  #Linear calculation 
  for(i in 1:length(value))
  { 
    sum[i+1] = sum[i] + value[i + 1]
  }
  
  #Delete last element "N/A"
  sum <- sum[-length(sum)]

  dailyCases <- dailyCases %>% 
    arrange(date)%>%
    mutate(newCases = sum)%>%
    arrange(desc(date))
  
  plotScale <- dailyCases$newCases[1] + 50000
  dailyCasesLinearPlot <-  ggplot(data = dailyCases, aes(x=date, y=newCases)) +
    geom_smooth(color="#FF6B33", size = 1.2)+
    ylim(0,plotScale) +
    ggtitle("Commutative cases")+
    xlab("Date")+
    ylab("New Cases")+
    theme_ipsum()
  
  return(dailyCasesLinearPlot)
  
}

dailyDeathsTimeSeriesPlot <- function (checkBoxMA){
  
  # Data preparation
  dailyDeaths <- getDailyCasesData("dailyDeaths")
  dailyDeaths <- dailyDeaths %>% 
    mutate(date = ymd(dailyDeaths$date), newDeaths28DaysByDeathDate = as.numeric(newDeaths28DaysByDeathDate))%>%
    arrange(date)
  
  plotScale <- max(dailyDeaths$newDeaths28DaysByDeathDate) + 500
  
  # Calculate predictions(Moving Average)
  dailyDeaths$Prediction <- TTR::SMA(dailyDeaths$newDeaths28DaysByDeathDate, n = k)
  
  dailyDeathsTimeSeriesPlot <-  ggplot(data = dailyDeaths, aes(x=date, y=newDeaths28DaysByDeathDate)) +
    geom_line(color="#FF3A30", size = 1.2) +
    geom_area(fill="#FF655D", alpha=0.9) +
    ggtitle("Daily deaths") + 
    xlab("Date")+
    ylab("Number of deaths")+
    ylim(0,plotScale) +
    theme_ipsum()
  
  # Check if Moving average should be applied 
  if(checkBoxMA == TRUE){
    dailyDeathsTimeSeriesPlot <- dailyDeathsTimeSeriesPlot + geom_line(data = dailyDeaths, aes(x = date, y = Prediction), color = "blue", size = 1.2, alpha = 0.7)
  }
  
  return(dailyDeathsTimeSeriesPlot)
  
}

dailyCommutativeDeathsPlot <- function(){
  
  #Data preparation
  dailyDeaths <- getDailyCasesData("dailyDeaths")
  dailyDeaths <- dailyDeaths %>% 
    arrange(date) %>%
    mutate(date = ymd(dailyDeaths$date), newDeaths28DaysByDeathDate = as.numeric(newDeaths28DaysByDeathDate))
  
  #Values for for loop
  value <- dailyDeaths$newDeaths28DaysByDeathDate
  sum=c()
  sum[1] = value[1]
  
  #Linear calculation 
  for(i in 1:length(value))
  { 
    sum[i+1] = sum[i] + value[i + 1]
  }
  
  #Delete last element "N/A"
  sum <- sum[-length(sum)]
  
  dailyDeaths <- dailyDeaths %>% 
    arrange(date)%>%
    mutate(newDeaths28DaysByDeathDate = sum)%>%
    arrange(desc(date))
  
  plotScale <- dailyDeaths$newDeaths28DaysByDeathDate[1] + 5000
  
  dailyCommutativeDeathsPlot <-  ggplot(data = dailyDeaths, aes(x=date, y=newDeaths28DaysByDeathDate)) +
    geom_smooth(color="#FF3A30", size = 1.2)+
    ylim(0,plotScale) +
    ggtitle("Commutative deaths")+
    xlab("Date")+
    ylab("Number of Deaths")+
    theme_ipsum()

  return(dailyCommutativeDeathsPlot)
  
}