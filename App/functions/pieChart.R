# This file creates interactive pie chart
# Load required R packages
library(dplyr)
library(highcharter) 
source("data/covid19Data.R")

pieControler <- function(cases_death_recovered){
  if(cases_death_recovered == "cases"){
    cases <- cases()
    return(piePlotGenerator(cases, "Cases"))
  }
  else if(cases_death_recovered == "death"){
    deaths <- deaths()
    return(piePlotGenerator(deaths, "Deaths"))
  }
  else if(cases_death_recovered == "recovered"){
    recovered <- recovered()
    return(piePlotGenerator(recovered, "Recovered"))
  }
}

piePlotGenerator <- function(dataFrame, dataName){
  # Set highcharter options
  options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
  
  data <- dataFrame
  df <- data.frame(data[ncol(data)], data[2])
  names(df)[1] <- "Value"
  
  df_sorted <-df[order(df[1]),]
  
  hc <- df_sorted %>% 
    hchart("pie", hcaes(x = Country.Region, y = "Value"),name = dataName)
  
  return(hc)
  
}
