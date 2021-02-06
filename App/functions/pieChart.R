# This file creates interactive pie chart
# Load required R packages
source("data/covid19Data.R")
library(dplyr)
library(highcharter) 

pieControler <- function(cases_death_recovered){
  if(cases_death_recovered == "cases"){
    return(piePlotGenerator(casesDataSet, "Cases"))
  }
  else if(cases_death_recovered == "death"){
    return(piePlotGenerator(deathsDataSet, "Deaths"))
  }
  else if(cases_death_recovered == "recovered"){
    return(piePlotGenerator(recoveredDataSet, "Recovered"))
  }
  else if(cases_death_recovered == "active"){
    return(piePlotGenerator(pieActiveCasesData(), "Active"))
  }
}

piePlotGenerator <- function(dataFrame, dataName){
  # Set highcharter options
  options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
  data <- dataFrame
  
  if(dataName == "Cases" | dataName == "Deaths" | dataName == "Recovered"){
    df <- data.frame(data[ncol(data)], data[2])
    names(df)[1] <- "Value"
  }
  else if(dataName == "Active"){
    df <- data.frame(data[2], data[1])
    names(df)[1] <- "Value"
  }
  
  df_sorted <-df[order(df[1]),]
  
  hc <- df_sorted %>% 
    hchart("pie", hcaes(x = Country.Region, y = "Value"),name = dataName)
  
  return(hc)
  
}
