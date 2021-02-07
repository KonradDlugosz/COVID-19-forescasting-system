# This file creates interactive pie chart
# Load required R packages
source("data/covid19Data.R")
library(dplyr)
library(highcharter) 

#### PROCCES DATA ####
# Cases Data 
pieCasesData <- function(){
  df <- casesDataSet
  data <- data.frame(df[2],df[ncol(df)])
  names(data)[1] <- "Country/Region"
  names(data)[2] <- "Active" 
  data <- data %>% group_by(`Country/Region`) %>% 
    summarise(n = sum(Active))
  return(data)
}

# Recovered Data 
pieRecoveredData <- function(){
  df <- recoveredDataSet
  data <- data.frame(df[2],df[ncol(df)])
  names(data)[1] <- "Country/Region"
  names(data)[2] <- "Active" 
  data <- data %>% group_by(`Country/Region`) %>% 
    summarise(n = sum(Active))
  return(data)
}

# Deaths data
pieDeathsData <- function(){
  df <- deathsDataSet
  data <- data.frame(df[2],df[ncol(df)])
  names(data)[1] <- "Country/Region"
  names(data)[2] <- "Active" 
  data <- data %>% group_by(`Country/Region`) %>% 
    summarise(n = sum(Active))
  return(data)
}

# Active Data
pieActiveCasesData <- function(){
  df <- activeCases()
  df <- df %>% group_by(`Country/Region`) %>% 
    summarise(n = sum(Active))
  return(df)
}

pieControler <- function(cases_death_recovered){
  if(cases_death_recovered == "cases"){
    return(piePlotGenerator(pieCasesData(), "Cases"))
  }
  else if(cases_death_recovered == "death"){
    return(piePlotGenerator(pieDeathsData(), "Deaths"))
  }
  else if(cases_death_recovered == "recovered"){
    return(piePlotGenerator(pieRecoveredData(), "Recovered"))
  }
  else if(cases_death_recovered == "active"){
    return(piePlotGenerator(pieActiveCasesData(), "Active"))
  }
}

piePlotGenerator <- function(dataFrame, dataName){
  # Set highcharter options
  options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
  data <- dataFrame
  
  df <- data.frame(data[2], data[1])
  names(df)[1] <- "Value"
  
  df_sorted <-df[order(df[1]),]
  
  hc <- df_sorted %>% 
    hchart("pie", hcaes(x = Country.Region, y = "Value"),name = dataName)
  
  return(hc)
  
}

pieControler("cases")
