getDailyCasesData <- function(selection){
  
  library(httr)
  library(jsonlite)
  library(ggplot2)
  library(dplyr)
  library(plotly)
  library(hrbrthemes)
  library(lubridate)
  
  if(selection == 'dailyCases'){
    
      endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newCases":"newCasesByPublishDate"}'
  }
  else if (selection == 'dailyDeaths'){
    endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newDeaths28DaysByDeathDate":"newDeaths28DaysByDeathDate"}'
  }
  
  else if (selection == "newTestsByPublishDate"){
    endpoint <- 'https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=nation;areaName=england&structure={"date":"date","newTestsByPublishDate":"newTestsByPublishDate"}'
  }
  
  response = GET(endpoint)
  
  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  
  ## Data format
  # Convert binary to JSON:
  json_text <- content(response, "text")
  data      <- jsonlite::fromJSON(json_text)
  
  # Convert list to data-frame:
  data <- as.data.frame(do.call(cbind, data$data))
  
  return(data)
}