returnFilteredColumns <- function(filterColumnsControls){
  # Define var
  columns <- c("Cases", "Recovered", "Deaths","Cases/1M Population", "Deaths/1M Population","Cases fatality ratio","Population")
  filteredColumns <- c()
  newDataframe <- data.frame(df$Country)
  names(newDataframe)[1] <- "Country"
  
  
  if(length(filterColumnsControls) == 0 ){
    return(newDataframe)
  }
  # Check which columns selected and store in var 
  for(i in 1:length(columns)){
    for(j in 1:length(filterColumnsControls)){
      if(columns[i] == filterColumnsControls[j]){
        filteredColumns[i] <- filterColumnsControls[j]
      }
    }
  }
  na.omit(filteredColumns)
  
  if("Cases" %in% filteredColumns){
    newDataframe$Cases <- df$Cases
  }
  if("Recovered" %in% filteredColumns){
    newDataframe$Recovered <- df$Recovered
  }
  if("Deaths" %in% filteredColumns){
    newDataframe$Deaths <- df$Deaths
  }
  if("Cases/1M Population" %in% filteredColumns){
    newDataframe$`Cases/1M Population` <- df$`Cases/1M Population`
  }
  if("Deaths/1M Population" %in% filteredColumns){
    newDataframe$`Deaths/1M Population` <- df$`Deaths/1M Population`
  }
  if("Cases fatality ratio" %in% filteredColumns){
    newDataframe$`Case Fatality Rate` <- df$`Case Fatality Rate`
  }
  if("Population" %in% filteredColumns){
    newDataframe$Population <- df$Population
  }else {
    
  }
  
  
  return(newDataframe)
 
}
