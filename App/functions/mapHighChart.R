# This file creates highchart map
source("data/covid19Data.R")
library(dplyr)
library(highcharter)

# Get country codes
countryCodes <-read_csv("https://gist.githubusercontent.com/radcliff/f09c0f88344a7fcef373/raw/2753c482ad091c54b1822288ad2e4811c021d8ec/wikipedia-iso-country-codes.csv")

hcmapSelector <- function(data){
  if(data == "cases"){
    return(hcmapGenerator(cases(), "Cases"))
  }
  else if(data == "deaths"){
    return(hcmapGenerator(deaths(), "Deaths"))
  }
  else if(data == "recovered"){
    return(hcmapGenerator(recovered(), "Recovered"))
  }
}

hcmapGenerator <- function(df, name){
  # Combine with COVID data 
  for(i in 1:nrow(df)){
    for(j in 1:nrow(countryCodes)){
      if(df$`Country/Region`[i] == countryCodes$`English short name lower case`[j]){
        df$`Province/State`[i] <- countryCodes$`Alpha-2 code`[j]
      }
    }
  }
  
  if(name == "Cases" | name == "Deaths"){
    # Manual correction NA country codes for CASES and DEATHS
    df$`Province/State`[250] <- "US"
    df$`Province/State`[28] <- "BO"
    df$`Province/State`[32] <- "BN"
    df$`Province/State`[94] <- "CG"
    df$`Province/State`[95] <- "CG"
    df$`Province/State`[101] <- "CZ"
    df$`Province/State`[143] <- "VA"
    df$`Province/State`[149] <- "IR"
    df$`Province/State`[159] <- "KR"
    df$`Province/State`[168] <- "LY"
    df$`Province/State`[183] <- "FM"
    df$`Province/State`[184] <- "MD"
    df$`Province/State`[190] <- "NA"
    df$`Province/State`[214] <- "RU"
    df$`Province/State`[240] <- "SY"
    df$`Province/State`[241] <- "TW"
    df$`Province/State`[243] <- "TZ"
    df$`Province/State`[268] <- "VE"
    df$`Province/State`[269] <- "VN"
    
  }
  else if(name == "Recovered"){
    # Manual correction NA country codes for RECOVERED
    df$`Province/State`[28] <- "BO"
    df$`Province/State`[32] <- "BN"
    df$`Province/State`[79] <- "CG"
    df$`Province/State`[86] <- "CZ"
    df$`Province/State`[134] <- "IR"
    df$`Province/State`[154] <- "LY"
    df$`Province/State`[225] <- "SY"
    df$`Province/State`[226] <- "TW"
    df$`Province/State`[228] <- "TZ"
    df$`Province/State`[235] <- "US"
    df$`Province/State`[253] <- "VE"
    df$`Province/State`[254] <- "VN"
  }
  
  # Correct column names
  names(df)[1] <- "code"
  names(df)[ncol(df)] <- "lastDate"
  code <- df$code
  lastDate <- df$lastDate
  
  mapData <- data.frame(code, lastDate)
  
  dfData <- mapData %>% 
    group_by(code) %>% 
    summarize(value = sum(lastDate))
  
  map <- hcmap(
    map = "custom/world-robinson-lowres", data = dfData,
    joinBy = c("iso-a2", "code"), value = "value", name = name, download_map_data = FALSE,
    borderWidth = 0
  ) 
  
  # Apply color
  if(name == "Cases"){
    map <- map %>%
      hc_colorAxis(
        stops = color_stops(colors = c("#ffa366", "#cc5200"))
      ) 
  }
  else if (name == "Deaths"){
    map <- map %>%
      hc_colorAxis(
        stops = color_stops(colors = c("#ff8080", "#cc0000"))
      ) 
  }
  else if (name == "Recovered"){
    map <- map %>%
      hc_colorAxis(
        stops = color_stops(colors = c("#80ccff", "#007acc"))
      ) 
  }

  return(map)
}

