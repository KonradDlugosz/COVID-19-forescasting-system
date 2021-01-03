### MAP demo ###
library(leaflet)
library (readr)
source("data/covid19Data.R")

# load data 
time_series_covid19_confirmed_global <- cases()
time_series_covid19_deaths_global <- deaths()
newCases <- newConfirmedCases()
newDeaths <- newConfirmedDeaths()

# Last column of the data
LastDateConfirmed <- as.data.frame(time_series_covid19_confirmed_global[,ncol(time_series_covid19_confirmed_global)])
LastDateDeaths <- as.data.frame(time_series_covid19_deaths_global[,ncol(time_series_covid19_deaths_global)])

# Adjust legend range 
casesLegend <- legendAjuster(LastDateConfirmed, 2500000 )
deathsLegend <- legendAjuster(LastDateDeaths, 50000 )

# Create a color palette for cases.
mybins <- seq(0, casesLegend, by=2500000)
mypalette <- colorBin( palette=c("#ff5233", "#330800"), domain=LastDateConfirmed[,1], na.color="#808080", bins = mybins)

# Create a color palette for deaths.
mybins2 <- seq(0, deathsLegend, by=50000)
mypalette2 <- colorBin( palette=c("#ff8533", "#331400"), domain=LastDateConfirmed[,1], na.color="#808080", bins = mybins2)

# Base map
baseMap <- function(){
  baseMap <-leaflet()%>%
    addProviderTiles(providers$CartoDB.Positron) %>% 
    setView(-3.436000, 55.378100, zoom = 2)
  return(baseMap)
}

# Number of cases map
casesMap <- function(){
  casesMap <-leafletProxy("mymap", data = time_series_covid19_confirmed_global)%>%
    clearMarkers() %>%
    clearShapes() %>%
    clearControls() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers( lat = ~Lat, lng = ~Long, color = "#ffcccc", fillColor = ~mypalette(LastDateConfirmed[,1]),weight = 1, radius = ~(LastDateConfirmed[,1])^(1/4),
                      label = sprintf("<strong>%s</strong>, <strong>%s</strong><br/> Confirmed cases: %d<br/> New cases: %d<br/> Confirmed deaths: %d<br/> New deaths: %d<br/>", time_series_covid19_confirmed_global$`Province/State`,time_series_covid19_confirmed_global$`Country/Region`,LastDateConfirmed[,1], newCases[,1] ,LastDateDeaths[,1], newDeaths[,1]) %>% lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal",  direction = "auto", padding = "3px 8px", "color" = "grey"),
                        textsize = "16px"))%>%
    addLegend( pal=mypalette, values=~LastDateConfirmed[,1], opacity=0.7, title = "Number of cases: ", position = "bottomright" )
  
  return(casesMap)
}

# Number of deaths map
deathsMap <- function(){
  deathsMap <- leafletProxy("mymap",data = time_series_covid19_deaths_global)%>%
    clearMarkers() %>%
    clearShapes() %>%
    clearControls() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers( lat = ~Lat, lng = ~Long, color = "#ffcccc", fillColor = ~mypalette2(LastDateDeaths[,1]),weight = 1, radius = ~(LastDateDeaths[,1])^(1/4),
                      label = sprintf("<strong>%s</strong>, <strong>%s</strong><br/> Confirmed cases: %d<br/> New cases: %d<br/> Confirmed deaths: %d<br/> New deaths: %d<br/>", time_series_covid19_confirmed_global$`Province/State`,time_series_covid19_confirmed_global$`Country/Region`,LastDateConfirmed[,1], newCases[,1] ,LastDateDeaths[,1], newDeaths[,1]) %>% lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal",  direction = "auto", padding = "3px 8px", "color" = "grey"),
                        textsize = "16px"))%>%
    addLegend( pal=mypalette2, values=~LastDateDeaths[,1], opacity=0.7, title = "Number of deaths: ", position = "bottomright" )
  
  return(deathsMap)
}

# Adjust legend function
legendAjuster <- function(data, value){
  maxNum <- max(data[,1])
  reminder <- maxNum %% value
  toAdd <- value - reminder 
  maxNum <- maxNum + toAdd
  return(maxNum)
}
