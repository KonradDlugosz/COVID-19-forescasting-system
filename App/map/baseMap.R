### MAP demo ###
library(leaflet)
library (readr)
source("data/covid19Data.R")

# load data 
time_series_covid19_confirmed_global <- cases()
time_series_covid19_deaths_global <- deaths()
newCases <- newConfirmedCases()
newDeaths <- newConfirmedDeaths()

#Last column of the data
LastDateConfirmed <- as.data.frame(time_series_covid19_confirmed_global[,ncol(time_series_covid19_confirmed_global)])
LastDateDeaths <- as.data.frame(time_series_covid19_deaths_global[,ncol(time_series_covid19_deaths_global)])

# Create a color palette with handmade bins.
maxNum <- max(LastDateConfirmed[,1])
mybins <- seq(0, maxNum+1000000, by=2500000)
mypalette <- colorBin( palette=c("#ff5233", "#330800"), domain=LastDateConfirmed[,1], na.color="#808080", bins = mybins)

baseMap <- function(){
  basemap <-leaflet(data = time_series_covid19_confirmed_global)%>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers( lat = ~Lat, lng = ~Long, color = "#ffcccc", fillColor = ~mypalette(LastDateConfirmed[,1]),weight = 1, radius = ~(LastDateConfirmed[,1])^(1/4),
                      label = sprintf("<strong>%s</strong>, <strong>%s</strong><br/> Confirmed cases: %d<br/> New cases: %d<br/> Confirmed deaths: %d<br/> New deaths: %d<br/>", time_series_covid19_confirmed_global$`Province/State`,time_series_covid19_confirmed_global$`Country/Region`,LastDateConfirmed[,1], newCases[,1] ,LastDateDeaths[,1], newDeaths[,1]) %>% lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal",  direction = "auto", padding = "3px 8px", "color" = "grey"),
                        textsize = "16px"))%>%
    addLegend( pal=mypalette, values=~LastDateConfirmed[,1], opacity=0.9, title = "Cases", position = "bottomright" )
  
  return(basemap)
  
}
