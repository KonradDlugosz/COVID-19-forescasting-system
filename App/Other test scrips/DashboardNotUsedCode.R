box( title = "Map", solidHeader = TRUE,collapsible = TRUE, status = "success", width = 12,
          leafletOutput("dashMap"),
            radioGroupButtons(
                inputId = "dataToDisplay",
                choices = c(`<i class="fas fa-globe-europe"></i>` = "GlobalCases",
                            `<i class="fas fa-skull-crossbones"></i>` = "Deaths", 
                            `<i class="fas fa-band-aid"></i>` = "Recovered"),
                size = "lg")
              
         )

################################################
#devtools::install_github("hrbrmstr/streamgraph")
# Library
library(streamgraph)

# Create data:
data <- data.frame(
  year=rep(seq(1990,2016) , each=10),
  name=rep(letters[1:10] , 27),
  value=sample( seq(0,1,0.0001) , 270)
)

# Basic stream graph: just give the 3 arguments
pp <- streamgraph(data, key="name", value="value", date="year", height="300px", width="1000px")
pp 
