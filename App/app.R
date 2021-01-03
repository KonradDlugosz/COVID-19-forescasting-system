## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)

### Sources ####
source("forecasting/dailyTimeSeriesPlot.R")
source("forecasting/valueBoxesReturns.R")
source("forecasting/arimaModel.R")
source("data/covid19Data.R")
source("map/baseMap.R")

### Shiny app ###
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      ##Source for icons https://fontawesome.com/
      menuItem("Dashboard", tabName = "dashboard", icon = icon("virus")),
      menuItem("Tracker", tabName = "tracker", icon = icon("map-marked-alt")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("laptop-code")),
      menuItem("About", tabName = "about", icon = icon("user-secret"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      
      ### First tab content ###
      tabItem(tabName = "dashboard",
              includeCSS("styles.css"),
              h1("Dashboard tab content")
      ),
      
      ### Second tab content ###
      tabItem(tabName = "tracker",
                leafletOutput("mymap", height = "850"),
                # Stats Display
                absolutePanel(id = "stats", class = "panel panel-default",
                              top = 70, left = "auto",right = 20, bottom = "auto" , width = "auto", fixed=TRUE, height = "auto", 
                              h3("Global situation: "),
                              h4(id = "map-info-new" ,sprintf("New cases (Last 7 days): %s", totalNewCases())),
                              h5(id = "map-info-confirmed" ,sprintf("Confirmed cases: %s", totalCases())),
                              h4(id = "map-info-new" ,sprintf("New deaths (Last 7 days): %s", totalNewDeaths())),
                              h5(id = "map-info-confirmed" ,sprintf("Confirmed deaths: %s", totalDeaths()))),
              # Controls Display
              absolutePanel(id = "controls", top = 150, bottom = "auto", right = "auto", height = "auto", width = "auto", fixed = TRUE,
                            h3("Controls for map: "),
                            fluidRow( id = "controls-buttons", 
                                      
                              actionButton("cases", "Cases"),
                              actionButton("deaths", "Deaths")
                            )
                            
                            
                            )
      ),
    
      
      ### Third tab content ###
      tabItem(tabName = "forecasting",
              fluidRow(
                column(width = 8,
                       #Value Boxes
                       valueBoxOutput("Cases"),
                       valueBoxOutput("Deaths"),
                       valueBoxOutput("Tests")
                )
              ),
              
              fluidRow(
                column(width = 12, 
                       box(
                         title = "Daily Time Series plot", solidHeader = TRUE, collapsible = TRUE, status = "primary", br(), width = 12,
                         sidebarPanel(
                           h3("Customise: "), br(),
                           selectInput("plotData", choices = c("New cases", "Deaths"), label = "Select data: "),
                           selectInput("plotType", choices = c("Daily", "Commutative"), label = "Select plot type: "),
                           h4("Moving Averages: "),
                           checkboxInput("checkBoxMA", label = "Simple Moving Average", value = FALSE),
                           checkboxInput("checkBoxEMA", label = "Exponential Moving Average", value = FALSE)
                         ),
                         mainPanel(
                           plotOutput("plotOutput",  height = "500px")
                         )),
                       
                       box(
                         title = "ARIMA Model", solidHeader = TRUE, collapsible = TRUE, status = "primary", br(), width = 12,
                         sidebarPanel(
                           h3("Forecast from ARIMA: "), br(),
                           selectInput("plotData2", choices = c("New cases", "Deaths"), label = "Select data: "),
                           sliderInput("movingAvergeNum", "Moving average period:",
                                       min = 0, max = 50, value = 7),
                           sliderInput("numberOfForecastDays", "Number of days to forecast:",
                                       min = 0, max = 50, value = 7),
                           p("As the number grows, the accuracy decreases.")
                         ),
                         mainPanel(
                           plotOutput("arimaOutput",  height = "500px")
                         )),
                )
              )
      ),
      
      ### Forth tab content  ###
      tabItem(tabName = "about")
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  
  # Map starts -----------------------------------------------------------------
  output$mymap <- renderLeaflet({ 
    baseMap()
  })
  
  observeEvent(input$cases, {
    casesMap()
  })
  
  observeEvent(input$deaths, {
    deathsMap()
  })
 
  # Map ends -------------------------------------------------------------------
  # Forecasting start ----------------------------------------------------------
  
  output$Cases <- renderValueBox({
    
    valueBox(
      totalDailyCasesUK(), "Total Cases", icon = icon("briefcase-medical"),
      color = "orange"
    )
  })
  
  output$Deaths <- renderValueBox({
    valueBox(
      totalDailyDeathsUK(), "Total deaths ", icon = icon("skull-crossbones"),
      color = "red"
    )
  })
  
  output$Tests <- renderValueBox({
    valueBox(
      totalNumberOfTests(), "Total tests", icon = icon("vials"),
      color = "green"
    )
  })
  
  # Plot mechanism selection render
  output$plotOutput <- renderPlot({
    if(input$plotData == "New cases"){
      
      if(input$plotType == "Daily"){
        dailyCasesTimeSeriesPlot(input$checkBoxMA, input$checkBoxEMA)
      }
      else if(input$plotType == "Commutative"){    
        dailyCasesLinear()
      }
    }
    else if(input$plotData == "Deaths"){
      
      if(input$plotType == "Daily"){
        dailyDeathsTimeSeriesPlot(input$checkBoxMA, input$checkBoxEMA)
      }
      else if(input$plotType == "Commutative"){
        dailyCommutativeDeathsPlot()
      }
    }
  })
  
  
  # ARIMA Mechanism Selection render
  output$arimaOutput <- renderPlot({
    if(input$plotData2 == "New cases"){
      dailyCasesARIMA(input$movingAvergeNum, input$numberOfForecastDays)
    }
    else if(input$plotData2 == "Deaths"){
      dailyDeathsARIMA(input$movingAvergeNum, input$numberOfForecastDays)
    }
    
  })
  
  # Forecasting End ------------------------------------------------------------
  
}

shinyApp(ui, server)
