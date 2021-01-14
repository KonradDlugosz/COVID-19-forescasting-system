## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
library(shinyWidgets)

### Sources ####
source("forecasting/dailyTimeSeriesPlot.R")
source("forecasting/valueBoxesReturns.R")
source("forecasting/arimaModel.R")
source("forecasting/plots.R")
source("data/covid19Data.R")
source("map/baseMap.R")


### Shiny app ###
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar( collapsed = TRUE,
    sidebarMenu( id = "sideBarMenu",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("virus")),
      menuItem("Tracker", tabName = "tracker", icon = icon("map-marked-alt")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("laptop-code")),
      menuItem("About", tabName = "about", icon = icon("user-secret"))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      ### DASHBOARD PAGE START -------------------------------------------------###
      tabItem(tabName = "dashboard",
              fluidPage(
                includeCSS("styles.css"),
                fluidRow(
                  column(12, align="center",
                         box(id = "mainPanel", solidHeader = TRUE, width = 12, height = "auto",
                             box(solidHeader = TRUE, width = 4, h1(totalCases()), h3("TOTAL CASES"), 
                                 actionButton("btn_totalCases", "", icon = icon("globe-europe"))),
                             box(solidHeader = TRUE, width = 4, h1(totalRecovered()), h3("TOTAL RECOVERED"), 
                                 actionButton("btn_totalRecovered", "", icon = icon("band-aid"))),
                             box(solidHeader = TRUE, width = 4, h1(totalDeaths()), h3("TOTAL DEATHS"),
                                 actionButton("btn_totalDeaths", "", icon = icon("skull-crossbones"))),
                             br(),
                             tabBox( width = 10,
                                   id = "mainTabPanels",
                                   tabPanel("Map", leafletOutput("mymap", height = "450px")),
                                   tabPanel("Chart", plotOutput("mainTimeSeriesPlot", height = "450px"))
                                   ),
                             box(solidHeader = TRUE, width = 2, title = "Quick access",
                                 actionButton("btn_map", "", icon = icon("map")),
                                 actionButton("btn_plots", "", icon = icon("chart-line")),
                                 actionButton("btn_about", "",icon = icon("info"))),
                             )),
                         )
              )
      ),
      #### DASHBOARD ENDS ------------------------------------------------------###
      ### MAP PAGE STARTS ------------------------------------------------------###
      tabItem(tabName = "tracker",
                # Stats Display
                absolutePanel(id = "stats", class = "panel panel-default",
                              top = 70, left = "auto",right = 20, bottom = "auto" , width = "auto", fixed=TRUE, height = "auto", 
                              h3("Global situation: "),
                              h4(id = "map-info-new" ,sprintf("New cases (Last 7 days): %s", totalNewCases())),
                              h5(id = "map-info-confirmed" ,sprintf("Confirmed cases: %s", totalCases())),
                              h4(id = "map-info-new" ,sprintf("New deaths (Last 7 days): %s", totalNewDeaths())),
                              h5(id = "map-info-confirmed" ,sprintf("Confirmed deaths: %s", totalDeaths()))),
              # Controls Display
              absolutePanel(id = "controls", top = "auto", bottom = 55, right = "auto", height = "auto", width = "auto", fixed = TRUE,
                            h2(""),
                            fluidRow( id = "controls-buttons", 
                              actionButton("cases", "", icon = icon("globe-europe")),
                              actionButton("deaths", "", icon = icon("skull-crossbones")),
                              actionButton("recovered", "", icon = icon("band-aid"))
                              ###NEEDS UPDATING - MAKE THE COLOR CHANGE AFTER COLICKED ###
                            ))
      ),
    
      #### MAP ENDS ------------------------------------------------------------###
      ### FORECASTING PAGE STARTS ----------------------------------------------###
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
                           checkboxInput("checkBoxEMA", label = "Exponential Moving Average", value = FALSE),
                           p("Data source: API Gov Uk")
                         ),
                         mainPanel(
                           plotOutput("plotOutput",  height = "auto")
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
      
      ### ABOUT PAGE STARTS ----------------------------------------------------###
      tabItem(tabName = "about")
      
      ### ABOUT PAGE ENDS ------------------------------------------------------###
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  
  #Dash starts------------------------------------------------------------------
  
  # Main Buttons 
  observeEvent(input$btn_totalCases, {
    output$mainTimeSeriesPlot <- renderPlot({
      dailyCasesPlot()
    })
    casesMap("mymap")
  })
  
  observeEvent(input$btn_totalRecovered, {
    output$mainTimeSeriesPlot <- renderPlot({
      dailyRecoveredPlot()
    })
    recoveredMap("mymap")
   
  })
  
  observeEvent(input$btn_totalDeaths, {
    output$mainTimeSeriesPlot <- renderPlot({
      dailyDeathsPlot()
    }) 
    deathsMap("mymap")
  })
  
  # Main panel View  
  output$mymap <- renderLeaflet({ 
    baseMap()
  })
  output$mainTimeSeriesPlot <- renderPlot({
    dailyCasesPlot()
  }) 
  
  # Quick access buttons 
  observeEvent(input$btn_map, {
    updateTabItems(session, "sideBarMenu", "tracker")
  })
  observeEvent(input$btn_plots, {
    updateTabItems(session, "sideBarMenu", "forecasting")
  })
  observeEvent(input$btn_about, {
    updateTabItems(session, "sideBarMenu", "about")
  })
  
  # Dash Ends ------------------------------------------------------------------
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
