## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)

### Sources ####
source("dailyTimeSeriesPlot.R")
source("mainPageValueBoxReturns.R")
source("arimaModel.R")

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
              h2("Dashboard tab content")
      ),
      
      ### Second tab content ###
      tabItem(tabName = "tracker",
              h2("Summary tab content"),
              #leafletOutput("map1")
      ),
    
      
      ### Third tab content ###
      tabItem(tabName = "forecasting",
              h2("Forecasting content"),
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
                         mainPanel(
                           plotOutput("arimaOutput",  height = "500px")
                         ))
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
  
  # Tracker starts -------------------------------------------------------------
 
  # Tracker ends ---------------------------------------------------------------
  #Forecasting start -----------------------------------------------------------
  
  output$Cases <- renderValueBox({
    
    valueBox(
      totalDailyCasesUK(), "Total Cases", icon = icon("briefcase-medical"),
      color = "orange"
    )
  })
  
  output$Deaths <- renderValueBox({
    valueBox(
      totalDailyDeathsUK(), "Total deaths within 28 days of possitve test", icon = icon("skull-crossbones"),
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
  
  output$arimaOutput <- renderPlot({
    ARIMA()
    })
  
  #Forecasting End -------------------------------------------------------------
  
}

shinyApp(ui, server)
