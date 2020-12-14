## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)

source("dailyTimeSeriesPlot.R")
source("mainPageValueBoxReturns.R")

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    sidebarMenu(
      ##Source for icons https://fontawesome.com/
      menuItem("Dashboard", tabName = "dashboard", icon = icon("virus")),
      menuItem("Summary", tabName = "summary", icon = icon("database")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("laptop-code")),
      menuItem("About", tabName = "about", icon = icon("user-secret"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      
      ### First tab content ###
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 8,
                       
                  #Dashboard page boxes - Start
                  h2("UK Statistics on Coronavirus", style="font-family:verdana;" ),
                  valueBoxOutput("Cases"),
                  valueBoxOutput("Deaths"),
                  valueBoxOutput("Tests")
                  
                )
              ),
              
              fluidRow(
                column(width = 12, 
                       box(
                         status = "warning", br(), height = "500px", width = 12,
                         sidebarPanel(
                           h3("Customise the plot: "), br(),
                           selectInput("plotData", choices = c("New cases", "Deaths"), label = "Select data: "),
                           selectInput("plotType", choices = c("Daily", "Commutative"), label = "Select plot type: "),
                           checkboxInput("checkBoxMA", label = "Moving Average", value = FALSE) ### Disable when commutative selected  TO DO ####
                         ),
                         mainPanel(
                           plotOutput("plotOutput")
                         ))
                       )
                )
      ),
      
      ### Second tab content ###
      tabItem(tabName = "summary",
              h2("Summary tab content")
      ),
    
      
      ### Third tab content ###
      tabItem(tabName = "forecasting",
              h2("Forecasting tab content")
      ),
      
      ### Forth tab content  ###
      tabItem(tabName = "about")
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  #Dashboard start -----------------------------------------------------------
  
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
        dailyCasesTimeSeriesPlot(input$checkBoxMA)
      }
      else if(input$plotType == "Commutative"){    
        dailyCasesLinear()
      }
    }
    else if(input$plotData == "Deaths"){
      
      if(input$plotType == "Daily"){
        dailyDeathsTimeSeriesPlot(input$checkBoxMA)
      }
      else if(input$plotType == "Commutative"){
        dailyCommutativeDeathsPlot()
      }
    }
  })
  
  #Dashboard End -----------------------------------------------------------
  
}

shinyApp(ui, server)
