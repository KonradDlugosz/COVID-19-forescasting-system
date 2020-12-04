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
      
      # First tab content--------------------------
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
                  tabBox(
                    title = "Time series plot for Cases",
                    side = "right", height = "350px", width = 12, 
                    selected = "Daily",
                    tabPanel("Daily", 
                             plotOutput("dailyCasesPlot")),
                    tabPanel("Linear", 
                             plotOutput("dailyCasesLinearPlot"))
                    #tabPanel("Deaths", 
                         #    plotOutput("dailyDeathsPlot"))
                  )
                )
                
              )
      ),
      
      
      # Second tab content-----------------------
      tabItem(tabName = "summary",
              h2("Summary tab content")
      ),
      
      # Third tab content------------------------
      tabItem(tabName = "forecasting",
              h2("Forecasting tab content")
      ),
      
      # Forth tab content------------------------
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

  
  
  output$dailyCasesPlot <- renderPlot({
    dailyCasesTimeSeriesPlot()
  })
  
  output$dailyCasesLinearPlot <- renderPlot({
    dailyCasesLinear()
  })
  
  output$dailyDeathsPlot <- renderPlot({
    dailyDeathsTimeSeriesPlot()
    
  })
  
  #Dashboard End -----------------------------------------------------------
  
}

shinyApp(ui, server)
