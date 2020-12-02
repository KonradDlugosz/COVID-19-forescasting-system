## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)

source("dailyCasesTimeSeriesPlot.R")

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Predictions"),
  dashboardSidebar(
    sidebarMenu(
      ##Source for icons https://fontawesome.com/
      menuItem("Dashboard", tabName = "dashboard", icon = icon("virus")),
      menuItem("Summary", tabName = "summary", icon = icon("database")),
      menuItem("Predictions", tabName = "predictions", icon = icon("laptop-code"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      
      # First tab content--------------------------
      tabItem(tabName = "dashboard",
              div(id = 'main_message',
                  #(src = "covid img.png", style="width:100%"),
                  h1('Welcome to COVID - 19 forecasting system', align = "center", style="font-family:verdana;" ),
                  p("In this system, I present forecasting algorithms for COVID-19 virus which can predict possible outcomes.",style="font-family:verdana;", align = "center")
                  
              ),
              fluidRow(
                column(width = 6, 
                  box(
                    title = "Daily Cases in UK", width = NULL, solidHeader = TRUE, status = "primary",
                    plotOutput("dailyCasesPlot")
                  ),
                )
                
              )
      ),
      
      
      # Second tab content-----------------------
      tabItem(tabName = "summary",
              h2("Summary tab content")
      ),
      
      # Third tab content------------------------
      tabItem(tabName = "predictions",
              h2("Predictions tab content")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  
  output$dailyCasesPlot <- renderPlot({
    dailyCasesTimeSeriesPlot()
  })
  
}

shinyApp(ui, server)
