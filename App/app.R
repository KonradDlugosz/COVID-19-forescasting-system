## app.R ##
library(shinydashboard)
library(shiny)

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
                  h1('Welcome to COVID - 19 prediction system', style = "color:#0059b3", align = "center" ) ,
                  h2("Dashboard tab content")
              ),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
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
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)