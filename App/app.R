#install.packages("leaflet")
#install.packages("leaflet.extras")
## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
library(shinyWidgets)

### Sources ####
source("data/covid19Data.R")
source("forecasting/dailyTimeSeriesPlot.R")
source("forecasting/valueBoxesReturns.R")
source("forecasting/arimaModel.R")
source("functions/dashPlots.R")
source("functions/countryPlots.R")
source("functions/forecasting.R")
source("functions/pieChart.R")
source("functions/mapHighChart.R")

### Shiny app ###
ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar( collapsed = TRUE,
    sidebarMenu( id = "sideBarMenu",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("virus")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-line")),
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
                             box(solidHeader = TRUE, width = 4,
                                 column(1, algin = "left", dropdown(style = "jelly", icon = icon("virus"), width = "800px", color = "primary",
                                                    column(3,h3("Situation:"),h3(id ="info-label","New"), h3(id ="info-label","7-days"), h3(id ="info-label","7-days change")),
                                                    column(2,h3("Cases"),h3(id = "info_text",formatLargeNumber(todayCases())), h3(id = "info_text",formatLargeNumber(sum(newCasesWeekly()))), h3(paste(weeklyCasesChange(), "%"))),
                                                    column(2,h3("Recovered"), h3(id = "info_text",formatLargeNumber(todayRecovered())), h3(id = "info_text",formatLargeNumber(weeklyRecovered()))),
                                                    column(2,h3("Deaths"), h3(id = "info_text",formatLargeNumber(todayDeaths())),h3(id = "info_text",formatLargeNumber(sum(newDeathsWeekly())))),
                                                    column(2,h3("Active"), h3(id= "activecases_text",formatLargeNumber(todayActiveCases())), h3(id = "activecases_text",formatLargeNumber(weeklyActiveCases()))),
                                                    column(12, p(id ="info-label", "Please note this infomration may be incomplete and not 100% accuarate"))
                                                    
                                                    )),
                                 column(11, h1(formatLargeNumber(totalCases())), h3("TOTAL CASES"), 
                                 actionButton("btn_totalCases", "", icon = icon("head-side-mask")),
                                 h3(id = "increase",sprintf("Daily increase: %s%s", dailyChange(casesDataSet), "%")))),
                                 
                             box(solidHeader = TRUE, width = 4, h1(totalRecovered()), h3("TOTAL RECOVERED"), 
                                 actionButton("btn_totalRecovered", "", icon = icon("band-aid")),
                                 h3(id = "increase",sprintf("Daily increase: %s%s", dailyChange(deathsDataSet), "%"))),
                             
                             box(solidHeader = TRUE, width = 4, h1(totalDeaths()), h3("TOTAL DEATHS"),
                                 actionButton("btn_totalDeaths", "", icon = icon("skull-crossbones")),
                                 h3(id = "increase",sprintf("Daily increase: %s%s", dailyChange(recoveredDataSet), "%"))),
                             
                             br(),
                             tabBox( width = 10,
                                   id = "mainTabPanels",
                                   tabPanel("Map", highchartOutput("dashMap")),
                                   tabPanel("Chart", highchartOutput("mainTimeSeriesPlot")),
                                   tabPanel("Pie", highchartOutput("casesHighChart"))
                                   ),
                             box(solidHeader = TRUE, width = 2, title = "Quick access",
                                 actionButton("btn_plots", "", icon = icon("chart-line")),
                                 actionButton("btn_forecast", "", icon = icon("laptop-code")),
                                 actionButton("btn_about", "",icon = icon("info"))),
                             )),
                         )
              )
      ),
      #### DASHBOARD ENDS ------------------------------------------------------###
      #### Plots STARTS --------------------------------------------------------###
      tabItem(tabName = "plots",
              fluidPage(
                includeCSS("styles.css"),
                fluidRow(
                  column(12, align="center",
                         box(id = "mainPanel", solidHeader = TRUE, width = 12, height = "auto",
                             box(width = 8, solidHeader = TRUE, height = "auto",
                                column(2, align="left", actionBttn(inputId = "backDash", label = NULL, style = "material-circle",  color = "primary",icon = icon("arrow-left")),),
                                column(10, 
                                       column(3, h2("Cases"), h3(id = "info_text",textOutput("casesInCountry"))),
                                       column(3, h2("Recovered"), h3(id = "info_text",textOutput("recoveredInCountry"))),
                                       column(3 ,h2("Deaths"), h3(id = "info_text",textOutput("deathsInCountry"))),
                                       column(3, h2("Active"), h3(id= "activecases_text",textOutput("activeInCountry")))
                                       )
                                 ),
                             box(width = 4, solidHeader = TRUE, height = "auto",
                                 selectInput("country", 
                                             choices = retrunListOfCountries(), 
                                             label = "Select country: ")
                                 ),
                             box(width = 12, solidHeader = TRUE, height = "auto",
                                 mainPanel(
                                   highchartOutput("selectedCountryPlotDaily")
                                   ),
                                 sidebarPanel( align = "left", id = "sidebarControls",
                                               h2("Controls"),
                                               radioGroupButtons(
                                                 inputId = "switchGraphType",
                                                 label = "Choose graph :", 
                                                 choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line"),
                                                 justified = TRUE
                                               ),
                                               radioGroupButtons(
                                                 inputId = "switchData",
                                                 label = "Choose data :", 
                                                 choices = c(`<i class="fas fa-head-side-mask"></i>` = "cases",`<i class="fas fa-skull-crossbones"></i>`= "deaths"),
                                                 justified = TRUE
                                               ),
                                               h3("Exponential Moving Average"),
                                               switchInput(inputId = "movingAverage", value = FALSE),
                                               p("Forecast done using Feed-forward neural networks")
                                               ), 
                                 ),
                             )),
                  )
              )),
      #### Plots ENDS ----------------------------------------------------------###
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
      #leafletOutput("mymap", height = "450px")
      
      ### ABOUT PAGE ENDS ------------------------------------------------------###
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  
  #--------------------------------Dash starts----------------------------------
  #### Default on start ####
  #Plot
  output$mainTimeSeriesPlot <- renderHighchart({
    dailyCasesPlot()
  })
  # Pie chart
  output$casesHighChart <- renderHighchart({
    pieControler("cases")
  })
  # Map
  output$dashMap <- renderHighchart({
    hcmapSelector("cases")
  })
  #casesMap("mymap")
  
  # Main Buttons 
  observeEvent(input$btn_totalCases, {
    #Plot
    output$mainTimeSeriesPlot <- renderHighchart({
      dailyCasesPlot()
    })
    # Pie chart
    output$casesHighChart <- renderHighchart({
      pieControler("cases")
    })
    # Map
    output$dashMap <- renderHighchart({
      hcmapSelector("cases")
    })
    #casesMap("mymap")
  })
  
  observeEvent(input$btn_totalRecovered, {
    output$mainTimeSeriesPlot <- renderHighchart({
      dailyRecoveredPlot()
    })
    
    output$casesHighChart <- renderHighchart({
      pieControler("recovered")
    })
    
    #recoveredMap("mymap")
    output$dashMap <- renderHighchart({
      hcmapSelector("recovered")
    })
  })
  
  observeEvent(input$btn_totalDeaths, {
    output$mainTimeSeriesPlot <- renderHighchart({
      dailyDeathsPlot()
    }) 
    
    output$casesHighChart <- renderHighchart({
      pieControler("death")
    })
    
    #deathsMap("mymap")
    output$dashMap <- renderHighchart({
      hcmapSelector("deaths")
    })
  })
  
  # Quick access buttons 
  observeEvent(input$btn_plots, {
    updateTabItems(session, "sideBarMenu", "plots")
  })
  observeEvent(input$btn_forecast, {
    updateTabItems(session, "sideBarMenu", "forecasting")
  })
  observeEvent(input$btn_about, {
    updateTabItems(session, "sideBarMenu", "about")
  })
  
  # Dash Ends ------------------------------------------------------------------
  # Plots Start ----------------------------------------------------------------
  observeEvent(input$backDash, {
    updateTabItems(session, "sideBarMenu", "dashboard")
  })
  # Display country situation
  output$casesInCountry <- renderText({
    formatLargeNumber(returnSumCasesOfCountry(createTimeSeiresForCountry(input$country)))
  })
  output$recoveredInCountry <- renderText({
    formatLargeNumber(returnSumRecoveredOfCountry(input$country))
  })
  output$deathsInCountry <- renderText({
    formatLargeNumber(returnSumDeathsOfCountry(input$country))
  })
  output$activeInCountry <- renderText({
    formatLargeNumber(returnSumActiveCasesOfCountry(input$country))
    
  })
  
  # Interactive plots 
  output$selectedCountryPlotDaily <- renderHighchart({
    interactivePlotsMechanism(createTimeSeiresForCountry(input$country),input$switchGraphType, input$movingAverage)
  })
  
  # Controls for plots
  output$value1 <- renderPrint({ input$switchGraphType })
  output$value2 <- renderPrint({ input$switchData })
  # Plots Ends -----------------------------------------------------------------
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
