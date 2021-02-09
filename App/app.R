#install.packages("leaflet")
#install.packages("leaflet.extras")
#install.packages("shinyjs")
#install.packages("shinyalert")
#shinycssloaders
## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(shinyalert)

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
                             box(solidHeader = TRUE, width = 3,h1(formatLargeNumber(totalCases())), h3("TOTAL CASES"), 
                                 actionButton("btn_totalCases", "", icon = icon("head-side-mask")),
                                 h3(id = "increase",sprintf("Daily change: %s%s", dailyChange(casesDataSet), "%"), 
                                    actionButton(changeIconID(dailyChange(casesDataSet),FALSE),"",icon = icon(changeIcon(dailyChange(casesDataSet)))))),
                                 
                             box(solidHeader = TRUE, width = 3, h1(totalRecovered()), h3("TOTAL RECOVERED"), 
                                 actionButton("btn_totalRecovered", "", icon = icon("band-aid")),
                                 h3(id = "increase",sprintf("Daily change: %s%s", dailyChange(recoveredDataSet), "%"), 
                                    actionButton(changeIconID(dailyChange(recoveredDataSet),TRUE),"",icon = icon(changeIcon(dailyChange(recoveredDataSet)))))),
                             
                             box(solidHeader = TRUE, width = 3, h1(totalDeaths()), h3("TOTAL DEATHS"),
                                 actionButton("btn_totalDeaths", "", icon = icon("skull-crossbones")),
                                 h3(id = "increase",sprintf("Daily change: %s%s", dailyChange(deathsDataSet), "%"), 
                                    actionButton(changeIconID(dailyChange(deathsDataSet),FALSE),"",icon = icon(changeIcon(dailyChange(deathsDataSet)))))),
                          
                             box(solidHeader = TRUE, width = 3, 
                                 column(11,
                                        h1(formatLargeNumber(totalActiveCases())), h3("GLOBAL ACTIVE"),
                                        actionButton("btn_totalActive", "", icon = icon("globe-europe")),
                                        h3(id = "increase" ,sprintf("Daily change: %s%s", dailyChangeActiveCases(), "%"), 
                                           actionButton(changeIconID(dailyChangeActiveCases(),FALSE),"",icon = icon(changeIcon(dailyChangeActiveCases()))))),
                                 column(1, dropdown(style = "jelly", icon = icon("question"), color = "primary", right = TRUE, width = "600px",
                                                    h3("Total Cases"),
                                                    p(id ="info-label","This shows total number of cases globaly obsereved."),
                                                    h3("Total Recovered"),
                                                    p(id ="info-label", "This shows total number of infected people that have recovered from COVID."),
                                                    h3("Total Deaths"),
                                                    p(id ="info-label", "This shows total number of deaths that occured due to COVID."),
                                                    h3("Active"),
                                                    p(id ="info-label", "This number shows how many cases are still active since incubation period (~14 days)")))
                                 ),
                             br(),
                             box(solidHeader = TRUE, width = 8, shinycssloaders::withSpinner(highchartOutput("dashMap"))),
                             box(solidHeader = TRUE, width = 4, shinycssloaders::withSpinner(highchartOutput("casesHighChart"))),
                             box(solidHeader = TRUE, width = 10,title = "Global Daily Situation Plot", shinycssloaders::withSpinner(highchartOutput("mainTimeSeriesPlot"))),
                             box(solidHeader = TRUE, width = 2,
                                 dropdown(inputId ="btn_virus", style = "simple",icon = icon("virus"), width = "800px", color = "primary", right = TRUE,
                                          column(3,h3("Situation:"),h3(id ="info-label","New"), h3(id ="info-label","7-days"), h3(id ="info-label","7-days change")),
                                          column(2,h3("Cases"),h3(id = "info_text",formatLargeNumber(todayCases())), h3(id = "info_text",formatLargeNumber(sum(newCasesWeekly()))), h3(paste(weeklyCasesChange(), "%"))),
                                          column(2,h3("Recovered"), h3(id = "info_text",formatLargeNumber(todayRecovered())), h3(id = "info_text",formatLargeNumber(weeklyRecovered()))),
                                          column(2,h3("Deaths"), h3(id = "info_text",formatLargeNumber(todayDeaths())),h3(id = "info_text",formatLargeNumber(sum(newDeathsWeekly())))),
                                          column(2,h3("Active"), h3(id= "activecases_text",formatLargeNumber(todayActiveCases())), h3(id = "activecases_text",formatLargeNumber(weeklyActiveCases()))),
                                          column(12, p(id ="info-label", "Please note this infomration may be incomplete and not 100% accuarate"))),
                                 h4("Situation"),
                                 actionButton("btn_plots", "", icon = icon("chart-line")),
                                 h4("Country plots"),
                             )
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
                             box(width = 4,  solidHeader = TRUE, align = "center", height = "160px",
                                 column(2, align="left", actionBttn(inputId = "backDash", label = NULL, style = "material-circle",  color = "primary",icon = icon("arrow-left"))),
                                 column(10,h2(textOutput("selectedCountryDisplay")),
                                        h3(textOutput("population")))),
                             box(width = 8, solidHeader = TRUE, height = "160px",
                                 column(3, h2("Cases"), h3(id = "info_text",textOutput("casesInCountry"))),
                                 column(3, h2("Recovered"), h3(id = "info_text",textOutput("recoveredInCountry"))),
                                 column(3 ,h2("Deaths"), h3(id = "info_text",textOutput("deathsInCountry"))),
                                 column(3, h2("Active"), h3(id= "activecases_text",textOutput("activeInCountry")))
                                 ),
                             box(width = 12, solidHeader = TRUE, height = "auto",
                                 mainPanel(
                                   h3(textOutput("countryPlotTitle")),
                                   shinycssloaders::withSpinner(highchartOutput("selectedCountryPlotDaily")),
                                   column(7, align = "right", h4(textOutput("MFE"))),
                                   column(5, align = "left",dropdown(style = "jelly", icon = icon("question"), color = "primary",
                                                      p(id ="info-label","This value shows on average, how many values the forecasat was away from actual.")))
                                   ),
                                 sidebarPanel( align = "center", id = "sidebarControls",
                                               selectInput("country", 
                                                           choices = retrunListOfCountries(), 
                                                           label = "Select country: "),
                                               radioGroupButtons(
                                                 inputId = "switchGraphType",
                                                 label = "Choose graph :", 
                                                 choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line"),
                                                 justified = TRUE,
                                                 size = "lg"
                                               ),
                                               radioGroupButtons(
                                                 inputId = "switchData",
                                                 label = "Choose data :", 
                                                 choices = c(`<i class="fas fa-head-side-mask"></i>` = "cases",`<i class="fas fa-skull-crossbones"></i>`= "deaths"),
                                                 justified = TRUE,
                                                 size = "lg"
                                               ),
                                               column(8 ,h3("Exponential Moving Average: ")),
                                               column(4 ,br(),switchInput(inputId = "movingAverage", value = FALSE)),
                                               knobInput(
                                                 inputId = "daysToForecast",
                                                 label = "Days to forecast:",
                                                 value = 14,
                                                 min = 0,
                                                 max = 30,
                                                 displayPrevious = TRUE,
                                                 fgColor = "#428BCA",
                                                 inputColor = "#428BCA"
                                               ),
                                               dropdown(style = "jelly", icon = icon("question"), color = "primary", align = "right",
                                                        p(id ="info-label","Forecast done using Feed-forward neural networks."),
                                                        p(id ="info-label", "Plases note, more days to forecast becomes less accurate."))
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
      
      ### ABOUT PAGE ENDS ------------------------------------------------------###
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  
  #--------------------------------Dash starts----------------------------------
  #### Default on start ####
  # Map
  output$dashMap <- renderHighchart({
    hcmapSelector("cases")
  })
  #Plot
  output$mainTimeSeriesPlot <- renderHighchart({
    dailyCasesPlot()
  })
  # Pie chart
  output$casesHighChart <- renderHighchart({
    pieControler("cases")
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
    
  })
  
  observeEvent(input$btn_totalRecovered, {
    output$mainTimeSeriesPlot <- renderHighchart({
      dailyRecoveredPlot()
    })
    
    output$casesHighChart <- renderHighchart({
      pieControler("recovered")
    })
    
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
    
    output$dashMap <- renderHighchart({
      hcmapSelector("deaths")
    })
  })
  
  observeEvent(input$btn_totalActive, {
    output$dashMap <- renderHighchart({
      hcmapSelector("active")
    })
    output$casesHighChart <- renderHighchart({
      pieControler("active")
    })
    output$mainTimeSeriesPlot <- renderHighchart({
      dailyActivePlot()
    }) 
  })
  # Quick access buttons 
  observeEvent(input$btn_plots, {
    updateTabItems(session, "sideBarMenu", "plots")
  })

  # Dash Ends ------------------------------------------------------------------
  # Plots Start ----------------------------------------------------------------
  observeEvent(input$backDash, {
    updateTabItems(session, "sideBarMenu", "dashboard")
  })
  # Display country situation
  output$casesInCountry <- renderText({
    paste(formatLargeNumber(returnSumCasesOfCountry(createTimeSeiresForCountry(input$country, "cases"))), "(",
          returnPercentageOfPopulation(returnSumCasesOfCountry(createTimeSeiresForCountry(input$country, "cases")),input$country), "%", ")" )
  })
  output$recoveredInCountry <- renderText({
    paste(formatLargeNumber(returnSumRecoveredOfCountry(input$country)), "(", 
          returnPercentageOfPopulation(returnSumRecoveredOfCountry(input$country),input$country), "%", ")")
  })
  output$deathsInCountry <- renderText({
    paste(formatLargeNumber(returnSumDeathsOfCountry(input$country)), "(",
          returnPercentageOfPopulation(returnSumDeathsOfCountry(input$country),input$country), "%", ")")
  })
  output$activeInCountry <- renderText({
    paste(formatLargeNumber(returnActiveCases(input$country)), "(",
          returnPercentageOfPopulation(returnActiveCases(input$country),input$country), "%", ")" )
    
  })
  
  # Interactive plots 
  output$selectedCountryPlotDaily <- renderHighchart({
    interactivePlotsMechanism(createTimeSeiresForCountry(input$country, input$switchData),
                              input$switchGraphType, input$movingAverage,input$daysToForecast, input$switchData)
  })
  # Controls for plots
  output$value1 <- renderPrint({ input$switchGraphType })
  output$value2 <- renderPrint({ input$switchData })
  # Accuracy for forecast
  output$MFE <- renderText({ 
    paste("Root Mean Squared Error (RMSE):",accurcyOfForecast(createTimeSeiresForCountry(input$country,input$switchData),input$daysToForecast)) 
    })
  
  # Display Country selected
  output$selectedCountryDisplay <- renderText({
    input$country
  })
  # Display population of selected country
  output$population <- renderText({
    paste("Population: ",formatLargeNumber(returnPopulationOfSelctedCountry(input$country)))
  })
  
  # Display title for plot
  output$countryPlotTitle <- renderText({
    returnTitleOfCountryPlots(input$switchGraphType,input$switchData)
  })
  
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
