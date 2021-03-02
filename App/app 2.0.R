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
library(forecast)
library(highcharter)
library(shinythemes)

### Sources ####
source("data/covid19Data.R")
source("data/dataDisplay.R")
source("functions/dashboard.R")
source("functions/forecast.R")
source("functions/forecastFunction.R")
source("functions/pieChart.R")
source("functions/mapHighChart.R")

### Shiny app ###
ui <- navbarPage("COVID-19", id = "navbarMenu", position = "fixed-top", theme =  shinytheme("yeti"),
                 
                 #####--------------------------------Dashboard TAB---------------------------------
                 tabPanel("Dashboard",
                          fluidPage(
                            # Fixed menu and body overlap 
                            tags$style(type="text/css", "body {padding-top: 70px;}"),
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
                                             column(1, dropdown(style = "jelly", icon = icon("question"), color = "primary", right = TRUE, width = "800px", align = "left",
                                                                h3(id = "cases_text","Total Cases"),
                                                                h4(id ="info-label","Total number of cases globaly obsereved."),
                                                                h3(id = "recovered_text","Total Recovered"),
                                                                h4(id ="info-label", "Total number of infected people that have recovered from COVID-19."),
                                                                h3(id = "deaths_text","Total Deaths"),
                                                                h4(id ="info-label", "Total number of deaths that occured due to COVID-19."),
                                                                h3(id = "activecases_text","Active"),
                                                                h4(id ="info-label", "Number of cases which are still active since infected, incubation period (~14 days)"),
                                                                p(id ="info-label", "Please note that this data might not be 100% accurate due to mistakes like system errors or human error.")))
                                         ),
                                         br(),
                                         box(solidHeader = TRUE, width = 8, height = "800px",
                                             shinycssloaders::withSpinner(highchartOutput("dashMap", height = "650px")),
                                             column(6,
                                                    h2("Map controls"),
                                                    selectInput("mapControl", 
                                                                choices = c("Obsereved" ,"1M/Population"), 
                                                                label = "Alter data: "),
                                             ),
                                             column(6,
                                                    h2("Plot controls"),
                                                    radioGroupButtons(inputId = "switchGraphTypeDashPlot",label = "Choose graph :", 
                                                                      choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line"),
                                                                      justified = TRUE,size = "normal", 
                                                    ),
                                             )),
                                         box(solidHeader = TRUE, width = 4, shinycssloaders::withSpinner(highchartOutput("casesHighChart"))),
                                         box(solidHeader = TRUE, width = 4,title = "Global situation", shinycssloaders::withSpinner(highchartOutput("mainTimeSeriesPlot")),
                                             ),
                                     )),
                            )
                          )),
                 #####--------------------------------Forecast TAB---------------------------------
                 tabPanel("Forecast",
                          fluidPage(
                            includeCSS("styles.css"),
                            fluidRow(
                              column(12, align="center",
                                     box(id = "mainPanel", solidHeader = TRUE, width = 12, height = "auto",
                                         box(width = 4,  solidHeader = TRUE, align = "center", height = "160px",
                                             h1(textOutput("selectedCountryDisplay")),
                                             h2(textOutput("population"))
                                             ),
                                         box(width = 8, solidHeader = TRUE, height = "160px",
                                             column(3, h2("Cases"), h3(id = "cases_text",textOutput("casesInCountry")), h4(id ="info-label",textOutput("percentageCases"))),
                                             column(3, h2("Recovered"), h3(id = "recovered_text",textOutput("recoveredInCountry")),h4(id ="info-label",textOutput("percentageRecovered"))),
                                             column(3 ,h2("Deaths"), h3(id = "deaths_text",textOutput("deathsInCountry")),h4(id ="info-label",textOutput("percentageDeaths"))),
                                             column(3, h2("Active"), h3(id= "activecases_text",textOutput("activeInCountry")),h4(id ="info-label",textOutput("percentageActive")))
                                         ),
                                         box(width = 12, solidHeader = TRUE, height = "auto",
                                             
                                             box(width = 8, solidHeader = TRUE, height = "auto",
                                                 h3(textOutput("countryPlotTitle")),
                                                 shinycssloaders::withSpinner(highchartOutput("selectedCountryPlotDaily"))
                                                 ),
                                             tabBox(width = 4, height = "auto", 
                                               tabPanel("Plot", id = "plotControlsPanel",
                                                        h3("Plot controls"),
                                                        selectInput("country", 
                                                                    choices = retrunListOfCountries(), 
                                                                    label = "Select country: "),
                                                        radioGroupButtons(inputId = "switchGraphType",label = "Choose graph :", 
                                                                          choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line"),
                                                                          justified = TRUE,size = "normal"
                                                                          ),
                                                        radioGroupButtons(inputId = "switchData",label = "Choose data :", 
                                                                          choices = c(`<i class="fas fa-head-side-mask"></i>` = "cases",`<i class="fas fa-skull-crossbones"></i>`= "deaths"),
                                                                          justified = TRUE,size = "normal"
                                                                          ),
                                                        h4("Moving Average: "),
                                                        switchInput(inputId = "movingAverage", value = FALSE),
                                                        dropdown(style = "jelly", icon = icon("question"), color = "primary",
                                                                 p(id ="info-label","Use the controls provided to alter the plot. The controls allow to toggle between cases and deaths of a country."))
                                                        
                                                        ),
                                               tabPanel("Forecast", 
                                                        h3("Forecast controls"),
                                                        selectInput("model", 
                                                                    choices = c("NNETAR", "ARIMA", "ETS", "BATS"), 
                                                                    label = "Select model: "),
                                                        h4("Select days to forecast:"),
                                                        knobInput(inputId = "daysToForecast", label = " ",
                                                                  value = 14, min = 2, max = 30, displayPrevious = TRUE,
                                                                  fgColor = "#428BCA",inputColor = "#428BCA"
                                                                  )
                                                        )
                                             ),
                                         ),
                                         
                                         box(width = 12, solidHeader = TRUE, height = "auto",
                                             h3("Model Accuracy"),
                                             tableOutput("accuracyTable"),
                                             actionBttn("RMSEInfo", " RMSE Info", style = "stretch", color = "primary"),
                                             actionBttn("MAPEInfo", " MAPE Info", style = "stretch", color = "primary"),
                                             br()
                                         ),
                                         box(width = 12, solidHeader = TRUE, height = "auto",
                                             h3("View of Data Decomposition"),
                                             shinycssloaders::withSpinner(highchartOutput("sesonality"))
                                         ),
                                         #box(width = 4, solidHeader = TRUE, height = "auto",
                                         #    h3("What is decomposition of time series ?"),
                                         #    p(id ="info-label","Deconstruction of time series into several components, each representing one of the underlying categories of patterns"),
                                         #    h4(id = "data_component","The data "),
                                         #    p(id ="info-label","Data containing all the layers"),
                                         #    h4(id = "seasonal_component","The seasonal component"),
                                         #    p(id ="info-label","A seasonal pattern exists when a time series is influenced by seasonal factors."),
                                         #    h4(id = "trend_component","The trend component"),
                                         #    p(id ="info-label","Reflects the long-term progression of the series. A trend exists when there is apersistent increasing or decreasing direction in the data."),
                                         #    h4(id = "remainder_component","The remainder component"),
                                         #    p(id ="info-label","It represents the residuals or remainder of the time series after the other components have been removed.")
                                         #    )
                                     
                                         )
                                     ),
                              )
                            )
                          ),
                 #####--------------------------------Explore Data TAB---------------------------------
                 tabPanel("Explore Data",
                          fluidPage(
                            fluidRow(
                              column(12,
                                     box(solidHeader = TRUE, width = 12,
                                         h2("Explore data"),
                                         br(),
                                         dataTableOutput("dataDisplay")
                                         )
                                     )
                            )
                          ))
)

server <- function(input, output, session) {
  set.seed(122)
  
  #--------------------------------Dash starts----------------------------------
  #### Default on start ####
  #Plot
  output$mainTimeSeriesPlot <- renderHighchart({
    selectDashPlot("cases", input$switchGraphTypeDashPlot)
  })
  # Pie chart
  output$casesHighChart <- renderHighchart({
    pieControler("cases")
  })
  
  # Map
  output$dashMap <- renderHighchart({
    hcmapSelector("cases")
  })
  
  # Main Buttons 
  observeEvent(input$btn_totalCases, {
    #Plot
    output$mainTimeSeriesPlot <- renderHighchart({
      selectDashPlot("cases", input$switchGraphTypeDashPlot)
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
      selectDashPlot("recovered", input$switchGraphTypeDashPlot)
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
      selectDashPlot("deaths",input$switchGraphTypeDashPlot)
    }) 
    
    output$casesHighChart <- renderHighchart({
      pieControler("death")
    })
    
    output$dashMap <- renderHighchart({
      hcmapSelector("deaths")
    })
  })
  
  observeEvent(input$btn_totalActive, {
    output$mainTimeSeriesPlot <- renderHighchart({
      activePlot()
    }) 
    output$casesHighChart <- renderHighchart({
      pieControler("active")
    })
    output$dashMap <- renderHighchart({
      hcmapSelector("active")
    })
  })
  
  # Data Display
  output$dataDisplay <- renderDataTable({
    df
  })

  # Dash Ends ------------------------------------------------------------------
  # Plots Start ----------------------------------------------------------------
  # Display country situation numbers
  output$casesInCountry <- renderText({
    formatLargeNumber(returnSumCasesOfCountry(createTimeSeiresForCountry(input$country, "cases"),input$country))
  })
  output$recoveredInCountry <- renderText({
    formatLargeNumber(returnSumRecoveredOfCountry(input$country))
  })
  output$deathsInCountry <- renderText({
    formatLargeNumber(returnSumDeathsOfCountry(input$country))
  })
  output$activeInCountry <- renderText({
    formatLargeNumber(returnActiveCases(input$country))
    
  })
  
  # Display percentage of population
  output$percentageCases <- renderText({
    paste( returnPercentageOfPopulation(returnSumCasesOfCountry(createTimeSeiresForCountry(input$country, "cases"),input$country),input$country), "%", "of population" )
  })
  output$percentageRecovered <- renderText({
    paste( returnPercentageOfPopulation(returnSumRecoveredOfCountry(input$country),input$country), "%", "of population" )
  })
  output$percentageDeaths <- renderText({
    paste( returnPercentageOfPopulation(returnSumDeathsOfCountry(input$country),input$country), "%", "of population" )
  })
  output$percentageActive <- renderText({
    paste( returnPercentageOfPopulation(returnActiveCases(input$country),input$country), "%", "of population" )
  })
  
  # Interactive plots 
  output$selectedCountryPlotDaily <- renderHighchart({
    interactivePlotsMechanism(createTimeSeiresForCountry(input$country, input$switchData),
                              input$switchGraphType, input$movingAverage,input$daysToForecast, input$switchData, input$model)
  })
  # Controls for plots
  output$value1 <- renderPrint({ input$switchGraphType })
  output$value2 <- renderPrint({ input$switchData })
  # Accuracy for forecast
  output$accuracyTable <- renderTable({ 
    accurcyTable(createTimeSeiresForCountry(input$country,input$switchData),input$daysToForecast) 
  })
  
  # Display Country selected
  output$selectedCountryDisplay <- renderText({
    input$country
  })
  # Display population of selected country
  output$population <- renderText({
    paste("Population: ",formatLargeNumber(returnPopulationOfSelctedCountry(input$country)))
  })
  
  useSweetAlert()
  # Display info of RMSE
  observeEvent(input$RMSEInfo, {
    sendSweetAlert(
      session = session,
      title = "What is Root Mean Square Error?",
      text = "This value shows on average, how many values the forecasat was away from actual.",
      type = "info"
    )
  })
  observeEvent(input$MAPEInfo, {
    sendSweetAlert(
      session = session,
      title = "What is Mean Absolute Forecast Error?",
      text = "It signifies % of average deviation of the forecast from the actual value in the given model.",
      type = "info"
    )
  })
  
  # Display title for plot
  output$countryPlotTitle <- renderText({
    returnTitleOfCountryPlots(input$switchGraphType,input$switchData)
  })
  
  # Display seasonality of selected country
  output$sesonality <- renderHighchart({
    decomposeDataOfSelectedCountry(createTimeSeiresForCountry(input$country,input$switchData))
  })
  
  # Plots Ends -----------------------------------------------------------------
  
  output$testData <- renderTable({
    casesDataSet
  })
  
}

shinyApp(ui, server)
