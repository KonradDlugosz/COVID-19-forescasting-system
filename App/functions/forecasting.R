# This file contains forecasting methods
source("functions/plots.R")

#### Neural Network forward feed time series forecast function ####
createNuralNetworkTSForecast <- function(countrySelected){
  # 1. Load and format data
  data <- countrySelected
  df <- ts(data$daily)
  
  # 2. Train model
  numDaysToForecast <- 14
  fit <- nnetar(df, repeats = 20)
  fcast <- forecast(fit, h = numDaysToForecast )
  #plot(fcast)
  
  # 3. Create dataframe for forcasted values
  newDates <- data$betterDates[nrow(data)] + 1
  for(i in 1:numDaysToForecast){
    newDates[i] <- data$betterDates[nrow(data)] + i
  }
  forcastedCases <- as.integer(fcast$mean)
  dfForecastedCases <- data.frame(newDates, forcastedCases)
  
  # 4. Make custom plot
  plot <-  ggplot(NULL) +
    xlab("Date")+
    ylab("New Cases")+
    geom_line(data = data, aes(x=betterDates, y=daily,color="#ff6600"), size = 1.2, alpha = 0.9 ) +
    geom_area(data = data, aes(x=betterDates, y=daily),fill="#ffa366", alpha=0.9) +
    geom_line(data = dfForecastedCases,aes(x=newDates, y=forcastedCases, color="#0099ff") ,size = 1.2, alpha = 0.9 ) + 
    geom_area(data = dfForecastedCases,aes(x=newDates, y=forcastedCases),fill="#66c2ff", alpha=0.9) + 
    scale_color_identity(name = "Legend",
                         breaks = c("#ff6600","#0099ff"),
                         labels = c("Obsereved", "Forecasted"),
                         guide = "legend")+
    theme_minimal(base_size = 18)

  return(plot)
}

#### Other functions ####
# Normalize function 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# Normalize data
#data$daily[1] <- 1 # add missing value, gives error without it.
#data$daily_norm <- normalize(data$daily)

