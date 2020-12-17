ARIMA <- function(){
  
  source("httpRequest.R")
  library(forecast)
  library(TTR)
  library(tseries)

  # Data prep
  dailyCases <- getDailyCasesData("dailyCases")
  dailyCases <- dailyCases %>% 
    mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases)) %>%
    arrange(date)

  # Time series object on new cases 
  count_TSObject = ts(dailyCases$newCases)
  # Clean, replace outlines and missing values if exist 
  dailyCases$clean_count = tsclean(count_TSObject)
  # Moving average 
  dailyCases$cnt_ma <- ma(dailyCases$clean_count, order= 7)
  
  #### DICOMPSITION OF THE DATA - Seasonality, trend, cycle ###
  # Calculate seasonal 
    count_ma = ts(na.omit(dailyCases$cnt_ma), frequency = 7)
  decomp = stl(count_ma, s.window = "periodic")
  desesonal_cnt <- seasadj(decomp)
  
  # Brings data closer together 
  count_d1 = diff(desesonal_cnt, differences = 1)
  plot(count_d1)
  
  fit2 <- arima(desesonal_cnt, order = c(1,1,6))
  ggtsdisplay(residuals(fit2))
  # Forecast fit model
  fcast <- forecast(fit2, h=30)
  fcastPlot <- autoplot(fcast) + ggtitle("Forecast from ARIMA model predicting new cases in 30 days")
  
  return(fcastPlot)

}
