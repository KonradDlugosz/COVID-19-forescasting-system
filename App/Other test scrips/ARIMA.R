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

#Plot cleaned data
ggplot()+
  geom_line(data = dailyCases, aes(x = date, y = dailyCases$clean_count)) + 
  ylab("Date")

# Moving average 
dailyCases$cnt_ma <- ma(dailyCases$clean_count, order= 7)

# Plot moving average
ggplot()+
  geom_line(data = dailyCases, aes(x = date, y = dailyCases$clean_count, color = "Count")) +
  geom_line(data = dailyCases, aes(x = date, y = dailyCases$cnt_ma, color = "Weekly moving avaerage "))+
  ylab("Date")

#############################################################
#### DICOMPSITION OF THE DATA - Seasonality, trend, cycle ###
# Calculate seasonal 
count_ma = ts(na.omit(dailyCases$cnt_ma), frequency = 7)
count_ma
decomp = stl(count_ma, s.window = "periodic")
desesonal_cnt <- seasadj(decomp)
plot(decomp)

# Augmented Dickey test 
adf.test(count_ma, alternative = "stationary")


#############################################################
### Autocorrelation model ###
# Displays correlation between series and its lags
acf(count_ma, lag.max = 100)
# includes previous lags
pacf(count_ma, lag.max = 100)

# Brings data closer together 
count_d1 = diff(desesonal_cnt, differences = 1)
plot(count_d1)

adf.test(count_d1, alternative = "stationary")

# Look for spikes at specific lag points of difference series
acf(count_d1, main = "ACF for Differenced Series", lag.max = 100)
pacf(count_d1, main = "PACF for Diffrenced Series", lag.max = 100)


##############################################################
### ARIMA MODELS ###
auto.arima(desesonal_cnt, seasonal = FALSE)
fit <- auto.arima(desesonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 100, main = "(1,1,1) Model Residules") # Lags make the data inaccurate 

fit2 <- arima(desesonal_cnt, order = c(1,1,6))
tsdisplay(residuals(fit2), lag.max = 100, main = "Seasonal Model Residules")

# Forecast fit model
fcast <- forecast(fit2, h=30)
plot(fcast)

###############################################################
### Test model performance
hold <- window(ts(desesonal_cnt), start= 300)
fit_no_holdout = arima(ts(desesonal_cnt[-c(300:343)]), order= c(1,1,6))
fcast_no_holdout <- forecast(fit_no_holdout, h= 43)
plot(fcast_no_holdout, main = " ")
lines(ts(desesonal_cnt))

#Add seasonality 
fit_w_seasonality = auto.arima(desesonal_cnt, seasonal = TRUE)
seas_fcast <- forecast(fit_w_seasonality, h = 30)
plot(seas_fcast)

###############################################################
#Other code
fit2 <- arima(dailyCases$newCases, order = c(7L, 1L, 7L),)
fcast <- forecast(fit2, h =30)
plot(fcast)