source("httpRequest.R")
library(TTR)

# Data prep
dailyCases <- getDailyCasesData("dailyCases")
dailyCases <- dailyCases %>% 
  mutate(date = ymd(dailyCases$date), newCases = as.numeric(newCases)) %>%
  arrange(date)

k = 7

### SMA calculates the arithmetic mean of the series over the past n observations ###
# Calculate SMA
dailyCases$SMA <- TTR::SMA(dailyCases$newCases, n = k)

### EMA calculates an exponentially-weighted mean, giving more weight to recent observations. ###
# Calculate EMA 
dailyCases$EMA <- TTR::EMA(dailyCases$newCases, n = k)

# Plot MA for Daily cases
ggplot(dailyCases, aes(x = date, y = newCases))+
  geom_line(color="#FF6B33") +
  geom_area(fill="#FF8E64", alpha=0.9) +
  geom_line(data = dailyCases, aes(x = date, y = SMA), color = "blue", size = 1.2, alpha = 0.7) +
  geom_line(data = dailyCases, aes(x = date, y = EMA), color = "purple", size = 1.2, alpha = 0.7) +
  theme_ipsum()
  

# Error difference between Cases and SMA prediction
dailyCases$Residuals <- dailyCases$newCases - dailyCases$SMA
dailyCases$ResidualsS <- (dailyCases$newCases - dailyCases$SMA)^2

# Other calculations 
a <- k+1 
b <- nrow(dailyCases)
MSE <- mean(dailyCases$ResidualsS[a:b])
MAE <- mean(abs(dailyCases$Residuals[a:b]))
MAPE <- mean(abs(dailyCases$Residuals[a:b]/dailyCases$newCases[a:b]))            



### other technique ###

library(smooth)
library(Mcomp)
library(xts)

# convert data to time series
data_ts <- xts(dailyCases$newCases, dailyCases$date)

sma(M3$N2457$x, h=18, silent=FALSE)
sma(M3$N2568$x, h=18, interval=TRUE)

