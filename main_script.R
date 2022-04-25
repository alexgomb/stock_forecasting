#loading the libraries
library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
#loading the data
stock_data <- tibble(read_csv("PHM_data.csv", col_names = TRUE))
#tidy the data
stock_data$Date = as.Date(stock_data$Date, '%Y-%m-%d', format = "%m")
stock_data$Close = as.numeric(stock_data$Close)
#plot the plose pric
ggplot(stock_data, aes(x = Date, y = Close)) + geom_line() + scale_x_date("Month") + ylab("Close price") +
  xlab("")
#Calculate the weekly and monthly moving average of close price
stock_data$close_week_ma = ma(stock_data$Close, order = 7)
stock_data$close_month_ma = ma(stock_data$Close, order = 30)
#plot the Weekly and monthly MA
ggplot() + 
  geom_line(data = stock_data, aes(x = Date, y = Close, colour = "Close price")) + 
  geom_line(data = stock_data, aes(x = Date, y = close_month_ma, colour = "Monthly Moving average")) +
  geom_line(data = stock_data, aes(x = Date, y = close_week_ma, colour = "Weekly Moving average"))
#analyze seasonality
ts_ma = ts(na.remove(stock_data$close_week_ma), frequency= 30)
decomp = stl(ts_ma, s.window="periodic")
deseasonal_close <- seasadj(decomp)
plot(decomp)
#check if the time serie is stationary
adf.test(na.remove(close_week_ma, alternative = "stationary")) #p-value 0.366
#H0 = no stationary
#H1 = stationary
#p-value > 0.05 
#ARIMA model requires stationary time series
close_d1 = diff(deseasonal_close, differences = 1) #mutate time series to stationary
plot(close_d1)
#check if the adjusted time series is stationary
adf.test(na.remove(close_d1, alternative = "stationary")) #p-value 0.01
#find the best ARIMA  model with auto.arima
autoarima1 <- auto.arima(deseasonal_close, seasonal = FALSE)
#forecast and plot
forecast1 <- forecast(autoarima1, h =30)
plot(forecast1)