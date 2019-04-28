library(ggplot2)
library(forecast)
library(tseries)

daily_data <- read.csv('R/scratch/day.csv', header = T, stringsAsFactors = F)

daily_data$Date <- as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month') + ylab("Daily Bike Checkouts") +
  xlab("")

# make time series object from counts of bicycles per day
count_ts <- ts(daily_data[, c('cnt')])

# clean outliers with tsclean
daily_data$clean_cnt <- tsclean(count_ts)

ggplot() +
  geom_line(data = daily_data, aes(x=Date, y=clean_cnt)) + ylab("Cleaned Bicycle Count")

# weekly moving average
daily_data$cnt_ma <- ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers

#monthly moving average
daily_data$cnt_ma30 <- ma(daily_data$clean_cnt, order=30)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma, colour = "Weekly Moving Average")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average")) +
  ylab('Bicycle Count')

# calculate seasonal component (30 obs. per month)
count_ma <- ts(na.omit(daily_data$cnt_ma), frequency = 30)
decomp <- stl(count_ma, s.window = "periodic")
# subtract seasonal component from original series (additive model only)
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# ADF significance test for stationarity
adf.test(count_ma, alternative = "stationary")

# autocorrelation plot (correlation b/t series and lags)
acf(count_ma, main='')

# partial autocorrelation plot (correlation b/t variable and lags that isn't explained by previous lags )
pacf(count_ma, main='')

# calculate difference param for ARIMA
count_d1 = diff(deseasonal_cnt, differencs = 1)
plot(count_d1)
# verify parameter with ADF significance test
adf.test(count_d1, alternative = "stationary")

cow_test <- read.csv("inst/extdata/temp/Bannock_2017_101_1149.csv", skipNul = T) %>%
  clean_location_data(aniid = "1149", gpsid = "101")

ggplot(cow_test, aes(DateTime, Altitude)) + geom_line() 

altitude_ts <- ts(cow_test$Altitude)
