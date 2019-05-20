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

cow <- read.csv("inst/extdata/temp/Bannock_2017_105_2289.csv", skipNul = T) %>%
  clean_location_data(aniid = "2289", gpsid = "105") %>%
  dplyr::filter(Latitude <= median(Latitude) + 2.5,
                Latitude >= median(Latitude) - 2.5,
                Longitude <= median(Longitude) + 2.5,
                Longitude >= median(Longitude) - 2.5)

cow_ts <- read.csv("inst/extdata/temp/Bannock_2017_105_2289.csv", skipNul = T) %>%
  clean_location_ts("2289", "105") %>%
  dplyr::filter(Latitude <= median(Latitude) + 2.5,
                Latitude >= median(Latitude) - 2.5,
                Longitude <= median(Longitude) + 2.5,
                Longitude >= median(Longitude) - 2.5) %>%
  lookup_elevation()

clean_location_ts <- function (df, aniid = NA, gpsid = NA, maxrate = 84, maxcourse = 100, maxdist = 840, maxtime=100, timezone = "UTC"){
  require(dplyr)
  require(tibble)
  df %>% 
    tibble::add_column(Order = df$Index, .before="Index")%>%  # add Order column
    tibble::add_column(Animal = aniid, .after="Index") %>%      # add Animal column 
    tibble::add_column(GPS = gpsid, .after="Animal") %>%      # add Animal column 
    tibble::add_column(DateTime = NA, .after="GPS") %>%      # add Date/Time column
    tibble::add_column(TimeDiff = NA, .after="DateTime") %>% 
    tibble::add_column(TimeDiffMins = NA, .after="TimeDiff") %>%
    tibble::add_column(Rate = NA, .after="Distance") %>%
    tibble::add_column(CourseDiff = NA, .after="Course") %>%
    dplyr::mutate(
      Animal = as.factor(Animal), # reclassify Animal column as a categorical (factor) variable
      DateTime = as.POSIXct(paste(Date, Time), "%Y/%m/%d %H:%M:%S", tz=timezone), # reclassify Date as a Date variable
      Date = as.Date(Date, "%Y/%m/%d"), # reclassify Date as a Date variable
      Time = as.character(Time),
      Latitude = forecast::tsclean(Latitude),
      Longitude = forecast::tsclean(Longitude),
      Altitude = forecast::tsclean(Altitude),
      Distance = forecast::tsclean(Distance)
    ) %>%
    dplyr::filter(!is.na(DateTime), !is.na(Date), !is.na(Time)) %>% # filter missing time slots before calculating differences
    dplyr::distinct(DateTime, .keep_all = TRUE) %>% # remove duplicate timestamps
    dplyr::mutate(
      TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins")) > maxtime), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), # compute sequential time differences (in seconds)
      TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))), # compute sequential time differences (in mins)
      Rate = ifelse(TimeDiffMins != 0, Distance/TimeDiffMins, 0), # compute rate of travel (meters/min), default to 0 to prevent divide by 0 error
      CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
      DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                   cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) ))), #compute geodesic distance between points
      
      RateFlag = 1*(Rate > maxrate), # flag any data points representing too fast travel
      CourseFlag = 1*(CourseDiff >= maxcourse) ,
      DistanceFlag = 1*(DistGeo >= maxdist ),
      TotalFlags = RateFlag + CourseFlag + DistanceFlag
    ) %>%
    dplyr::filter(TotalFlags < 2,
                  !DistanceFlag ) %>%
    dplyr::mutate( # recalculate columns affected by filtering
      TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins")) > maxtime), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), 
      TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))),
      Rate = ifelse(TimeDiffMins != 0, Distance/TimeDiffMins, 0),
      CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
      DistGeo = geosphere::distGeo(cbind(Longitude, Latitude),
                                   cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude))))
    ) %>%
    dplyr::select(-c("RateFlag", "CourseFlag", "DistanceFlag", "TotalFlags")) # remove flags after use
}

# weekly moving average
cow_ts$ma_7 <- ma(cow_ts$Elevation, order=7) # using the clean count with no outliers

#monthly moving average
cow_ts$ma_30 <- ma(cow_ts$Elevation, order=30)

ggplot() +
  geom_line(data = cow_ts, aes(x = DateTime, y = Elevation, colour = "Elevation")) +
  geom_line(data = cow_ts, aes(x = DateTime, y = ma_7, colour = "Weekly Moving Average")) +
  geom_line(data = cow_ts, aes(x = DateTime, y = ma_30, colour = "Monthly Moving Average")) +
  ylab('Elevation')
