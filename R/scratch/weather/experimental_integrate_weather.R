library(tidyverse)
library(animaltracker)
library(lubridate)
library(rnoaa)

# test_ani <- read.csv("R/scratch/Riggs_March19_79.csv", skipNul = TRUE) %>% 
# clean_location_data(dtype = "igotu", filters = FALSE, aniid = 79)

test_ani <- demo %>% 
  select(-c(wind_direction, wind_speed, temperature, temperature_dewpoint, air_pressure))

# use date and location to look up weather

# variable documentation: https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf

dates <- list(min = min(test_ani$Date), max = max(test_ani$Date))

# choose a data set
# ?isd

# given a location, find the nearest station(s)
station_options <- isd_stations_search(lat = median(test_ani$Latitude, na.rm=TRUE), 
                                       lon = median(test_ani$Longitude, na.rm=TRUE), 
                                       radius = 10000 ) %>% 
  mutate(begin = as.Date(as.character(begin), format = "%Y%m%d"),
         end = as.Date(as.character(end), format = "%Y%m%d")) %>%
  filter(dates$min > begin, dates$max < end) %>%
  slice_head(n = 10)

## MAKE THIS INTERACTIVE, BUT DEFAULT TO CLOSEST (row 1)
station_choose <- station_options %>% slice(2)

if(nrow(station_closest) == 0){
  print("the rest of this code won't work")
}
# given dates, find the weather data from the station(s)
data_years <- year(dates$min):year(dates$max)

weather_raw <- lapply(data_years, function(x){
  isd(station_choose$usaf, station_choose$wban, x)
}) %>% 
  bind_rows

weather_df <- weather_raw %>% 
  select(raw_date = date,  raw_time = time, 
         wind_direction, wind_speed, 
         temperature, temperature_dewpoint, 
         air_pressure ) %>% 
  mutate(date =  as.Date(as.character(raw_date), format = "%Y%m%d"),
         datetime = as.POSIXct(paste(raw_date, raw_time), format = "%Y%m%d %H%M", tz = "UTC"),
         datehr = round_date(datetime, unit = "hour")
  ) %>% 
  mutate_at(vars(wind_direction, wind_speed, temperature, temperature_dewpoint, air_pressure), 
            function(x){ # convert strings to numeric format, remove NAs (indicated by 9999)
              xdata <- x
              xdata[xdata %in% c("999", "9999", "99999", "+9999")] <- NA
              as.numeric(xdata)
            }) %>%
  mutate_at(vars(temperature, temperature_dewpoint, air_pressure), function(x) x/10 ) %>%
  filter(datetime >= min(round_date(test_ani$DateTime-hours(12), unit="hour"), na.rm=TRUE), 
         datetime <= max(round_date(test_ani$DateTime+hours(12), unit="hour"), na.rm=TRUE),
         !is.na(datehr),
         !duplicated(datehr) # note: might be better to group_by(datehr) and aggregate/average
  )

# build a time series of the weather data (hourly)

date_time_seq <- seq.POSIXt(min(weather_df$datehr), 
                            max(weather_df$datehr), by = 'hour')


## create time series object with 3 columns: DateTime, Longitude, Latitude
weather_ts <- data.frame(datehr = date_time_seq) %>% 
  left_join( weather_df, by = "datehr") 

# round the animal data to the nearest hour
# left_join weather ts to the animal data

test_ani_aug <- test_ani %>% 
  mutate(datehr = round_date(DateTime, unit= "hour")) %>%
  left_join(weather_ts) 

####################
# ANALYZE POTENTIAL WEATHER EFFECTS

# rate by hour of the day
test_ani_aug %>% 
  filter(Latitude!=0, Longitude!=0,!is.na(Rate), !is.na(DistGeo)) %>%
  mutate(hr = hour(datehr)) %>% 
  group_by(hr) %>% 
  summarize( n = n(),
             temp = mean(temperature),
             rate = mean(Rate),
             distance = sum(DistGeo)) %>%
  ungroup() %>%
  ggplot(aes(x = hr, y = rate))+ 
  geom_line() +
  geom_smooth()

# temperature by hour of the day
test_ani_aug %>% 
  filter(Latitude!=0, Longitude!=0,!is.na(Rate), !is.na(DistGeo)) %>%
  mutate(hr = hour(datehr)) %>% 
  group_by(hr) %>% 
  summarize( n = n(),
             temp = mean(temperature, na.rm=TRUE),
             rate = mean(Rate, na.rm=TRUE),
             distance = mean(DistGeo, na.rm=TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = hr, y = temp))+ 
  geom_line() +
  geom_smooth()

# distance, rate, and temperature by hour of the day
test_ani_aug %>% 
  filter(Latitude!=0, Longitude!=0,!is.na(Rate), !is.na(DistGeo)) %>%
  mutate(hr = hour(datehr)) %>% 
  group_by(hr) %>% 
  summarize( n = n(),
             temp = mean(temperature, na.rm=TRUE),
             rate = mean(Rate, na.rm=TRUE),
             distance = mean(DistGeo, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(-c(hr,n), names_to = "variable") %>%
  mutate(variable = factor(variable)) %>%
  ggplot(aes(x = hr, y = value))+ 
  geom_line() +
  geom_smooth() +
  facet_grid(rows = vars(variable) , scales = "free" )+
  theme_minimal() 

# distance, rate, and temperature by month and hour of the day
data_by_month_hr <- test_ani_aug %>% 
  filter(Latitude!=0, Longitude!=0,!is.na(Rate), !is.na(DistGeo), Keep == 1, !is.na(date)) %>%
  mutate(hr = hour(datehr), 
         mnth = month(date, label = TRUE, abbr = TRUE)) %>% 
  group_by(mnth, hr) %>% 
  summarize( n = n(),
             temp = mean(temperature, na.rm=TRUE),
             rate = mean(Rate, na.rm=TRUE),
             distance = mean(DistGeo, na.rm=TRUE)) %>%
  ungroup() %>%
  pivot_longer(-c(mnth, hr,n), names_to = "variable") %>%
  mutate(variable = factor(variable)) 

data_by_month_hr %>%
  ggplot(aes(x = hr, y = value))+ 
  geom_line() +
  geom_smooth() +
  labs(x = "Hour of the Day", 
       y = "", 
       title = "Mean Distance, Rate, and Temperature by Month and Time of Day",
       subtitle = paste("Based on a sample of n =", sum(data_by_month_hr$n[data_by_month_hr$variable == "temp"]), "GPS measurements.")
  )+
  facet_grid(rows = vars(variable), cols = vars(mnth) , scales = "free" )+
  theme_minimal() +
  ggsave("R/scratch/weather/sample_GPS_by_Temp_time.png", width = 7.5, height = 5)
