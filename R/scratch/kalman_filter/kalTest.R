library(ggplot2)
library(KFKSDS)
library(stsm)
library(timeSeries)
library(FKF)

setwd("R/scratch/kalman_filter") # optional - set a new working directory

cattle.data <- read.csv(file = "test2.csv")

ggplot(cattle.data, aes(y = Latitude, x = Longitude)) + geom_line() +
  xlim(-116, -117) + ylim(43, 44)

latVec <- cattle.data[["Latitude"]]  # by column name
longVec <- cattle.data[["Longitude"]]  # by column name
timeVec <- cattle.data[["Time"]]

#c() Combine Values Into A Vector Or List
#y is the data set
data_1 <- data.frame(latVec,longVec, timeVec)     # Create example data fra
data_2 <-ts(data_1)

m <- stsm::stsm.model(model = "llm+seas", y = data_2,
                      pars = c("var1" = 2, "var2" = 15, "var3" = 30))
ss <- stsm::char2numeric(m)
KF(latVec, ss, convergence = c(0.001, length(latVec)), t0 = 1)


library(tidyverse) # data visualization and wrangling
library(tsbox) # converting between time series formats
library(imputeTS) # for filling in missing / outlier data

setwd("R/scratch/kalman_filter") # optional - set a new working directory


## read sample data
## - label outliers using a 'window' on longitudes and latitudes
cattle_df <- read.csv(file = "test2.csv") %>% 
  mutate(DateTime = paste(Date, Time),
         DateTime = as.POSIXct(DateTime),
         isOutlier = Longitude < -117 | Longitude > -116 | Latitude < 43 | Latitude > 44
  ) %>%
  arrange(DateTime)

## PLOT the raw geo paths by date
## - exclude outliers 
cattle_df %>% 
  filter(!isOutlier) %>% 
  ggplot(aes(x = Longitude, y = Latitude)) + 
  geom_path( aes(group = Date, color = Date), size = 1.5) +
  theme_minimal()+
  ggtitle("Sample Animal Trajectory", 
          subtitle = paste0("Raw Data",
                            ", sample size = ", sum(!cattle_df$isOutlier, na.rm=TRUE)
          )
  )

## convert the irregular measurement data to a time series (equally spaced measurements)
##  - for each date in the sample, build a sequence of equally spaced times at 
##  - use one second intervals
##  - this will make a MUCH larger data set, with many NA values for missing measurements
date_time_seq <- lapply( unique( cattle_df$Date[!cattle_df$isOutlier] ),
                         function(this_date){
                           df_day <- cattle_df %>% 
                             filter(Date == this_date, !isOutlier)
                           # make an equally-spaced sequence of date-times for this date
                           seq.POSIXt(min(df_day$DateTime), 
                                      max(df_day$DateTime), by = 'sec')
                         }
)  %>% Reduce(c, .)
                        

## create time series object with 3 columns: DateTime, Longitude, Latitude
cattle_ts <- data.frame(DateTime = date_time_seq) %>% 
  left_join(cattle_df, by = "DateTime") %>% 
  mutate(Longitude = ifelse(isOutlier, NA, Longitude),
         Latitude = ifelse(isOutlier, NA, Latitude)) %>% 
  select(DateTime, Longitude, Latitude) %>%
  ts_df() # uses library tsbox

long_clean <- forecast::tsoutliers(ts(cattle_ts["Longitude"] ) )

## fill in missing data using kalman smoothing
# do not attempt to fill-in more than maxgap measurements (e.g., 5 minutes)
cattle_ts_smoothed <- na_kalman(cattle_ts, 
                                model = "StructTS", smooth = TRUE, type = "trend",
                                maxgap = 60*5) %>% 
  rename(Latitude_Clean = Latitude, Longitude_Clean = Longitude) %>% 
  filter(!is.na(Latitude_Clean), !is.na(Longitude_Clean)) %>% 
  left_join(cattle_df %>% filter(!isOutlier), by = "DateTime") %>%
  mutate(Date = as.factor(as.character(as.Date(DateTime))) ) 

## PLOT both the raw and the smoothed geo paths by date
ggplot() + 
  # draw original data in gray as a background
  geom_point( data = cattle_df %>% filter(!isOutlier), 
              aes(x = Longitude, y = Latitude, group = Date), 
              size = 5, alpha = .5, color = "#aaaaaa") +
  # draw smoothed data in color as foreground
  geom_point(data = cattle_ts_smoothed, 
             aes(x = Longitude_Clean, y = Latitude_Clean, group = Date, color = Date), 
             size = 2, alpha = 1) +
  geom_path( data = cattle_ts_smoothed, 
             aes(x = Longitude_Clean, y = Latitude_Clean, group = Date, color = Date), 
             size = 1, alpha = 1) +
  theme_minimal()+
  ggtitle("Sample Animal Trajectory", 
          subtitle = paste0("Raw and Smoothed Data",
                            ", original n =",  nrow(cattle_df),
                            ", filtered n = ", sum(!cattle_df$isOutlier, na.rm=TRUE),
                            ", cleaned n = ", nrow(cattle_ts_smoothed)
          )
  )

## PLOT ONE TIME SERIES (e.g., Latitude) ON A PARTICULAR DATE
ggplot() + 
  geom_point( data = cattle_df %>% filter(!isOutlier, Date == "2018/01/18"), 
              aes(x = DateTime, y = Latitude, group = Date), 
              size = 5, alpha = .5, color = "#aaaaaa") +
  geom_point(data = cattle_ts_smoothed %>% filter(Date == "2018-01-18"), 
             aes(x = DateTime, y = Latitude_Clean, group = Date, color = Date), 
             size = 2, alpha = 1) +
  geom_path( data = cattle_ts_smoothed %>% filter(Date == "2018-01-18"), 
             aes(x = DateTime, y = Latitude_Clean, group = Date, color = Date), 
             size = 1, alpha = 1) +
  theme_minimal()+
  ggtitle("Sample Animal Trajectory", 
          subtitle = paste0("Raw and Smoothed Data",
                            ", original n =",  nrow(cattle_df),
                            ", filtered n = ", sum(!cattle_df$isOutlier, na.rm=TRUE),
                            ", cleaned n = ", nrow(cattle_ts_smoothed)
          )
  )
