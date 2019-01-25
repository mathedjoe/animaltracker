clean_locations<- function (df, aniid = NA, gpsid = NA, timezone = "UTC", window =  list(latmax = 43.3464, lonmin = -117.2305, latmin = 43.2472, lonmax=-117.101 )){
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
  dplyr::mutate(Animal = as.factor(Animal))  %>%                     # reclassify Animal column as a categorical (factor) variable
  dplyr::mutate(DateTime = as.POSIXct(paste(Date, Time), "%Y/%m/%d %H:%M:%S", tz=timezone)) %>%  # reclassify Date as a Date variable
  dplyr::mutate(Date = as.Date(Date, "%Y/%m/%d"))  %>%            # reclassify Date as a Date variable
  dplyr::mutate(TimeDiff = as.numeric(DateTime - dplyr::lag(DateTime,1))) %>%  # compute sequential time differences (in seconds)
  dplyr::mutate(TimeDiffMins = as.numeric(difftime(DateTime,dplyr::lag(DateTime,1), units="mins")))  %>% # compute sequential time differences (in mins)
  dplyr::mutate(Rate = Distance/TimeDiffMins) %>% # compute rate of travel (meters/min)
  dplyr::mutate(CourseDiff = abs(Course - dplyr::lag(Course,1))) %>%
  dplyr::mutate(DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                             cbind(dplyr::lag(Longitude,1), dplyr::lag(Latitude, 1))
  ) ) %>% #compute geodesic distance between points
  dplyr::mutate(RateFlag = 1*(Rate > 84)) %>%  # flag any data points representing too fast travel
  dplyr::mutate(CourseFlag = 1*(CourseDiff >= 100) ) %>%
  dplyr::mutate(DistanceFlag = 1*(DistGeo >= 840 )) %>%
  dplyr::mutate(TotalFlags = RateFlag + CourseFlag + DistanceFlag) %>%
  dplyr::filter(!is.na(DateTime), TotalFlags < 2,
                Latitude!=0, Longitude !=0,
                TimeDiffMins < 100,
                Altitude > 2700/3.3, Altitude< 6000/3.3, # lower and upper limits (converted from feet to meters)
                Latitude >= window$latmin,  Latitude <= window$latmax,
                Longitude >= window$lonmin,  Longitude <= window$lonmax,
                !DistanceFlag )
}

dftest <- read.csv("data/sample_location_data.csv", skipNul = T)
xx <- clean_locations(dftest)

# install.packages("forecast")
library(forecast)
library(ggplot2)

  ggplot( xx %>% 
            select(x = Date, y = Latitude) , aes(x = x, y=y) ) + 
    geom_line()
 zz <- tsclean(xx$Latitude)
 
 yy<- tsoutliers(xx$Latitude)
length(zz)

clean_locations_auto<- function (df, aniid = NA, gpsid = NA, timezone = "UTC"){
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
      Latitude = forecast::tsclean(Latitude),
      Longitude = forecast::tsclean(Longitude),
      Altitude = forecast::tsclean(Altitude),
      Distance = forecast::tsclean(Distance),
      Animal = as.factor(Animal), # reclassify Animal column as a categorical (factor) variable
      DateTime = as.POSIXct(paste(Date, Time), "%Y/%m/%d %H:%M:%S", tz=timezone), # reclassify Date as a Date variable
      Date = as.Date(Date, "%Y/%m/%d"), # reclassify Date as a Date variable
      TimeDiff = as.numeric(DateTime - dplyr::lag(DateTime,1)), # compute sequential time differences (in seconds)
      TimeDiffMins = as.numeric(difftime(DateTime,dplyr::lag(DateTime,1), units="mins")), # compute sequential time differences (in mins)
      Rate = Distance/TimeDiffMins, # compute rate of travel (meters/min),
      CourseDiff = abs(Course - dplyr::lag(Course,1)),
      DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                   cbind(dplyr::lag(Longitude,1), dplyr::lag(Latitude, 1) )), #compute geodesic distance between points
    
      RateFlag = 1*(Rate > 84), # flag any data points representing too fast travel
      CourseFlag = 1*(CourseDiff >= 100) ,
      DistanceFlag = 1*(DistGeo >= 840 ),
      TotalFlags = RateFlag + CourseFlag + DistanceFlag
                ) %>%
  dplyr::filter(!is.na(DateTime), TotalFlags < 2,
                TimeDiffMins < 100,
                !DistanceFlag )
}

xx2 <- clean_locations_auto(dftest)

# Try batch cleaning a list of data frames

unzip("data/to_process.zip", exdir = "data/temp")

to_process <- list.files("data/temp", full.names=T)

dfs <- lapply(to_process, function(filename){
  df <- read.csv(filename, skipNul = T)
  clean_locations_auto(df)
})