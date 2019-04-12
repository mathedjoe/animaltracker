# install.packages("GSODR")
library(GSODR)

mystations <- nearest_stations(-27.56, 151.95, 100)

length(mystations) # should be 17

sample(x = c(0,1), size = 100, prob=c(.88, .12), replace=TRUE)

# create a vector of 0s
x<- rep(0,6322)

# replace every nth 0 with a 1, where n = .12*length(x)
x [ seq(1, length(x), round(.12*length(x)))] <- 1

xx <- read.csv("data/dirty_sample_data.csv", skipNul = T, as.is=T)
nrow(xx)
View(xx)
xx$Latitude <- as.numeric(xx$Latitude)
xx<- xx[!is.na(xx$Latitude),]
nrow(xx)
xx<- xx[xx$Latitude!="Latitude",]
nrow(xx)
df[df$Latitude == ""]

install.packages("GSODR")
data <- readRDS("R/scratch/cow_data.rds")

fetch_temp_data <- function(test_data, radius, data_yrs){
  require(GSODR)
  require(dplyr)
  data_yrs <- as.numeric(unique(format(data$Date, "%Y")))
  
  find_nearest_data <- function( test_data, radius, year = data_yrs){
    lat <- median(test_data$Latitude, na.rm = TRUE)
    lon <- median(test_data$Longitude, na.rm = TRUE)
    mystations <- nearest_stations(lat, lon, radius)
    
    #   print(mystations)
    
    dist <- 5
    df_out <- get_GSOD (years = year, station = mystations[1], country = "US")
    
    for (stat in mystations){
      df <- get_GSOD (years = year, station = stat, country = "US")
      if(nrow(df) >0){
        lat_stat <- median(df$LAT, na.rm=T)
        lon_stat <- median(df$LON, na.rm=T)
        newdist <- sqrt((lat - lat_stat)^2+ (lon-lon_stat)^2)
        print(c(stat, lat_stat, lon_stat, newdist))
        if(newdist < dist){
          print(paste(stat, "is the new closest stations"))
          df_out <- df
          dist <- newdist
          #       print(newdist)
        }
      }
      
    }
    #    print(unique(df_out$STNID))
    df_out
  }
  tempdata <- find_nearest_data(test_data, radius)
  
  tempdata <- dplyr::bind_rows(tempdata, id = NULL)
  print(names(tempdata))
  # combine year, month and date into one column
  tempdata$Date <-as.Date(paste0(tempdata$YEAR, "-", tempdata$MONTH, "-", tempdata$DAY))
  
  temp_data <- tempdata %>% 
    select(Date, 
           STN_ID = STNID, STN_NAME, 
           STN_LAT = LAT, STN_LON = LON, 
           TEMP, 
           DEWP, 
           PRCP, 
           TEMP_MIN = MIN, TEMP_MAX = MAX)
  
  left_join(test_data, temp_data, by="Date")
}
data <- dplyr::bind_rows(data, id = NULL)
final_data <- fetch_temp_data(data, 60, 2017)
final_data

  
  