####################
# clean GPS cow data
# code by Joe Champion (joechampion@gmail.com), 05/22/2018
# data and guidance by Sergio Arispe
# guidance by "Data Analysis for Mobile Action i-gotU GPS units and Lotek 3300 GPS tracking collars, Version 1",  Knight & Bailey, 1/23/2017. (MS Word Doc)


###### PRELIMINARIES
## helper function to load any needed packages
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

# load any needed packages
using("tidyr", 
      "sp",
      "rgdal",
      "raster",
      "geosphere", 
      "ggplot2")

# folder of files ready for processing
data_dir <- "data/to_process"

timezone <- "UTC"


get_file_meta <- function(data_dir){
  datafiles <- list.files(data_dir, pattern="*.csv") 
  
  gps_units <- gsub("(.*)(20)([0-9]{2}\\_)(.*)(\\_{1}.*)(\\.csv)","\\4",data_files)
  
  cow_ids <- gsub("(.*)(20)([0-9]{2}\\_)(.*\\_)(.*)(\\.csv)","\\5",data_files)
  
  # assign random ids to missing cow ids
  cow_ids_na <- cow_ids == "cowxxxx"
  cow_ids[cow_ids_na] <- sample(1000:9999, size=sum(cow_ids_na), replace=F)
  cow_ids[cow_ids_na] <- paste0("R", cow_ids[cow_ids_na])
  
  list(cows = cow_ids, gps = gps_units)
}


data_files <- list.files(data_dir, pattern="*.csv", full.names=T)
data_info <- get_file_meta(data_dir)

window <- list(latmax = 43.3464, lonmin = -117.2305, latmin = 43.2472, lonmax=-117.101 )
  
data_sets <- list()
for (i in 1:length(data_files) ){
  df <- read.csv(data_files[i], skipNul = T)
  cowid <- data_info$cows[i]
  gpsid <- data_info$gps[i]
  nstart <- nrow(df)
  
  print(paste("processing ", nstart, "data points for cow #",cowid, "with gps unit #", gpsid, "..."))
  
  ### REMOVE BAD DATA POINTS (as described on pages 26-39 of Word Doc)
  df<- df %>% 
    tibble::add_column(Order = df$Index, .before="Index")%>%  # add Order column
    tibble::add_column(Cow = cowid, .after="Index") %>%      # add Cow column 
    tibble::add_column(GPS = gpsid, .after="Cow") %>%      # add Cow column 
    tibble::add_column(DateTime = NA, .after="GPS") %>%      # add Date/Time column
    tibble::add_column(TimeDiff = NA, .after="DateTime") %>% 
    tibble::add_column(TimeDiffMins = NA, .after="TimeDiff") %>%
    tibble::add_column(Rate = NA, .after="Distance") %>%
    tibble::add_column(CourseDiff = NA, .after="Course") %>%
    dplyr::mutate(Cow = as.factor(Cow))  %>%                     # reclassify Cow column as a categorical (factor) variable
    dplyr::mutate(DateTime = as.POSIXct(paste(Date, Time), "%Y/%m/%d %H:%M:%S", tz=timezone)) %>%  # reclassify Date as a Date variable
    dplyr::mutate(Date = as.Date(Date, "%Y/%m/%d"))  %>%            # reclassify Date as a Date variable
    dplyr::mutate(TimeDiff = as.numeric(DateTime - dplyr::lag(DateTime,1))) %>%  # compute sequential time differences (in seconds)
    dplyr::mutate(TimeDiffMins = as.numeric(difftime(DateTime,dplyr::lag(DateTime,1), units="mins")))  %>% # compute sequential time differences (in mins)
    dplyr::mutate(Rate = Distance/TimeDiffMins) %>% # compute rate of travel (meters/min)
    dplyr::mutate(CourseDiff = abs(Course - dplyr::lag(Course,1))) %>%
    dplyr::mutate(DistGeo = distGeo(cbind(Longitude, Latitude), 
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
 
  print(paste("...", nstart - nrow(df), "points removed"))
  print(paste("...total distance traveled =", round(sum(df$DistGeo)/1000, 1), "km"))
  print(paste("...saving", nrow(df), "good data points"))
  write.csv(df, paste0("data/processed/",cowid,".csv"), row.names=F)
  data_sets[[paste0("cow",cowid)]] <- df
}

saveRDS(data_sets, "cow_data.rds")

# saveRDS(data_sets, file.path("CowVisualizer","cow_data.rds"))


### visualize one of the data sets
# using("leaflet", "xts", "dplyr")
# 
# df<- data_sets[[1]]
# 
# spdf <- SpatialPointsDataFrame(coords = df[c("Longitude", "Latitude")], data = df,
#                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# 
# #### add elevation data
# using("elevatr")
# get_elev_point(spdf, src = "epqs") # ONE BY ONE (Super Slow!)
# 
# 
# using("geonames")
# options(geonamesUsername="joechampion") # create account via http://www.geonames.org/login/
# source(system.file("tests","testing.R",package="geonames"),echo=TRUE) # FAILS
# 
# 
# leaflet(spdf) %>%
#   addTiles() %>%
#   addProviderTiles("OpenTopoMap") %>%
#   addCircleMarkers(radius=6, fillOpacity = .9, stroke=F, popup = ~as.character(DateTime))

## Add big files to a .gitignore file
allfiles <- list.files(full.names = TRUE, recursive=T)
bigfiles <- allfiles[sapply(allfiles, file.size) > 9*10^6]
bigfiles <- gsub("^\\.","**",bigfiles)
fileignore <- c("**/elevation/**","*.zip","*.tar", "*.tar.gz", bigfiles )
                

fileConn<-file(".gitignore")
writeLines(fileignore, fileConn)
close(fileConn)




### CONVERT SPATIAL COORDINATES FROM LAT/LON TO UTM

## compute distance traveled
# # save lat/lon data to a spatial data frame
# 
# # helper function from https://stackoverflow.com/questions/18639967/converting-latitude-and-longitude-points-to-utm
# LongLatToUTM<-function(data, xvar, yvar, zone){
#   x<- data[[xvar]]
#   y<-data[[yvar]]
#   xy <- data.frame(ID = 1:length(x), X = x, Y = y)
#   coordinates(xy) <- c("X", "Y")
#   proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
#   res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
#   return(as.data.frame(res))
# }
# 
# utm_zone <- 11
# 
# ptsUTM <- LongLatToUTM(data = dftest, xvar = "Longitude", yvar = "Latitude", zone = utm_zone)
# 
# ptsUTM <- ptsUTM %>%
#             dplyr::mutate( Xlag = dplyr::lag(X,1), Ylag = dplyr::lag(Y,1)) %>%
#             dplyr::mutate( Dist = sqrt((X-Xlag)^2+(Y-Ylag)^2) )
# 
# 
# ptsUTM$Dist[ptsUTM$Dist<100] %>% hist





          