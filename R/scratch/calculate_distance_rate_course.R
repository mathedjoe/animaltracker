df <- readRDS("2dtype_test.rds")[[1]]

df2 <- readRDS("2dtype_test.rds")[[2]]

library(dplyr)

df_check <- df %>% 
  mutate( distGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                   cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) )))
  )

# Bearing formula from https://www.movable-type.co.uk/scripts/latlong.html

calc_bearing <- function(lat1, lon1, lat2, lon2){
  bearing_radian <- atan2( sin(lon2-lon1)*cos(lat2) , cos(lat1)*sin(lat2) - sin(lat1)*cos(lat2)*cos(lon2-lon1) )
  (bearing_radian * 180/pi +360 )%% 360
}

calc_bearing(55.739399, 37.592572, 55.735632, 37.678367)
geosphere::distGeo(c(55.739399, 37.592572), c(55.735632, 37.678367))

sapply(1:20, function(x){calc_bearing(df2$Latitude[1], df2$Longitude[1], df2$Latitude[x], df2$Longitude[x])})
