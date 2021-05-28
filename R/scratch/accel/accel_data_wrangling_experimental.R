library(tidyverse)
df_raw <- read.csv("R/scratch/accel/gulf_coast_docs/DATA-002.csv", skip = 28)

df_locs <- df_raw[c("Lon", "Lat")] %>% 
  mutate( Lon = rnorm(nrow(.), first(Lon), .001),
          Lat = rnorm(nrow(.), first(Lat), .001),
          zone = ceiling((Lon + 180)/6)
  )

### NOTE: CALCULATING DISTANCES USING NORTHINGS AND EASTINGS DOESN'T WORK IF THE DATA CROSSES ZONES

# Setting existing coordinate as lat-long system
coords <- sp::SpatialPoints(df_locs %>% select(Lon, Lat), proj4string = sp::CRS("+proj=longlat"))

get_mode <- function(x){
  names(sort(-table(x)))[1]
}

coords_UTM <- sp::spTransform(coords, 
                              sp::CRS(paste0("+proj=utm +zone=", get_mode(df_locs$zone), " ellps=WGS84"))) %>% 
  as.data.frame() %>% 
  dplyr::rename(Northing=Lat, Easting = Lon) %>% 
  mutate(UTM_Dist = sqrt( (Northing - lag(Northing))^2 + (Easting - lag(Easting))^2 ) )

df <- df_locs %>% 
  cbind( as.data.frame(coords_UTM))
