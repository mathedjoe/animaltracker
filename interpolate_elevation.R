# script for downloading and interpolating elevation data using a GPS data source and USGS data

# install.packages("elevatr")
require(elevatr) # retrieve tiles of elevation data from Amazon's Terrain service (USGS DEP3)
require(raster) # work with tiles of elevation data
require(nabor) # find nearest neighbors in data frames
require(dplyr) # wrangle data frames
require(ggplot2) # plot data

## Retrieve high resolution elevation data for the region of analysis from the internet

# Expand Sergio's limits a little bit to ensure GPS are not at the edges.
latmin = 43.1; # deg
latmax = 43.5; #deg
lonmin = -117.6; #deg
lonmax = -117.0; #deg

data_region <- sp::bbox(cbind(c(lonmin, lonmax), c(latmin,latmax))) # set a bounding box for retrieval of elev data

elev <- get_aws_terrain( data_region, z=12, prj = "+proj=longlat") # retrieve high res elev data

elev2<- projectRaster(elev, crs = "+proj=utm +zone=11 ellps=WGS84")
# raster::extent(elev) <- raster::extent(lonmin, lonmax, latmin, latmax) # fix lon/lat to match bounding box

elevpts <- raster::rasterToPoints(elev2, spatial=TRUE) # convert to spatial pts

# names(elevpts) <- c("lon", "lat", "alt")
# coordinates(elevpts) <- ~lon+lat

## Optional - Save high res elevation data to computer
# saveRDS(elevpts, "~/elevpts.rds")
# elevpts <- readRDS("~/elevpts.rds")

## Load GPS data from a provided csv
datapts <- read.csv("data/elev_test/cdata.csv") %>% 
  select(lon = Longitude, lat = Latitude, alt = Altitude )

## Load GPS data from a provided csv
datapts <- read.csv("data/elev_test/cdata.csv") %>% 
  select(x = Longitude, y = Latitude)
datapts <- rgdal::project(as.matrix(datapts), "+proj=utm +zone=11 ellps=WGS84")
datapts <- as.data.frame(datapts)
datapts$alt <- read.csv("data/elev_test/cdata.csv")$Altitude
coordinates(datapts) <- ~x+y


datapts_elev <- nabor::knn(coordinates(elevpts), coordinates(datapts), k=1) 

datapts$elev <- elevpts$layer[ datapts_elev$nn.idx] 

ggplot(as.data.frame(datapts), aes(x = elev - alt)) + 
  xlim(-100,100)+
  geom_histogram(aes(y=..density..), colour="blue", fill="lightblue", binwidth = 2 )+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept = mean((elev-alt)[abs(elev-alt) <= 100])),col='blue',size=2)+
  labs(title = "Distribution of Modeled Elevation - Measured Altitude (meters)")+
  theme_minimal()+
  ggsave("elev-alt_diff_plot.png")

write.csv(as.data.frame(datapts)[,1:4], "data_pts.csv", row.names=F)






# consider spot checking http://www.gpsvisualizer.com/elevation


## Add elevation data to cowdata
cowdata <- readRDS("cow_data.rds")

for ( i in 1:length(cowdata) ){
  cowdf <- cowdata[[i]]
  print(names(cowdata)[i]) 
  datapts <- as.matrix(cowdf[c("Longitude", "Latitude")] )
  colnames(datapts) <- c("x","y")
  datapts <- rgdal::project(as.matrix(datapts), "+proj=utm +zone=11 ellps=WGS84")
  datapts <- as.data.frame(datapts)
  datapts$alt <- cowdf$Altitude
  coordinates(datapts) <- ~x+y
  
  datapts_elev <- nabor::knn(coordinates(elevpts), coordinates(datapts), k=1) 
  
  cowdf$Elevation <- elevpts$layer[ datapts_elev$nn.idx]
  
  cowdata[[i]]<- cowdf
   
}

saveRDS(cowdata, "cow_data_with_elev.rds")

saveRDS(cowdata, file.path("CowVisualizerApp","cow_data.rds"))
