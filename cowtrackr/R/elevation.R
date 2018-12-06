# estimating elevation for lat/lon points in gps data



df<- data_sets[[1]]

spdf <- SpatialPointsDataFrame(coords = df[c("Longitude", "Latitude")], data = df,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#### add elevation data using API

# USGS API
using("elevatr")
get_elev_point(spdf, src = "epqs") # ONE BY ONE (Super Slow!)

# GEONAMES API
using("geonames")
options(geonamesUsername="joechampion") # create account via http://www.geonames.org/login/
source(system.file("tests","testing.R",package="geonames"),echo=TRUE) # FAILS

# GOOGLE MAPS API
# too expensive? (single and daily query limits are well below data sizes)

#### add elevation data via Digital Elevation Model (DEM)

# read geoTIFF elevation map
using("raster", "sp", "rgdal")
dem_file <- list.files("data/elevation", pattern=".tif", full.names = T)

DEM <- raster(dem_file)
plot(DEM, main="Digital Elevation Model for U.S.")

extent(DEM)
crs(DEM)

DEM.spf<-as(DEM,"SpatialPixelsDataFrame") # takes a long time?

DEM.gps <- projectRaster(DEM, crs = crs(spdf))

