# replace progress bar when downloading DEMs
library(animaltracker)
require(dplyr)
df <- demo_unfiltered %>% dplyr::select(x = Longitude, y = Latitude)


# Custom aws tile fetcher
custom_get_aws_terrain <- function (locations, z, prj, ...) {
  ll_geo <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  locations <- sp::SpatialPointsDataFrame(sp::coordinates(locations), proj4string = sp::CRS(prj), 
                                          data =  data.frame(elevation = vector("numeric", nrow(locations))))
  bbx <- sp::bbox(locations)
  bbx <- sp::bbox(sp::spTransform(sp::SpatialPoints(t(sp::coordinates(bbx)), 
                                                    bbox = bbx, proj4string = sp::CRS(prj)), sp::CRS(ll_geo)))
  
  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff/"
  
  latlong_to_tilexy <- function (lon_deg, lat_deg, zoom) {
    lat_rad <- lat_deg * pi/180
    n <- 2^zoom
    xtile <- (lon_deg + 180)/360 * n
    ytile <- (1 - log(tan(lat_rad) + (1/cos(lat_rad)))/pi)/2 * 
      n
    return(c(xtile, ytile))
  }
  get_tilexy <- function (bbx, z) {
    min_tile <- latlong_to_tilexy(bbx[1, 1], bbx[2, 1], z)
    max_tile <- latlong_to_tilexy(bbx[1, 2], bbx[2, 2], z)
    x_all <- seq(from = floor(min_tile[1]), to = ceiling(max_tile[1]))
    y_all <- seq(from = ceiling(min_tile[2]), to = floor(max_tile[2]))
    return(expand.grid(x_all, y_all))
  }
  
  tiles <- get_tilexy(bbx, z)
  dem_list <- vector("list", length = nrow(tiles))
  pb <- progress::progress_bar$new(format = "SUPER Downloading DEMs [:bar] :percent eta: :eta", 
                                   total = nrow(tiles), clear = FALSE, width = 60)
  for (i in seq_along(tiles[, 1])) {
    pb$tick()
    Sys.sleep(1/100)
    tmpfile <- tempfile()
    url <- paste0(base_url, z, "/", tiles[i, 1], "/", 
                  tiles[i, 2], ".tif")
    resp <- httr::GET(url, httr::write_disk(tmpfile, overwrite = TRUE), 
                      ...)
    if (httr::http_type(resp) != "image/tiff") {
      stop("API did not return tif", call. = FALSE)
    }
    dem_list[[i]] <- raster::raster(tmpfile)
    raster::projection(dem_list[[i]]) <- web_merc
  }
  origins <- t(data.frame(lapply(dem_list, raster::origin)))
  min_origin <- c(min(origins[, 1]), min(origins[, 2]))
  change_origins <- function(x, y) {
    raster::origin(x) <- y
    x
  }
  dem_list <- lapply(dem_list, function(x, y) change_origins(x, 
                                                             min_origin))
  if (length(dem_list) == 1) {
    return(dem_list[[1]])
  }
  else if (length(dem_list) > 1) {
    message("Merging DEMs")
    return(do.call(raster::merge, dem_list))
  }
  
}

test_data <- custom_get_aws_terrain(df, prj = "+proj=longlat", z=6)
test_data <- raster::projectRaster(test_data, crs = sp::CRS( "+proj=longlat"))
# convert terrain data to spatial pts
elevpts <- raster::rasterToPoints(test_data, spatial=TRUE) 

elev_coords <- sp::coordinates(elevpts)

# determine nearest neighbors in the terrain data for the animal locations
datapts_elev <- nabor::knn(data =elev_coords, query = df, k=1)

# add Elevation column to the animal data
xx <- elevpts$layer[ datapts_elev$nn.idx]
