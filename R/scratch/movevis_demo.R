library(animaltracker)
library(moveVis)
library(move)
library(raster)
library(magrittr)

demo <- animaltracker::demo

demo_move <- df2move(demo, proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                     x = "Longitude", y = "Latitude", time = "DateTime", track_id = "Animal") %>% align_move(res = 4, unit = "mins")

frames <- frames_spatial(demo_move,
                         map_service = "carto", map_type = "light", alpha = 0.5)

animate_frames(frames, out_file = "R/scratch/test_animation.gif")
