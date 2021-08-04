library(animaltracker)
library(moveVis)
library(move)
library(raster)
library(magrittr)
library(RColorBrewer)

demo <- animaltracker::demo

df_animate <- demo %>% 
  filter(Date == "2017-12-13", 
         Animal %in% c("1149", "2253", "8855", "9964")) %>% 
  group_by(Animal) %>% 
  arrange(DateTime) %>% 
  sample_n(50)

n_animals <- length(unique(df_animate$Animal))
animal_colors <- brewer.pal(n_animals, "Dark2")
  
demo_move <- df2move(df_animate, 
                     proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                     x = "Longitude", y = "Latitude", time = "DateTime", track_id = "Animal") %>% 
  align_move(res = 10, unit = "mins")

frames <- frames_spatial(demo_move,
                         map_service = "osm", 
                         map_type = "streets", 
                         path_legend_title = "Animal ID",
                         path_colours = animal_colors,
                         alpha = 0.5) %>% 
  add_timestamps() %>% 
  add_labels(x = "", y="", title = "Animation of Animal Locations")

frames[[100]]
animate_frames(frames, out_file = "R/scratch/demo_animation.mov", overwrite=TRUE)
