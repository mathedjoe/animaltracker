library("animaltracker")
library("tidyverse") # data wrangling and plots
library("gganimate") # animated gif plots
library("gifski") # for display of gif plots


# get some raw trajectory data (to cross-validated with other outlier analyses)

raw_files <- list.files("test_data/DeepWell_2018_Collar_Raw", full.names = TRUE) 
ani_ids <- gsub("(.*DW_)(\\d+)(_.*)","\\2", raw_files)
df <- lapply(1:length(raw_files), function(i){
  clean_location_data(read.csv(raw_files[i]), dtype="igotu", 
      prep = TRUE, filters = FALSE, 
      aniid = ani_ids[i], gpsid = NA, 
      maxrate = 84, maxcourse = 100, maxdist = 840, maxtime=60*60,
      tz_in = "UTC", tz_out = "UTC") %>% 
    mutate(
      Order = as.integer(Order),
      Index = as.integer(Index),
      Altitude = as.numeric(Altitude),
      EHPE = as.numeric(EHPE),
      Speed = as.numeric(Speed),
      Timeout = as.character(Timeout),
      Satelite.ID = as.character( Satelite.ID),
      SleepTime = as.character(SleepTime),
      Keep = as.factor(Keep),
      TimeElapsed = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S"), 
      TimeElapsed = TimeElapsed- first(TimeElapsed)
    )
  }) %>% 
  bind_rows()


# look for good cow/date combinations to sample for outliers
sampling_combos <- df %>% 
  group_by(Animal, Date) %>% 
  summarize( n = n(), Drop = sum(Keep == 0), Keep = sum(Keep == 1),  Keep = round(100*Keep/n,2) ) %>% 
  filter(Keep < 99.5) %>% 
  arrange(Keep)

#############
## simple visualization for one cow, one day
df_sample <- df %>% 
  # filter(Date == "2018-06-16" | Date == "2018-06-17" | Date == "2018-06-18" | Date == "2018-06-19") 
   # filter(Date == sampling_combos$Date[1], Animal == sampling_combos$Animal[1] )
  filter(Date == "2018-05-24", Animal == "011")

table(df_sample$Keep, exclude = NULL)

## 2d trajectory
p_latlon <- df_sample %>%
  ggplot( aes(x = Longitude, y= Latitude) )  +
  geom_line()+
  geom_point( aes(color = Keep))+
  theme_minimal()+
  coord_fixed()

##  time series for lat / lon
p_lat <- df_sample %>% 
  ggplot( aes(x = TimeElapsed, y= Latitude) ) +
  geom_line()+
  geom_point(aes(color = Keep))+
  theme_minimal()

p_lon <-  df_sample %>% 
  ggplot( aes(x = TimeElapsed, y= Longitude) ) +
    geom_line()+
    geom_point(aes(color = Keep))+
    theme_minimal()

ggpubr::ggarrange(
  p_latlon,  # first row             
  ggpubr::ggarrange(p_lat, p_lon, 
            ncol = 2, labels = c("B", "C")), #second row
  nrow = 2, 
  labels = "A"       # Label of the line plot
) 

###############
## AUTOMATED APPROACH #1
## TIME SERIES
## Use `forecast` library to clean lat and longitude as separate time series, combine
library(forecast)
tsoutliers_lat <- tsoutliers(df_sample$Latitude)$index
tsoutliers_lon <- tsoutliers(df_sample$Longitude)$index

length(tsoutliers_lat)
length(tsoutliers_lon)

df_sample$tsout_lat <- 1*(1:nrow(df_sample) %in% tsoutliers_lat)
df_sample$tsout_lon <- 1*(1:nrow(df_sample) %in% tsoutliers_lon)
df_sample$tsout <- 1*(1:nrow(df_sample) %in% c(tsoutliers_lon,tsoutliers_lat) )

with(df_sample, table(tsout, 1*(Keep=='0')))  %>% prop.table() %>% round(.,2)
# APPEARS TO BE OVERLY SENSITIVE (labels 10+% outliers not identified by the manual cleaning method)

###############
## AUTOMATED APPROACH #2
## Kalman filter / Kalman smoothing (common in GPS)


###############
## AUTOMATED APPROACH #3
## MARSS modeling (fancy dynamic modeling of animal motion)
# https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf
# see Ch 11 of User Guide
# install.packages("MARSS")

library(MARSS)
dat <- df_sample %>% select(Longitude, Latitude) %>% t

gps_error <- 10^(-3)
kemod <- list( Z = "identity", # design matrix
              U = "unconstrained", # rates of travel in lat/lon directions
              Q = "equalvarcov", # variation in rate of travel
              R = "diagonal and equal" # errors in location
              # R = matrix(list(gps_error,0,0,gps_error), nrow = 2, ncol = 2, byrow = TRUE)

)
kem <- MARSS(dat, model = kemod, method = "kem", control = list(conv.test.slope.tol = .1))


## identify outliers via residuals
kem_resids <- MARSSresiduals(kem, type="tT", normalize=TRUE)$state.residuals %>% t() %>% as.data.frame

df_sample_pred <- df_sample %>%
  bind_cols(., tibble(Longitude.Pred = kem$states[1,], 
                      Latitude.Pred = kem$states[2,],
                      res_lon = kem_resids$X.Longitude, 
                      res_lat = kem_resids$X.Latitude)) %>% 
  mutate(
         out_lon = 1* (abs(res_lon) > 2),
         out_lat = 1* (abs(res_lat) > 2),
         out_marss = as.factor( out_lon + out_lat )
  ) %>% 
  arrange(DateTime) %>% 
  mutate(index = 1:nrow(.))

# color scale for out_marss outlier indicator
col_out_scale <-  c("#1a9641", "#fdae61", "#d7191c") 

# hist(df_sample_pred$res_lat)
# hist(df_sample_pred$res_lon)


  ## 2d trajectory
  p_latlon <-  df_sample_pred %>% 
    ggplot() +
    geom_line(aes(x = Longitude.Pred, y= Latitude.Pred), color = "#333333")+
    geom_point( aes(x = Longitude, y= Latitude, color = out_marss, group = seq_along(index)) )+
    scale_discrete_manual("color", values = col_out_scale)+
    theme_minimal()+
    coord_fixed() 
  
  # animate(p_latlon + transition_reveal(index) ,  fps = 15, width = 800, height = 800, renderer = gifski_renderer(loop = FALSE))
  # anim_save("R/scratch/ts_clean/animate_spatialplot.gif")
  
  ##  time series for lat / lon
  p_lat <- df_sample_pred %>% 
    ggplot( aes(x = TimeElapsed) ) +
    geom_line(aes(y= Latitude.Pred), color = "#333333")+
    geom_point( aes(y= Latitude, color = out_marss, group = seq_along(index)) )+
    scale_discrete_manual("color", values = col_out_scale)+
    theme_minimal()
  
  p_lon <-  df_sample_pred %>% 
    ggplot( aes(x = TimeElapsed) ) +
    geom_line(aes(y= Longitude.Pred), color = "#333333")+
    geom_point( aes(y= Longitude, color = out_marss, group = seq_along(index)) )+
    scale_discrete_manual("color", values = col_out_scale)+
    theme_minimal()
  
  p_combined <- ggpubr::ggarrange(
    p_latlon,  # first row             
    ggpubr::ggarrange(p_lat, p_lon, 
                      ncol = 2, labels = c("B", "C")), #second row
    nrow = 2, 
    labels = "A"       # Label of the line plot
  ) 
  

  p_combined
  
  with(df_sample_pred, table(out_marss, 1*(Keep=='0')))  
  # %>% prop.table() %>% round(.,2)

  

###############
## AUTOMATED APPROACH #4
## IMPLEMENT Density Based Clustering via the DBSCAN algorithm / package
# https://cran.r-project.org/web/packages/dbscan/vignettes/dbscan.pdf

install.packages("dbscan")
library("dbscan")

cluster_analyze <- function(data, animal, date, knn_k = 5, knn_eps = .001, verbose = TRUE){
  df <- data %>% 
    filter(Date == date, Animal == animal) %>%
    select(Longitude, Latitude)
  
  res <- dbscan(df, eps = knn_eps, minPts = knn_k )
  if(verbose){ print( res )}
  
  data_out <- df %>%
    mutate(cluster = as.factor(ifelse(res$cluster ==0, NA, res$cluster))) 
  
  plot_out <- ggplot( data_out, aes(x = Longitude, y= Latitude, color = cluster) ) +
    geom_point(  )+
    geom_line( data = data_out[!is.na(data_out$cluster),] )+
    labs(title = "Density Based Spatial Clustering", subtitle = paste0("date = ", date,"; animal = ", animal)) +
    theme_minimal()+
    coord_fixed()
  
  if(verbose){ print(plot_out)}
  
  list(data = data_out, plot = plot_out)
}

# set-up neighbor search radii
# about 111 km per degree latitude
# cows = 84 m max travel between measurements
# cars = 80 mi/h max (= 129 km/h)
cow_eps <- 84/(111*1000)

car_eps <- 128.75 /(6*111)

my_clusters <- cluster_analyze(df, "011", "2018-06-22", knn_eps = cow_eps) # maybe works?

ex_clusters <- cluster_analyze(df, sampling_combos$Animal[28], sampling_combos$Date[28], knn_eps = cow_eps)

ex_clusters_car <- cluster_analyze(df, sampling_combos$Animal[2], sampling_combos$Date[2], 
                                   knn_eps = car_eps*.48)


## get cluster analysis plots for all the animal / date examples in the sampling combos data

my_dbscans <- lapply( 1:nrow(sampling_combos), 
                      function(i){
                        cluster_analyze(df, sampling_combos$Animal[i], sampling_combos$Date[i], 
                                        knn_eps = cow_eps, verbose = FALSE)$plot
                      } 
)

my_dbscans[[28]]

## save cluster analysis plots to a file
pdf("R/scratch/ts_clean/cluster_analysis_DW.pdf",onefile = TRUE)
for(i in 1:length(my_dbscans)){
  print(my_dbscans[[i]])
}
dev.off()

###########
## try OPTICS algorithm
## helps identify eps value for dbscan
xdf <- df %>% 
  filter(Date == "2018-06-22", Animal == "011") %>%
  select(Longitude, Latitude)

res <- optics(xdf, minPts = 10)
res

plot(res)
abline(h = .001, col = "red")

res2 <- extractDBSCAN(res, eps_cl = .001)

data_out <- xdf %>%
  mutate(cluster = as.factor(ifelse(res2$cluster ==0, NA, res2$cluster))) 

ggplot( data_out, aes(x = Longitude, y= Latitude, color = cluster) ) +
  geom_point(  )+
  geom_line( data = data_out[!is.na(data_out$cluster),] )+
  labs(title = "Density Based Spatial Clustering", subtitle = paste0("date = ","; animal = ")) +
  theme_minimal()+
  coord_fixed()




###############
## AUTOMATED APPROACH #5
## IMPLEMENT Trend-Residual Dual Modeling for Detection of Outliers in Low-Cost GPS Trajectories
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5191017/pdf/sensors-16-02036.pdf
