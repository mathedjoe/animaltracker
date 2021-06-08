library(tidyverse)
library(caret) # for computing cross-validation
library(ranger)
library(vip)
library(lubridate)

load("R/scratch/accel/supervised/model_rf_parsy.rda")

model_rf_parsy
confusionMatrix(model_rf_parsy)
cow_accel_file_all <- list.files("R/scratch/accel/rf_2018/", pattern="All Cows", full.names = TRUE)
cow_accel_raw_all <- readxl::read_excel(cow_accel_file_all)

clean_accel_data <- function(data){
  # before using, prep the data to format: datetime, X, Y, Z, Behavior
  data %>% 
    dplyr::mutate(
      elapsed = lubridate::seconds(datetime - dplyr::lag(datetime)),
      elapsed = as.numeric(gsub("S", "", elapsed)),
      elapsed = replace_na(elapsed, 0),
      XY.Z = X*Y+Z,  
      XZ = X*Z,
      X.Y.Z = X+Y+Z,
      norm_accel = sqrt(X^2+Y^2+Z^2),
      tiltX = asin(X/norm_accel),
      tiltY = asin(Y/norm_accel),
      tiltZ = acos(Z/norm_accel),
      pitch = atan(X/(Y^2+Z^2)),
      roll = atan(Y/(X^2+Z^2)),
      yaw = atan(Z /(X^2+Y^2)),
      jerkX = (X-dplyr::lag(X))/elapsed, #meters/sec^3
      jerkY = (Y -dplyr::lag(Y))/elapsed,
      jerkZ = (Z-dplyr::lag(Z))/elapsed,
      tvelX = (cumsum(X) - dplyr::first(X))*elapsed, #meters/sec
      tvelY = (cumsum(Y) - dplyr::first(Y))*elapsed,
      tvelZ = (cumsum(Z) - dplyr::first(Z))*elapsed,
      tdispX = cumsum(tvelX)*elapsed, #meters
      tdispY = cumsum(tvelY)*elapsed,
      tdispZ = cumsum(tvelZ)*elapsed
    ) %>% 
    mutate(across(starts_with("jerk"), function(x){ replace_na(x,0)}) ) %>% 
    filter(abs(elapsed) < 60, !is.infinite(jerkX))
}

cow_accel_all <- cow_accel_raw_all %>% 
  dplyr::rename(Behavior = Observed) %>% 
  dplyr::mutate(Behavior = as.factor(Behavior),
                Time = format(strptime(Time, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S"),
                datetime = as.POSIXct(paste(Date, Time))) %>% 
  dplyr::group_by(datetime, COWID) %>% 
  dplyr::mutate(Offset = row_number()-n()-1,
                datetime = datetime + minutes(1) + seconds(5*Offset)) %>% 
  dplyr::filter(!duplicated(datetime)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(COWID, datetime, Behavior, X, Y, Z) %>% 
  clean_accel_data()

cow_accel_plus <- cow_accel_all %>% 
  modelr::add_predictions(model_rf_parsy) %>% 
  mutate(SMAMAX = abs(X)+abs(Y)+abs(Z))

cow_accel_plus %>% 
  ggplot(aes(x = SMAMAX, group = pred, color = pred )) +
  geom_density()+
  geom_vline(xintercept = c(1.809, 1.642, 2.344))+
  theme_minimal()+
  facet_wrap(vars(COWID))

# SMAMAX was < 1.809 = resting (n = 13; SE = ± 0.016  ) and < 2.405 for grazing (n = 15; SE = ± 0.088). The average SMAMAX values (n = 7)  for collar mounted accelerometers was SMAMAX < 1.642 = resting (SE = ± 0.030) and < 2.344 for grazing (SE = ± 0.020).  I did have to eliminate 3 halters that had SMAMAX values at ≈ < 3.6 for resting and ≈ < 4.3 for grazing.

cow_accel_plus %>% 
  ggplot(aes(x = norm_accel, group = pred, color = pred )) +
  geom_density()+
  # geom_vline(xintercept = c(1.809, 1.642, 2.344))+
  theme_minimal()+
  facet_wrap(vars(COWID))
