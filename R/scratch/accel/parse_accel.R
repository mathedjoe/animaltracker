library(tidyverse)
library(lubridate)
library(sp)

accel_raw <- read_csv("R/scratch/accel/DATA-002.CSV", skip = 28) %>% 
  dplyr::rename(Time = ";Time") %>% 
  dplyr::mutate(Time = as.numeric(Time),
                real_time = strftime(lubridate::as_datetime(Time, tz = "UTC"), "%Y-%m-%d %H:%M:%OS3", tz = "UTC")) %>% 
  dplyr::mutate(dplyr::across(starts_with("A"), function(x) x/2048, .names = "{.col}_g"))

