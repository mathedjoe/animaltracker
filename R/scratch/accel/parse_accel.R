library(tidyverse)
library(lubridate)
library(sp)
library(zoo)
# library(data.table)
  
### READ INTEGRATED ACCEL/GPS DATA FROM STANDARDIZED FORMAT
# metadata is always 10 lines

read_accel <- function(filename) {
  sat_line <- colnames(read_csv(filename, skip = 8, n_max = 0)) # jump to the 9th line
  num_sat <- as.numeric(sat_line[length(sat_line)]) # get number of satellites
  # there is always 1 extra metadata line after satellite list
  accel_raw <- read_csv(filename, skip = 10 + num_sat + 1) %>% 
    dplyr::rename(Time = ";Time") %>% 
    dplyr::mutate(Time = as.numeric(Time),
                  real_time = strftime(lubridate::as_datetime(Time, tz = "UTC"), "%Y-%m-%d %H:%M:%OS3", tz = "UTC")) %>% 
    dplyr::mutate(dplyr::across(starts_with("A"), function(x) x/2048, .names = "{.col}_g"))
  
  accel_raw %>% filter(!is.na(Lat))
  
  # do linear interpolation on coordinates. rule = 2 means that trailing NAs are filled with the endpoints
  accel_raw$Lat_fill <- zoo::na.approx(accel_raw$Lat, na.rm = FALSE, rule = 2) 
  accel_raw$Lon_fill <- zoo::na.approx(accel_raw$Lon, na.rm = FALSE, rule = 2)
  
  accel_reformat <- accel_raw %>% 
    dplyr::rename(Longitude = Lon_fill,
                  Latitude = Lat_fill,
                  LonRaw = Lon,
                  LatRaw = Lat,
                  DateTime = real_time) %>% 
    dplyr::mutate(Time = strftime(DateTime, format="%H:%M:%OS3", tz="UTC"),
                  Date = strftime(DateTime, format="%Y-%m-%d", tz="UTC"),
                  Course = calc_bearing(dplyr::lag(Latitude, 1, default = first(Latitude)), dplyr::lag(Longitude, 1, default = first(Longitude)), Latitude, Longitude),
                  Distance = geosphere::distGeo(cbind(Longitude, Latitude), 
                                                cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) ))))

return(accel_reformat)
}
### READ TIMESTAMPED BEHAVIOR DATA FROM XLSM SPREADSHEET
test_behavior_file <- list.files("test_data/test_accel", pattern = ".xlsm", full.names=TRUE)[1]

behavior_sheets <- readxl::excel_sheets(test_behavior_file)

custom_name_repair <- function(name_text){ifelse(name_text =="", "XX", tolower(gsub("\\ ", "_", name_text)))}

# behavior_raw_1sheet <-   readxl::read_excel(test_behavior_file, 
#                                             sheet = behavior_sheets[3],
#                                             range = readxl::cell_cols(c(1:31)),
#                                             col_types = "text", 
#                                             .name_repair = custom_name_repair) %>% 
#   dplyr::select(-XX) %>% 
#   type.convert() 
# 
# behavior_raw_1sheet 

behavior_raw <- lapply(behavior_sheets, function(sheet_name){
  readxl::read_excel(test_behavior_file, 
                     sheet = sheet_name,
                     range = readxl::cell_cols(c(1:31)),
                     col_types = "text", 
                     .name_repair = custom_name_repair) %>% 
    dplyr::select(-XX) %>%
    # drop any rows that are all NA (except cowid)
    filter(if_any(setdiff(everything(),one_of("cowid")) , ~ !is.na(.)))
  }) %>% 
  dplyr::bind_rows() %>% 
  type_convert()

# Sprinkle's conversion formula from meeting notes: unix/86500 - 0.25 + 25569 = excel
reformat_behavior<- function(data_raw, tz = "Etc/GMT-6", timeout = 300){
  data_raw %>% 
    select(-contains("elapsed"), -contains("bites")) %>%
    tidyr::pivot_longer(drinking:walking, names_to = "behavior") %>% 
    dplyr::filter(!is.na(value)) %>% 
    dplyr::select(-value) %>%
    dplyr::rename(DateTime = timestamp) %>%
    dplyr::mutate(
      DateTime = lubridate::as_datetime((DateTime - 25569)*86400),
      DateTime = lubridate::force_tz(DateTime, tz),
      Date = format(strptime(DateTime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d"),
      Time = format(strptime(DateTime, format="%Y-%m-%d %H:%M:%S"), format="%H:%M:%S")
    ) %>%
    dplyr::arrange(DateTime) %>%
    dplyr::group_by(cowid, DateTime) %>%
    dplyr::mutate(timediff_hs = 1/(n()) ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(cowid) %>%
    dplyr::mutate(
                  timediff = ifelse(DateTime != dplyr::lag(DateTime),
                                      as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="secs")),
                                      timediff_hs),
                  timediff = ifelse(is.na(timediff), 0, timediff),
                  behavior_id = (behavior != lag(behavior, 1, default = "") | timediff > timeout ),
                  behavior_id = cumsum(behavior_id)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(cowid, behavior_id) %>%
    dplyr::mutate(
      behavior_time = cumsum(timediff ) - first(timediff) + first(timediff_hs) ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cowid, DateTime)
}



# %>%
#   dplyr::arrange(cowid) %>% 
#   dplyr::group_by(DateTime, cowid, behavior) %>% 
#   dplyr::mutate(time_elapsed = (row_number()-1)*(1/n())) %>%
#   dplyr::ungroup()

behavior_formatted <- reformat_behavior(behavior_raw)


# testdf <- behavior_formatted %>% 
#   group_by(cowid) %>%
#   arrange(DateTime) %>%
#   mutate(behavior_switch = behavior != dplyr::lag(behavior) )

# list of unique behaviors, with start_time and time_elapsed
behavior_formatted %>% 
  group_by(cowid, behavior_id, behavior) %>% 
  summarize(
    start_time = first(DateTime),
    time_elapsed = last(behavior_time)  )

data_18 <- read_accel("test_data/test_accel/DATA-018.CSV")
data_17 <- read_accel("test_data/test_accel/DATA-017.CSV")
data_0519 <- dplyr::bind_rows(data_17, data_18)

behavior_reformat <- behavior_reformat %>% 
  dplyr::mutate(DateTime_estimate = as_datetime(DateTime + seconds(time_elapsed)))


