library(tidyverse)
library(lubridate)
library(sp)
library(zoo)
  
### READ INTEGRATED ACCEL/GPS DATA FROM STANDARDIZED FORMAT
# metadata is always 10 lines
sat_line <- colnames(read_csv("test_data/test_accel/DATA-001.CSV", skip = 8, n_max = 0)) # jump to the 9th line
num_sat <- as.numeric(sat_line[length(sat_line)]) # get number of satellites
# there is always 1 extra metadata line after satellite list
accel_raw <- read_csv("test_data/test_accel/DATA-001.CSV", skip = 10 + num_sat + 1) %>% 
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

### READ TIMESTAMPED BEHAVIOR DATA FROM XLSM SPREADSHEET
test_behavior_file <- list.files("test_data/test_accel", pattern = ".xlsm", full.names=TRUE)[1]

behavior_sheets <- readxl::excel_sheets(test_behavior_file)

custom_name_repair <- function(name_text){ifelse(name_text =="", "XX", tolower(gsub("\\ ", "_", name_text)))}

behavior_raw_1sheet <-   readxl::read_excel(test_behavior_file, 
                                            sheet = behavior_sheets[3],
                                            range = readxl::cell_cols(c(1:31)),
                                            col_types = "text", 
                                            .name_repair = custom_name_repair) %>% 
  dplyr::select(-XX) %>% 
  type.convert() 

behavior_raw_1sheet 

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


behavior_long <- behavior_raw %>% 
  pivot_longer(drinking:walking, names_to = "behavior") %>% 
  filter(!is.na(value)) %>% 
  select(-value) %>% 
  pivot_longer(contains("elapsed"), values_to = "time_elapsed") %>% 
  select(-name)


