## R Script to Bulk Process Cow Accelerometer Data using a Random Forest Behavior Classification Model
# Contact: joechampion@boisestate.edu
# Created: 2018/08/16

## system configuration (fix as needed for your data locations)
import_data_folder_parent <- "//cifs-prd-01/Research/ESLab/Data_02/Rock_Creek_Ranch/DATA/day_files"
export_data_folder_parent <- "//cifs-prd-01/Research/ESLab/Data_02/Rock_Creek_Ranch/DATA/"
export_data_folder_name <- "processed_RFmodel_by_day"


## load necessary packages (if get error(s), run the command: install.packages( "missingpackage" )   )
library(tidyr)
library(randomForest)

## get the random forest model (from training data, see explore.R)
# cow11_rf 
load("//cifs-prd-01/Research/ESLab/Data_02/Rock_Creek_Ranch/RFModeling/cow11_rf.RData")



## get data
data_folders <- list.files(import_data_folder_parent, full.names=TRUE)

data_name_pattern <- "(.*)(Cow_\\d{1,})(.*)" # e.g., "XXX...Cow_1...XXX" or "XXX...Cow_12...XXX"
data_name_pattern_toclean <- "\\2"
data_names_clean <- gsub(data_name_pattern, data_name_pattern_toclean, data_folders)

## process raw data for applying the model
# library to read matlab data formats into R
library(R.matlab)

daily_data_files <- list.files(data_folders[1], pattern=".mat", full.names=TRUE)
tday_data <- readMat(daily_data_files[1]) %>% as.data.frame

# transform processed new data for prediction using the RF model
elapsed <- 1/25 # (sampling rate is 25 Hz)
g_const <- -9.8
day_data <- tday_data %>% 
    dplyr::select( datetime = t, X = x, Y = y, Z = z
    ) %>%
    dplyr::mutate(
        datetime = as.POSIXct((datetime - 719529)*86400, origin = "1970-01-01", tz = "UTC"),  # 719529 is ‘1970-01-01 00:00.00 UTC’ in Matlab's datenum and 86400 the number of seconds in an standard UTC day
        X = X/g_const,
        Y = Y/g_const,
        Z = Z/g_const - 2, #to get between 0 and 1
        XY.Z = X*Y+Z,  
        lnXY.Z = log(abs(X*Y)) + Z,
        XZ = X*Z,
        lnXZ=	log(abs(X))*Z,
        X.Y.Z = X+Y+Z,
        lnX.Y.Z = log(abs(X))+Y+Z,
        lnZ = log(abs(Z)),
        tiltX = asin(X),
        tiltY = asin(Y),
        tiltZ = acos(Z),
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
    na.omit  # omit any missing data (there's not much)
    

## apply random forest model to processed data
tpreds <- predict(cow11_rf, cow11 %>% dplyr::select(X:tdispZ))

tpreds <- predict(cow11_rf, day_data %>% dplyr::select(-datetime, -Observed))
    







## export data
export_data_folder <- file.path(export_data_folder_parent, export_data_folder_name)
if(!dir.exists(export_data_folder) ){
    dir.create(export_data_folder)
}