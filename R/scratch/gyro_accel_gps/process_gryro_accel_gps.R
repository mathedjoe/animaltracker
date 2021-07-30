library(tidyverse)
library(devtools)
load_all() # load the animaltracker library

unzip("R/scratch/gyro_accel_gps/sample_file.zip", exdir = "R/scratch/gyro_accel_gps")
testfile <- "R/scratch/gyro_accel_gps/FILE0003.002"

data_row1 <- readLines(testfile, 1, skipNul = TRUE)

### FUNCTION FOR FILLING NA values with non-NA values ABOVE (or below)
## example: a device occasionally records latitude/longitude, and we assume the value is the same in between
fill_na_downup <- function(x){
  xout <- zoo::na.locf(x, na.rm=FALSE)   # fill body NA's with non-NA values FROM ABOVE
  zoo::na.locf(xout, fromLast = TRUE) # fill leading NA's with non-NA values FROM BELOW
}

## CONDITION FOR IDENTIFYING THIS DATA TYPE 
# first row starts with #Format
grepl("^#Format", data_row1)


  

### READ AND PREPARE ONE DATA FILE OF KNOWN TYPE



df_test <- read_prep_onefile(testfile)$df


write.csv(df_test, "R/scratch/gyro_accel_gps/test_processed_intgag.csv", row.names=FALSE)
