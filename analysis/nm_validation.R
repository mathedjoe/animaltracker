correct <- read.csv("test_data/newmexico_validate/nm_validate_correct.csv", skipNul = T, stringsAsFactors = F)
candidate <- read.csv("test_data/newmexico_validate/nm_validate_candidate.csv", skipNul = T, stringsAsFactors = F)

# reformat correct

correct <- correct %>% 
  dplyr::rename(TimeDiff = timedifference,
                TimeDiffMins = timedifference.in.minutes,
                CourseDiff = coursedifference,
                DistanceFlag = DistFlag,
                GPS = Cow) %>% 
  dplyr::mutate(DateTime = lubridate::with_tz(as.POSIXct(paste(Date, Time), format="%m/%d/%Y %H:%M:%S", tz="MST"), tz="UTC"),
                Time = strftime(DateTime, format="%H:%M:%S", tz="UTC"),
                Date = as.Date(DateTime))


# as.POSIXct(paste(Date, Time), format="%m/%d/%Y %H:%M:%S")

# reformat candidate

candidate <- candidate %>% 
  dplyr::mutate(GPS = as.integer(gsub("(.*)(\\d{3})(.*)", "\\2", GPS)),
                DateTime = as.POSIXct(DateTime, tz="UTC"))


comparison <- compare_flags(correct, candidate, use_elev = FALSE, use_slope = FALSE, has_flags = TRUE, Keep)

# merge datasets while also looking up time zones from geo coords
setwd("R/scratch/nm_validate")
df_candidate <- read.csv("nm_validate_candidate.csv", stringsAsFactors = FALSE)
df_correct <- read.csv("nm_validate_correct.csv", stringsAsFactors = FALSE)

# add DateTime column to df_correct
## not easy! need to replace #VALUE! with NA, and add leading 0s to both day of month and hour
## once Date and Time are formatted correctly, can paste them together and coerce to datetime via as.POSIXlt and a format like
## format = "%d/%m/%Y %H/%M/%S %p"... see   https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html  

# install.packages("lutz")
library("lutz")

compute_tz_offsets <- function(df){
  
  # lookup timezone by lat/long
  tzs <- tz_lookup_coords(df$Latitude, df$Longitude, method = "accurate")
  
  # set-up a progress bar
  pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
  
  # compute timezone offset for each row in the data
  tz_offsets <- sapply(1:nrow(df), function(i){
    
    # occasionally update the progress bar
    if(i %% 1000 == 0){
      setTxtProgressBar(pb, i)
    }
    return( tz_offset(df$DateTime[i], tzs[i])$utc_offset_h )
    
  })
  close(pb)
  return(tz_offsets)  
}

tz_offsets_candidate <- compute_tz_offsets(df_candidate) # takes awhile

tz_offsets_correct <- compute_tz_offsets(df_correct) # takes awhile



candidate <- read.csv("analysis/df_candidate.csv", stringsAsFactors = FALSE)
correct <- read.csv("analysis/df_correct.csv", stringsAsFactors = FALSE)

candidate <- candidate %>% 
  dplyr::mutate(DateTime = as.POSIXct(DateTime, tz="UTC"))
 
correct <- correct %>% 
  dplyr::mutate(tz = ifelse(tz_offset < 0, paste0("Etc/GMT+", abs(tz_offset)), "UTC")) %>% 
  dplyr::group_by(tz) %>% 
  dplyr::mutate(DateTime = as.POSIXct(DateTime, tz=tz[1]),
                TimeDiffMins = timedifference.in.minutes)

join <- dplyr::full_join(correct, candidate, by=c("Cow", "Latitude", "Longitude", "EHPE"))

# ~82000 of tz_offset -7 are unmatched.
# ~2000 of tz_offset -6 are unmatched.

#nm_meta <- animaltracker:::store_batch_list(list(datapath="test_data/newmexico_validate/nm_validate.zip", name="nm_validate.zip"))
#nm_meta$data[[7]] <- nm_meta$data[[7]] %>% dplyr::mutate(Order = as.numeric(Order),
                                                         #Index = as.numeric(Index),
                                                         #Altitude = as.numeric(Altitude),
                                                         #Speed = as.numeric(Speed),
                                                         #Timeout = as.numeric(Timeout),
                                                         #SleepTime = as.numeric(SleepTime),
                                                         #EHPE = as.numeric(EHPE),
                                                         #Satelite.ID = as.numeric(Satelite.ID))
#candidate_raw <- dplyr::bind_rows(nm_meta$data) %>% 
  #dplyr::filter(!is.na(DateTime))
