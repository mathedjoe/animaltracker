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

