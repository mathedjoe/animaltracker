# merge datasets while also looking up time zones from geo coords
setwd("R/scratch/nm_validate")
library("dplyr")
df_candidate <- read.csv("nm_validate_candidate.csv", stringsAsFactors = FALSE)
df_correct <- read.csv("nm_validate_correct.csv", stringsAsFactors = FALSE, na.strings = c("", "#VALUE!"))

# add DateTime column to df_correct
## not easy! need to replace #VALUE! with NA, and add leading 0s to both day of month and hour
## once Date and Time are formatted correctly, can paste them together and coerce to datetime via as.POSIXlt and a format like
## format = "%d/%m/%Y %H/%M/%S %p"... see   https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html  

df_correct <- df_correct %>%
  mutate( DateTime = as.POSIXct(paste(Date, Time), format= "%m/%d/%Y %H:%M:%S %p")) %>%
  filter(!is.na(DateTime))


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

df_candidate <- cbind(df_candidate, tz_offset = tz_offsets_candidate) %>%
  filter(!is.na(tz_offset))

df_candidate <- df_candidate %>%
  mutate(Cow = as.numeric( gsub("(.*DW\\_)(\\d+)(\\_.*)","\\2", Animal)) )

df_correct <- cbind(df_correct, tz_offset = tz_offsets_correct)

df_candidate %>% 
  mutate(DateTime = as.POSIXct(DateTime)) %>%
  arrange(Cow, DateTime) %>%
  write.csv("df_candidate.csv")

df_correct %>%
  arrange(Cow, DateTime) %>%
  write.csv("df_correct.csv")

df_merged <- full_join(df_candidate %>% mutate(DateTime = as.POSIXct(DateTime)), df_correct, by = c("DateTime", "Cow"))


df_correct2 <- df_correct %>%
  mutate(DateTime = lubridate::with_tz(lubridate::force_tz(DateTime, tz = "Etc/GMT+7"), tzone = "UTC"))

df_candidate2<- df_candidate %>% 
  mutate(DateTime = as.POSIXct(DateTime, tz = "UTC"))

df_merged <- full_join(df_candidate2, df_correct2, by = c("Cow", "Index"))

table(df_merged$DateTime.x == df_merged$DateTime.y)

df_merged %>% 
  filter(Latitude.x != Latitude.y) %>% 
  select(Cow, Index, DateTime.x, DateTime.y, Latitude.x, Latitude.y, Longitude.x, Longitude.y) %>%
  View
