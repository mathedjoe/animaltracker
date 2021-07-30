
## load required packages (if error, use install.packages("packagename"))
library("tidyverse")
library("RSQLite")
library("animaltracker")

# establish a connection to the database
con <- DBI::dbConnect(RSQLite::SQLite(),
                      "R/scratch/database/demo_ani_data.sqlite") # new database, stored in memory

meta <- data.frame( filename = c("file001.csv", "file002.csv"), 
                    dataset = c("igotu-file001", "columbus-file002"),
                    site = c("demo loc", "demo loc 2"), 
                    ani = c("Ani1", "Ani2"),
                    dtype = c("igotu", "columbus"),
                    minlat = c(0,50), 
                    maxlat =c(0,100))

# get some clean data
data_clean <- demo %>% 
  dplyr::mutate(dataset = "igotu-file001")

# save clean data to database
dbWriteTable(con, "data_clean", data_clean, overwrite = TRUE)

# save meta data to database
dbWriteTable(con, "meta", meta, overwrite = TRUE)

# some fake new data to append to the clean data
data_clean_new <- demo %>%
  dplyr::mutate(dataset = "columbus-file002")

dbWriteTable(con, "data_clean", data_clean_new, append = TRUE)

# list tables in the database
dbListTables(con)

df_tbl <- tbl(con, "data_clean")

str(df_tbl %>% as.data.frame)

# read data from local database
meta_df <- dbReadTable(con, "meta") %>% as.data.frame()

# basic query
tbl(con, "data_clean") %>% 
  filter(source == "app demo") %>% 
  collect()

df_now <- tbl(con, "data_clean") %>% 
  filter(Latitude > 43.28 & Latitude < 43.31 ) %>% 
  collect()

# basic query across two tables
srcs <- tbl(con, "meta") %>% 
  filter(ani == "Ani2") %>% 
  pull(dataset)

tbl(con, "data_clean") %>% 
  filter(dataset %in% srcs )

# mixed query across two tables
# dtype = "igotu" AND restricted Speed 

srcs <- tbl(con, "meta") %>% 
  filter(dtype == "igotu") %>% 
  pull(dataset)

df_filtered <- tbl(con, "data_clean") %>% 
  filter(dataset %in% srcs ) %>% 
  filter(Speed != 0 & Speed < 500 ) %>% 
  collect()
