# # SCRATCH CODE TO WORK OUT SAVING/GETTING GPS DATA FROM A DATABASE
#
# 
library(dbplyr)
library(dplyr)


anidata <- readRDS("data/animal_data_with_elev.rds")

# save gps data to an sqlite database (optionally (re)create the database)
save_gps_to_database<- function(gpsdata, datasite, dbfile, overwrite = TRUE){
  
  if(overwrite & file.exists(dbfile)){
    unlink(dbfile)
  }
  # connect to the database
  db <- dbConnect (RSQLite::SQLite(), 
                        dbname = dbfile)
  
  # discard any empty data frames from gpsdata
  gpsdata<- gpsdata[sapply(gpsdata, function(df)nrow(df)>0)]
  
  # insert gps data to the database
  for (dataname in names(gpsdata)){
    
    # get the data frame

      df<- gpsdata[[dataname]] %>% 
              mutate(data_site = datasite,      # add the data site
                     data_src = dataname        # add the data source
                     ) 
      
      # save to database
      RSQLite::dbWriteTable(db, "gps", df, append=TRUE, row.names = FALSE)
    
  
  }
  writeLines(paste("wrote the following data to the", dbListTables(db), "table in", dbfile, ":") )
  print(noquote(names(gpsdata)  ))
  # close the database connection
  dbDisconnect(db)
  
}
save_gps_to_database (anidata, "initial sample", "data/animaldb.sqlite")


# retrieve gps data from an sqlite database 
# (uses a list of opts to specify the query)
get_gps_data<- function(dbfile, query = 'SELECT * FROM gps LIMIT 100'){
  
  # connect to the database
  db <- dbConnect (RSQLite::SQLite(), 
                   dbname = dbfile)
  
  df <- dbGetQuery(db, 
                   query)
  return(df)
  
}

df<- get_gps_data(dbfile = "data/animaldb.sqlite", 
             query = 'SELECT "data_src", "Cow","GPS" FROM gps WHERE "data_src" == "cow1149"
             ')

summary(df)
get_gps_data(dbfile = "data/animaldb.sqlite", query = 'SELECT * FROM gps LIMIT 5')

## save all gps data files to the animaldb in a gps table
# 
# 
# gps_meta <- data.frame(id = 1:3,
#                        site = c("47 ranch", "47 ranch", "3 fingers"),
#                        filename = paste0(letters[1:3], ".csv")
# )

# 
# 
# mydata <- dplyr::tbl(animaldb, "cow1149")

# 
# animaldb %>% 
#   select(Cow)

######################
# OLD DATA
#
# # Loading data
#
# animaldb <- dbSendQuery(conn = animaldb,
#                       "CREATE TABLE MetaData   # The name of the table is MetaData
#                       (File NULL,              # Not sure about the data type of file, so named it NULL
#                       Animal CHAR,            # The column/ field named Animal - is it the collar id, or some text?
#                       Date DATETIME)")         # DATETIME is the data type for Date field.
#
# animaldb <- dbSendQuery(conn = animaldb,
#                       "CREATE TABLE Data
#                       Files...") # Yet to complete the code!
#
# # List tables in the database
# dbListTables(mydb)
