# # Creating a new database - provide the filename to dbConnect()
#
# 
# anidata <- readRDS("data/animal_data_with_elev.rds") 
# animaldb <- dbConnect (RSQLite::SQLite(), dbname = "animaldb.sqlite")
# gps_meta <- data.frame(id = 1:3,
#                       site = c("47 ranch", "47 ranch", "3 fingers"),
#                       filename = paste0(letters[1:3], ".csv")
# )
# 
# names(anidata)
# 
# for (dataname in names(anidata)){
#   
#   dplyr::copy_to(animaldb, anidata[[dataname]], name = dataname)
# }
# 
# 
# mydata <- dplyr::tbl(animaldb, "cow1149")
# library(dbplyr)
# library(dplyr)
# 
# animaldb %>% 
#   select(Cow)


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
