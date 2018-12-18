# # Creating a new database - provide the filename to dbConnect()
#
# animaldb <- dbConnect (RSQLite::SQLite(), dbname = "animaldb.sqlite")
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
