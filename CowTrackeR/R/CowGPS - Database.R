
# Creating a new database - provide the filename to dbConnect()

CowGPS <- dbConnect (RSQLite::SQLite(), dbname = "CowGPS.sqlite")

# Loading data

CowGPS <- dbSendQuery(conn = CowGPS,
                    "CREATE TABLE MetaData   # The name of the table is MetaData
                    (File NULL,              # Not sure about the data type of file, so named it NULL
                    Cow INTEGER?,            # The column/ field named cow - is it the collar id, or some text?
                    Date DATETIME)")         # DATETIME is the data type for Date field.

CowGPS <- dbSendQuery(conn = CowGPS,
                      "CREATE TABLE Data
                      Files...") # Yet to complete the code!

# List tables in the database
dbListTables(mydb)


