###
# DATA STORAGE PROCESS
# 1. new data is uploaded through the app 
#   (user uploads files (maybe in a zip folder) or points to a folder on their computer)
# 2. clean the data one file at a time
# 3. save meta data
#     (fileid, filename, site, date min/max, cows, min/max lat/longitude, storage location (rds))
# 4. cleaned data is stored in an rds (e.g., 50 files of data per rds)
# 5. cache.rds keeps track of recent data frames used by the shiny app
# 6. current.df is the current set of data used by the shiny app
#
# functions:
#    clean_batch
#       - clean_df
#       - get_meta
#       - save_meta
#    get_data_from_meta
#       - options to select days/times/cows/sites
#       - check if cached
#    save_to_cache
#   
# the shiny app needs to use some of these functions
