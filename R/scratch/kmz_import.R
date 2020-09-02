library(maptools)

kmz_coords <- getKMLcoordinates(kmlfile = unzip(zipfile = "test_data/PastureFiles/McIntyre Pasture.kmz", 
                                                   exdir = "test_data/PastureFiles"))
