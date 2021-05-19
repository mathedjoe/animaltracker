# animaltracker 0.2.0

## Major changes

* Improved custom elevation lookup through AWS DEM (no longer RAM-intensive)

* Enabled fencing upload (.kmz) in Shiny app

* Enabled elevation lookup for demo data in app

* Enabled time-based data filtering in app

* Revamped app UI to be more user-friendly

* default `maxtime` parameter in `clean_location_data` is increased to 3600

* `V8` package not imported starting this update

## Bug fixes

* Uploading data through app no longer generates duplicate columns when columns already exist in data

* Fixed geodesic distance calculation

* Updated app polygon filtering to be compatible with newest version of `shinyjs`

* Updated analysis functions to be compatible with newest version of `dplyr`

* Omitted plotting (0, 0) latitude/longitude in app