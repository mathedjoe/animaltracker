# Animal Tracker

## Overview

This is a repository for an R package with data management and visualization tools for GPS and accelerometer data for tracking animals (e.g., cows). Includes two [Shiny Apps](https://shiny.rstudio.com/) to visualize and analyze the associated spatial-temporal data.

Please refer to the [PDF manual](https://github.com/mathedjoe/animaltracker/blob/master/animaltracker.pdf) for detailed information on function usage.

## Core Features

### Dynamic User Interface and Data

The Animal Tracker app ships with demo data included. Or, conveniently upload your own .csv or .txt animal data files in a zip folder. There is an option for the app to filter bad data points after files are uploaded. You can configure the filtration parameters and optionally cluster with a Kalman clustering algorithm.

![image](https://user-images.githubusercontent.com/5386960/118291424-723db580-b48c-11eb-9a31-00193f303aca.png)

By default, the elevation lookup is bounded by the maximum/minimum latitude and longitude determined from parsing the uploaded data, but the bounds can be modified manually. These bounds also impact the Kalman clustering from the filtration menu. Similarly, the zoom level defaults to the current map zoom, but can also be changed manually to values from 1-14. Click on the checkboxes to include slope and aspect with elevation.

![image](https://user-images.githubusercontent.com/37714689/70207613-86eb1b00-16e8-11ea-9d29-ee67a5fbaaea.png)

Use the dropdown selections to filter by site, animal, or date range. The app features will dynamically update based on your selections, and the filtered data is also available as a download.

![image](https://user-images.githubusercontent.com/37714689/70207658-b437c900-16e8-11ea-8e52-663f5b8cbc83.png)


### Dynamic Map

In addition, Animal Tracker can visualize the terrain from your animal data. Toggle between point and heatmap views, or overlay both. Draw a rectangle to select a particular region. 

![image](https://user-images.githubusercontent.com/37714689/70207764-ea754880-16e8-11ea-8277-3d1d17e4ae22.png)

![image](https://user-images.githubusercontent.com/37714689/70207817-0d9ff800-16e9-11ea-8766-6ef7ec09285c.png)


### Dynamic Plots

Animal Tracker also visualizes and compares your animals' behaviors. Plots include elevation over time by animal and number of points in the sample by animal as well as rate of travel by animal and total time spent by location (not shown).

![image](https://user-images.githubusercontent.com/37714689/52104155-602ef600-25a6-11e9-85ed-ec84b9712955.png)


### Dynamic Statistics

The app comes with a statistical summary utility to complement the visualizations. Choose between elevation, time difference between GPS measurements, course, course difference between GPS measurements, distance, and rate to summarize. Then, select from sample size, mean, median, standard deviation, variance, minimum value, maximum value, range, interquartile range, first quartile, and third quartile statistics. 

![image](https://user-images.githubusercontent.com/37714689/52104169-7d63c480-25a6-11e9-8060-c43f8359b3ae.png)

## Additional Features

### Validation App

Compare and visualize two datasets side-by-side with the validation app. Try the app with the included demo datasets, or upload your own .csv files (.txt is not yet supported).

![image](https://user-images.githubusercontent.com/37714689/70208596-3cb76900-16eb-11ea-8f9e-1009fe475b90.png)

Outlier detection can also be applied. We currently support modified z-score classification according to Iglewicz and Hoaglin (1993).

![image](https://user-images.githubusercontent.com/37714689/70208887-f3b3e480-16eb-11ea-9b1b-79cb525e0201.png)

Furthermore, summary statistics are provided for flagged data from the app's outlier detection or a method already applied to the data before upload.

![image](https://user-images.githubusercontent.com/37714689/70208924-0a5a3b80-16ec-11ea-8b75-b4c380e6615c.png)

## Usage

### Installation in R
This app can be installed and run through RStudio (or the basic R console).

#### devtools
Before installing Animal Tracker, ensure that you have the [devtools](https://github.com/r-lib/devtools) package installed and loaded:
```
install.packages("devtools") 
library(devtools)
```

#### animaltracker
Install and load animaltracker by running the following:
```
install_github("mathedjoe/animaltracker")
library(animaltracker) 
```
#### Core Shiny App
Launch the core shiny app by running the following :
```
run_shiny_animaltracker()
```
### Validation Shiny App
Launch the validation shiny app by running the following:
```
run_validation_app()
```

## Questions
Email [joechampion@boisestate.edu](mailto:joechampion@boisestate.edu).

## Contributors

* Sergio Arispe (lead researcher), Oregon State University, <sergio.arispe@oregonstate.edu>
* Joe Champion (lead developer), Boise State University, <joechampion@boisestate.edu>
* Thea Sukianto (student assistant), Boise State University, <TheophiliaSukian@u.boisestate.edu>
* Chithkala Dhulipati (student assistant), Boise State University, <chithkaladhulipa@u.boisestate.edu>
* Dylan Mikesell (researcher), Boise State University, <dylanmikesell@boisestate.edu>

