# Animal Tracker

## Overview

This is a repository for an R package with data management and visualization tools for GPS and accelerometer data for tracking animals (e.g., cows). Includes a [Shiny App](https://shiny.rstudio.com/) to visualize and analyze the associated spatial-temporal data.

## Features

### Dynamic User Interface and Data

The Animal Tracker app ships with demo data included. Or, conveniently upload your own .csv animal data files in a zip folder. Use the dropdown selections to filter by site, animal, or date range. The app features will automatically update based on your selections, and the filtered data is also available as a download.

![image](https://user-images.githubusercontent.com/37714689/52103856-0974ec80-25a5-11e9-8939-c7dc2cddaf68.png)

### Dynamic Map

In addition, Animal Tracker can visualize the terrain from your animal data. Toggle between point and heatmap views, or overlay both. Draw a rectangle to select a particular region. 

![image](https://user-images.githubusercontent.com/37714689/52103747-7340c680-25a4-11e9-925f-bcb08e70e584.png)

### Dynamic Plots

Animal Tracker also visualizes and compares your animals' behaviors. Plots include elevation over time by animal and number of points in the sample by animal as well as rate of travel by animal and total time spent by location (not shown).

![image](https://user-images.githubusercontent.com/37714689/52104155-602ef600-25a6-11e9-85ed-ec84b9712955.png)

### Dynamic Statistics

The app comes with a statistical summary utility to complement the visualizations. Choose between elevation, time difference between GPS measurements, course, course difference between GPS measurements, distance, and rate to summarize. Then, select from sample size, mean, median, standard deviation, variance, minimum value, maximum value, range, interquartile range, first quartile, and third quartile statistics. 

![image](https://user-images.githubusercontent.com/37714689/52104169-7d63c480-25a6-11e9-8060-c43f8359b3ae.png)



## Usage

### Installation in R
This app can be installed and run through RStudio (or the basic R console).

#### devtools
Before installing Animal Tracker, ensure that you have the [devtools](https://github.com/r-lib/devtools) package installed and loaded:
```
install.packages(devtools) 
library(devtools)
```

#### animaltracker
Install and load animaltracker by running the following:
```
install_github(“mathedjoe/animaltracker”)
library(animaltracker) 
```
#### Shiny App
Launch the shiny app by running the following :
```
run_shiny_animaltracker()
```
## Questions
Email [joechampion@boisestate.edu](mailto:joechampion@boisestate.edu).

## Contributers

* Sergio Arispe (lead researcher), Oregon State University, <sergio.arispe@oregonstate.edu>
* Joe Champion (lead developer), Boise State University, <joechampion@boisestate.edu>
* Thea Sukianto (student assistant), Boise State University, <TheophiliaSukian@u.boisestate.edu>
* Chithkala Dhulipati (student assistant), Boise State University, <chithkaladhulipa@u.boisestate.edu>
* Dylan Mikesell (researcher), Boise State University, <dylanmikesell@boisestate.edu>

