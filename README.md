# Healthy_Ride_Rshiny_Application

This repository contains the code for a RShiny application that analyzes Healthy Ride Trip Data from the Western Pennsylvania Regional Data Center. Only a handful of quarters were selected from the data to be looked at. The data used for this application can be found here:

https://data.wprdc.org/dataset/healthyride-trip-data

This repository has three main components:

### `app.R`

This R file contains the actual structure and feature of the apps. It calls the `data_cleaning.R` file at the start. It also loads all packages used within the application itself.

### `data_cleaning.R`

This R file just contains a few data cleaning scripts that need to be taken care of before the application is run. Its main function is to combine all of the downloaded CSVs into one big dataframe to be analysed. Missing values and improper data types are also dealt with.

### `healthy_ride_data`

This directory contains several CSV files to be loaded in an combined. Their names were changed after initial download in order to be uniform with one another.
