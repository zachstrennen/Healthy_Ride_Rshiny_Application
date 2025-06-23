library(tools)
library(dplyr)

# Path to dat directory
csv_dir <- "healthy_ride_data"

# Get names of all CSV files in the directory
csv_files <- list.files(path = csv_dir, pattern = "\\.csv$", full.names = TRUE)

# Read all CSVs into a list
csv_list <- lapply(csv_files, function(file) read.csv(file, stringsAsFactors = FALSE))

# Function to read a CSV and clean its column names
read_and_clean <- function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  # Make all column names uniform given discrepancies
  names(df) <- tolower(gsub(" ", "_", names(df)))
  names(df) <- gsub("\\.", "_", names(df))
  # Add column for quarter depending on file name
  df$quarter <- file_path_sans_ext(basename(file))
  return(df)
}

# Bind the rows of all the data
data <- do.call(bind_rows, lapply(csv_files, read_and_clean))

# Initial datetime conversion
data$starttime <- as.POSIXct(data$starttime, format = "%m/%d/%Y %H:%M")
data$starttime <- as.POSIXct(data$stoptime, format = "%m/%d/%Y %H:%M")

# Omit missing rows
data <- na.omit(data)

# datetime conversion for easier RShiny handling
data$starttime <- as.POSIXct(data$starttime, format = "%Y-%m-%d %H:%M:%S")
data$stoptime <- as.POSIXct(data$stoptime, format = "%Y-%m-%d %H:%M:%S")
