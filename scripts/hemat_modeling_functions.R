###################################
# Aidan Coyle, afcoyle@uw.edu
# Roberts Lab, UW-SAFS
# 2022-07-27
# File containing functions created during Hematodinium modeling project
###################################

#### read_tidbit_data(): Reads in all Tidbit data within a folder 

# Takes the following inputs:
#     data_path: Path to data folder for that specific survey or year. Don't include the last /

read_tidbit_data  <- function(data_path) {
  # Load all required libraries
  require(tidyverse)
  require(readxl)
  require(tools)
  
  # Get full path to all containing files
  tidbits <- list.files(path = data_path, full.names = TRUE, recursive = TRUE)
  
  # See if any non-Tidbit files in there. All non-Tidbit files have "_data" somewhere in the name
  if (any(grepl("_data", tidbits, fixed = TRUE)) == TRUE) {
    # If so, remove the non-Tidbit files
    tidbits <- tidbits[-grep("_data", tidbits, fixed = TRUE)]
  }
  
  # Extract year of survey
  get_year <- unlist(strsplit(data_path, split = '/', fixed = TRUE))[5]
  
  # Initialize matrix
  tidbit_data <- matrix(nrow = 0, ncol = 4)
  
  for (i in 1:length(tidbits)) {
    print(tidbits[i])
    print(i)
    
    #ID the file extension
    file_type <- file_ext(tidbits[i])
    
    # If file type is xls or xlsx, read in as excel
    if (file_type == "xls" | file_type == "xlsx") {
      new_tidbit <- read_excel(path = tidbits[i], col_names = TRUE, skip = 1)
      # Remove all columns aside from 2 and 3 (1 is rownames)
      new_tidbit <- new_tidbit %>%
        select(2,3)
      # Rename columns
      names(new_tidbit) <- c("DateTime", "Temp")
      # Split DateTime column
      suppressWarnings(
        new_tidbit <- new_tidbit %>%
        separate(DateTime, into = c("Date", "Time"), sep = " ")
      )
      # Drop rows with NAs
      new_tidbit <- na.omit(new_tidbit)
      
    } else if (file_type == "txt") {
      new_tidbit <- read.delim(file = tidbits[i], row.names = NULL)
      
    } else if (file_type == "csv") {
      # Find max number of columns in file
      max_cols <- max(count.fields(tidbits[i], sep = ","))
      # Read in file, prefilling col names
      new_tidbit <- read.csv(file = tidbits[i], col.names = c("Row", "DateTime", "Temp", rep("Trash", times = max_cols - 3)), skip = 2)
      # Remove all columns aside from 2 and 3 (1 is rownames)
      new_tidbit <- new_tidbit %>%
        select(c("DateTime", "Temp"))
      # Split DateTime column
      suppressWarnings(
        new_tidbit <- new_tidbit %>%
        separate(DateTime, into = c("Date", "Time"), sep = " ")
      )
      # Drop rows with NAs
      new_tidbit <- na.omit(new_tidbit)
      
    } else {
      print("Aidan says unknown file type!")
      break
    }
    
    # If first column was interpreted as the names of a row (thanks to misplaced spacing), edit
    if ("row.names" %in% names(new_tidbit)) {
      names(new_tidbit) <- c("Date", "Time", "Temp")
    } else {
    }
    
    # If last column was interpreted as two separate columns (thanks to misplaced spacing), remove last one
    # Remove all columns with only NAs
    new_tidbit <- new_tidbit[colSums(!is.na(new_tidbit)) > 0]
    # Remove all columns listed as "Junk"
    if ("Junk" %in% names(new_tidbit)) {
      new_tidbit <- new_tidbit %>%
        select(-Junk)
    }
    
    
    # If we have two columns, they're "DateTime" and "Temp"
    # If we have three, they're "Date", "Time", and "Temp"
    # If we have four, they're "Date", "Time", "Temp", and "Fahrenheit temp" (to be deleted)
    # Either way, standardize names of all columns
    if (length(names(new_tidbit)) == 2) {
      names(new_tidbit) <- c("tidbit_datetime", "Temp")
    } else if (length(names(new_tidbit)) == 3) {
      names(new_tidbit) <- c("Date", "Time", "Temp")
    } else if (length(names(new_tidbit)) == 5) {
    } else if (length(names(new_tidbit)) == 4) {
      names(new_tidbit) <- c("Date", "Time", "Temp", "Fahrenheit")
      new_tidbit <- new_tidbit %>%
        select(-Fahrenheit)
    } else {
      print("Aidan says error in columns!")
      break
    }
    
    # Get name of survey
    get_survey <- unlist(strsplit(tidbits[i], split = "/", fixed = TRUE))[6]
    
    # Extract ID from name of Tidbit file, removing ending
    get_id <- tail(strsplit(tidbits[i], split = "/", fixed = TRUE)[[1]], n = 1)
    get_id <- str_remove(get_id, paste0(".", file_type))
    
    # Add year, survey, and ID to the tidbit data column
    new_tidbit$year <- get_year
    new_tidbit$survey <- get_survey
    new_tidbit$tidbit_id <- get_id
    
    # Create datetime column from existing columns
    # Change Date and Time columns to correct object type
    
    # If the tidbit measures data to the second, it parses time differently
    # than those that measure data to the minute
    # Some are also stored as dates + times, which we just need to specify
    
    # First: if it uses datetime
    if ("tidbit_datetime" %in% names(new_tidbit)) {
      new_tidbit$tidbit_datetime <- mdy_hms(new_tidbit$tidbit_datetime, tz = "US/Alaska")
      # Next: if it uses AM/PM
    } else if (any(str_detect(new_tidbit$Time, " AM")) == TRUE) {
      new_tidbit$Date <- mdy(new_tidbit$Date, tz = "US/Alaska")
      new_tidbit$Time <- parse_time(new_tidbit$Time, "%I:%M:%S %p")
      new_tidbit$tidbit_datetime <- paste(new_tidbit$Date, new_tidbit$Time)
      new_tidbit <- new_tidbit %>%
        select(-c(Date, Time))  
      # If time is in minutes
    } else if (max(nchar(new_tidbit$Time)) < 6) {
      new_tidbit$Date <- mdy(new_tidbit$Date, tz = "US/Alaska")
      new_tidbit$Time <- parse_time(new_tidbit$Time, "%H:%M")
      new_tidbit$tidbit_datetime <- paste(new_tidbit$Date, new_tidbit$Time)
      new_tidbit <- new_tidbit %>%
        select(-c(Date, Time))  
      # If time is in seconds
    } else if (max(nchar(new_tidbit$Time)) < 18) {
      new_tidbit$Date <- mdy(new_tidbit$Date, tz = "US/Alaska")
      new_tidbit$Time <- parse_time(new_tidbit$Time, "%H:%M:%S")
      new_tidbit$tidbit_datetime <- paste(new_tidbit$Date, new_tidbit$Time)
      new_tidbit <- new_tidbit %>%
        select(-c(Date, Time))
    } else {
      print("Aidan says time setup broken!")
      break
    }
    
    # Standardize length of all columns
    new_tidbit <- new_tidbit[, 1:5]
    
    
    # Join new Tidbit info to full Tidbit dataframe
    tidbit_data <- rbind(tidbit_data, new_tidbit)
  }
  # Set the temperature column to numeric
  tidbit_data$Temp <- as.numeric(tidbit_data$Temp)
  
  # Return tidbit data
  tidbit_data
}


fix_long_csvs <- function(filepath) {

  issue <- read.csv(filepath)
  
  # Drop the unneeded column of Fahrenheit values
  issue <- issue %>%
    select(-Temperature....F.)
  
  # Split date and time columns up
  issue <- issue %>%
    separate(Date.Time, c("Date", "Time"), sep = " ")
  
  # Done! Just write it out
  write.table(issue, file = filepath,
              sep = "\t",
              row.names = FALSE)
  
}

# This quick function just splits apart tables
# where format is column 1 = date, column 2 = time,temp

fix_timetemp_comma <- function(filepath) {

  issue <- read.delim(file = filepath)
  
  # Split the time/temp column apart using the comma
  issue <- issue %>%
    separate(2, c("Time", "Temp"), sep = ",")
  
  # Done! Write it out
  write.table(issue, file = filepath,
              sep = "\t",
              row.names = FALSE)
}

# This quick function is meant to fix issues in the 2016 data 
# in which .txt files are reading in with too many columns

fix_longhead_txt <- function(filepath) {
  # Get maximum number of columns in file
  max_cols <- max(count.fields(filepath, sep = "\t"))
  
  # Read in file, prefilling column names
  issue <- read.delim(file = filepath, col.names = c("DateTime", "Temp", rep("Trash", times = max_cols - 2)))
  
  # Remove all columns aside from 1 and 2
  issue <- issue %>%
    select(c("DateTime", "Temp"))
  
  # Split the DateTime column
  suppressWarnings(
    issue <- issue %>%
    separate(DateTime, into = c("Date", "Time"), sep = " ")
  )
  
  # Write out file
  write.table(issue, file = filepath,
              sep = "\t",
              row.names = FALSE)
  
}


