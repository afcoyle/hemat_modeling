###################################
# Aspen Coyle, afcoyle@uw.edu
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
  
  # See if any non-Tidbit files in there. All non-Tidbit files have "_data" somewhere in the name. If so, remove them.
  # Unfortunately, so does our filepath, so we'll search for either files with "_data." or "_data_"
  # And since our formatting doesn't work if none are TRUE, we gotta do it separately
  if (any(grepl("_data.", tidbits, fixed = TRUE)) == TRUE){
    tidbits <- tidbits[-grep("_data.", tidbits, fixed = TRUE)]
  }
  if (any(grepl("_data_", tidbits, fixed = TRUE)) == TRUE){
    tidbits <- tidbits[-grep("_data_", tidbits, fixed = TRUE)]
  }
  
  
  
  # Extract year of survey
  get_year <- unlist(strsplit(data_path, split = '/', fixed = TRUE))[6]
  
  # Initialize matrix
  tidbit_data <- matrix(nrow = 0, ncol = 4)
  
  for (i in 1:length(tidbits)) {
    
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
          separate(DateTime, into = c("Date", "Time"), sep = " ", extra = "merge")
      )
      # Drop rows with NAs
      new_tidbit <- na.omit(new_tidbit)
      
    } else if (file_type == "txt" | file_type == "TXT") {
      new_tidbit <- read.delim(file = tidbits[i], row.names = NULL)
      
    } else if (file_type == "csv" &  get_year != "2019") {
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
          separate(DateTime, into = c("Date", "Time"), sep = " ", extra = "merge")
      )
      # Drop rows with NAs
      new_tidbit <- na.omit(new_tidbit)
    } else if (file_type == "csv" & get_year == "2019") {
      new_tidbit <- read.csv(file = tidbits[i])
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
    get_survey <- unlist(strsplit(tidbits[i], split = "/", fixed = TRUE))[7]
    
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
  
  # Reorder data columns for consistency
  tidbit_data <- tidbit_data %>%
    select(year, survey, tidbit_id, tidbit_datetime, Temp)
  
  # Return tidbit data
  tidbit_data
}


#### fix_long_csvs() ---------------------------------

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

#### fix_timetemp_comma() ----------------------------------

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

#### fix_longhead_txt() -----------------------------

# This quick function is meant to fix issues in the 2016-2019 data 
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


#### fix_txt_headers()------------------------

#This quick function is meant to fix issues in the 2018-2019 dataset in which R misreads the columns

fix_txt_headers <- function(filepath) {
  # Read in file, skipping first line
  issue <- read.delim(file = filepath, skip = 1, header = FALSE)
  # Label first three columns
  names(issue) <- c("date", "time", "temp")
  # Select those three columns
  issue <- issue %>%
    select(date, time, temp)
  
  # Done! Write it out
  write.table(issue, file = filepath,
              sep = "\t",
              row.names = FALSE)
}

#### sim_dat()---------------------------------------

# This quick function is meant to repeatedly simulate a model and get the number of bitter crab

sim_dat <- function(model, data, num_sims){
  
  sim_results <- matrix(nrow = num_sims, ncol = 3)
  names(sim_results) <- c("sim_num", "Total_Bitter", "pct_change_from_data")
  num_bitter <- sum(data$Bitter == 1)
  
  for (i in 1:num_sims){
    mod_res <- simulate(model)
    mod_res <- mod_res$sim_1[,1]
    sim_results[i, 1] <- i                     # Simulation number
    sim_results[i, 2] <- sum(mod_res)          # Total number of bitter crab
    sim_results[i, 3] <- (sum(mod_res) / nrow(data)) - (num_bitter / nrow(data))
  }
  colnames(sim_results) <- c("sim_num", "total_bitter", "pct_change_from_data")
  return(as.data.frame(sim_results))
}


#### pred_dat()
# This function is meant to repeatedly predict model output from the variables included
# and get the number of bitter crab. Uses

# model: A specific model you want to simulate
# data: the data with which that model was simulated (or new data that you want to test model accuracy on)
# num_sims: the number of simulations you want to run

pred_dat <- function(model, data, num_sims){
  
  # Set up empty matrix
  pred_results <- matrix(nrow = num_sims, ncol = 3)
  # Extract number of bitter crab in the actual data
  num_bitter <- sum(data$Bitter == 1)
  
  #### See what variables are in the model, pull into vector
  mod_vars <- as.character(model$call)[2]
  # Remove the Bitter part and random effects, since they'll be in all
  mod_vars <- str_split(mod_vars, pattern = fixed(" ~ "))[[1]][2]
  mod_vars <- str_split(mod_vars, pattern = fixed("+ (1 | Site) + (1 | s.Year)"))[[1]][1]
  # Split by the plus sign, remove spaces
  mod_vars <- str_split(mod_vars, pattern = fixed("+"))[[1]]
  mod_vars <- str_remove_all(mod_vars, pattern = " ")
  # Print variables in the model
  print(paste("Model", i, "variables are:"))
  print(mod_vars)
  
  # Choose all variables in model, put in new dataframe
  mod_vars <- names(data)[names(data) %in% mod_vars]
  mod_dat <- data %>%
    dplyr::select(c(Bitter, mod_vars, Site, s.Year))
  
  # Each run of this for loop is a different simulation
  for (i in 1:num_sims){
    mod_dat$Pred <- predict(model, new.data = mod_dat, type = "response")
    pred_results[i, 1] <- i                              # simulation number
    pred_results[i, 2] <- sum(mod_dat$Pred)              # Number of bitter crab in simulation
    pred_results[i, 3] <- sum(mod_dat$Pred) - num_bitter # Difference between simulated and true number of bitter crabs
  }
  colnames(pred_results) <- c("sim_num", "total_bitter", "pct_change_from_data")
  return(as.data.frame(pred_results))
}



