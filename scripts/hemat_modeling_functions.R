###################################
# Aidan Coyle, afcoyle@uw.edu
# Roberts Lab, UW-SAFS
# 2022-07-27
# File containing functions created during Hematodinium modeling project
###################################

#### read_tidbit_data(): Reads in all Tidbit data within a folder 

# Takes the following inputs:
#     data_path: Path to data folder for that specific survey. Don't include the last /
#     data_type: Either .txt or .xls

read_tidbit_data  <- function(data_path, data_type) {

# Get full path to all containing files
tidbits <- list.files(path = data_path, full.names = TRUE, recursive = TRUE)

# Remove all non-Tidbit files. All non-tidbit files end with "_data.xls" (or "_data.xlsx")
tidbits <- tidbits[-grep("_data", tidbits, fixed = TRUE)]

# Extract year of survey
get_year <- unlist(strsplit(data_path, split = '/', fixed = TRUE))[5]

# Initialize matrix
tidbit_data <- matrix(nrow = 0, ncol = 4)

for (i in 1:length(tidbits)) {
  print(tidbits[i])
  print(i)
  
  # Read in Tidbit file
  new_tidbit <- read.delim(file = tidbits[i], row.names = NULL)
  
  # If first column was interpreted as the names of a row (thanks to misplaced spacing), edit
  if ("row.names" %in% names(new_tidbit)) {
    names(new_tidbit) <- c("Date", "Time", "Temp")
  } else {
  }
  
  # If last column was interpreted as two separate columns (thanks to misplaced spacing), remove last one
  # Remove all columns with only NAs
  new_tidbit <- new_tidbit[colSums(!is.na(new_tidbit)) > 0]
  
  # If we have two columns, they're "DateTime" and "Temp"
  # If we have three, they're "Date", "Time", and "Temp"
  # Either way, standardize names of all columns
  if (length(names(new_tidbit)) == 2) {
    names(new_tidbit) <- c("tidbit_datetime", "Temp")
  } else if (length(names(new_tidbit)) == 3) {
    names(new_tidbit) <- c("Date", "Time", "Temp")
  } else if (length(names(new_tidbit)) == 5) {
  } else {
    print("Aidan says error in columns!")
    break
  }
  
  # Get name of survey
  get_survey <- unlist(strsplit(tidbits[i], split = "/", fixed = TRUE))[6]
  
  # Extract ID from name of Tidbit file, removing .csv or .txt as needed
  get_id <- tail(strsplit(tidbits[i], split = "/", fixed = TRUE)[[1]], n = 1)
  if (data_type == ".txt") {
    get_id <- str_remove(get_id, "(?i).txt")
  } else if (data_type == ".xls") {
    get_id <- str_remove(get_id, "(?i).xls")
  } else {
    print ("Aidan says invalid data type")
    break
  }
  
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
  # Second: if time is in minutes
  } else if (max(nchar(new_tidbit$Time)) < 6) {
    new_tidbit$Date <- mdy(new_tidbit$Date, tz = "US/Alaska")
    new_tidbit$Time <- parse_time(new_tidbit$Time, "%H:%M")
    new_tidbit$tidbit_datetime <- paste(new_tidbit$Date, new_tidbit$Time)
    new_tidbit <- new_tidbit %>%
      select(-c(Date, Time))  
  # Third: if time is in seconds
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

