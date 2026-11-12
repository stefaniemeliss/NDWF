#### ALL-PURPOSE HELPER FUNCTIONS ####

# source code
source_code(target_repo = "helper_functions", file_name = "functions.R")

#### DfE data processing functions ####

# source functions
source_code(target_repo = "DfE_data", file_name = "functions.R")


#### PROJECT-SPECIFIC FUNCTIONS ####

# functions for data processing #

# Function to insert "/" in the format "YYYYYY" to "YYYY/YY"
insert_slash <- function(number) {
  sub("(\\d{4})(\\d{2})", "\\1/\\2", number)
}

# function to convert numeric date fields into dates, used in AAR processing
to_posixct_safe <- function(x) {
  # If the column is already POSIXct, return it unchanged
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  
  # If the column is another datetime type, such as POSIXlt,
  # convert it to POSIXct
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }
  
  # If the column is numeric, assume it contains Excel serial dates
  # and convert from days since 1899-12-30 to POSIXct
  if (is.numeric(x)) {
    return(as.POSIXct(x * 86400, origin = "1899-12-30", tz = "UTC"))
  }
  
  # If the column is character, try parsing several common date formats
  if (is.character(x)) {
    # First try full ISO datetime format: 2023-01-31 14:30:00
    out <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    # For any values still missing, try ISO date format: 2023-01-31
    missing <- is.na(out)
    out[missing] <- as.POSIXct(x[missing], format = "%Y-%m-%d", tz = "UTC")
    
    # For any values still missing, try UK date format: 31/01/2023
    missing <- is.na(out)
    out[missing] <- as.POSIXct(x[missing], format = "%d/%m/%Y", tz = "UTC")
    
    # For any values still missing, try UK datetime format: 31/01/2023 14:30
    missing <- is.na(out)
    out[missing] <- as.POSIXct(x[missing], format = "%d/%m/%Y %H:%M", tz = "UTC")
    
    # Return the parsed datetime vector
    return(out)
  }
  
  # For any other data type, make a final attempt to convert to POSIXct
  return(as.POSIXct(x, tz = "UTC"))
}
