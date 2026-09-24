rm(list=ls())
gc()

###############################
##### WEB SCRAPING SCRIPT #####
###############################


# create function to source code
source_code <- function(root_dir_name = "code", target_repo = "helper_functions", branch = "main", file_name = "file.R") {
  
  # Construct the raw GitHub URL
  git_url <- paste0("https://raw.githubusercontent.com/stefaniemeliss/", target_repo, "/", branch, "/", file_name)
  message("Trying to download: ", git_url)
  
  # Attempt to download from GitHub
  tempp_file <- tempfile(fileext = ".R")
  download_success <- tryCatch({
    curl::curl_download(git_url, tempp_file, quiet = FALSE)
    TRUE
  }, error = function(e) {
    message("Download failed: ", e$message)
    FALSE
  })
  
  if (download_success) {
    # If successful, source file
    source(tempp_file)
    unlink(tempp_file)
  } else {
    # Load local copy of file
    current_dir <- getwd()
    dir_components <- strsplit(current_dir, "/")[[1]]
    root_index <- which(dir_components == root_dir_name)
    if (length(root_index) == 0) {
      stop(paste("Root directory", root_dir_name, "not found in the current path"))
    }
    root_dir <- do.call(file.path, as.list(dir_components[1:root_index]))
    project_repo <- dir_components[root_index + 1]
    dir <- file.path(root_dir, project_repo)
    if (target_repo != project_repo) {
      dir <- gsub(project_repo, target_repo, dir)
    }
    file_path <- file.path(dir, file_name)
    print(paste("Directory:", dir))
    print(paste("File path:", file_path))
    source(file_path, local = parent.frame())
  }
}

# function to set up the environment
setup_environment <- function() {
  # Prevent scientific notation
  options(scipen = 999)
  
  # Empty the workspace in the global environment except for this function
  current_function <- c("setup_environment", "source_code")
  rm(list = setdiff(ls(envir = .GlobalEnv), current_function), envir = .GlobalEnv)
  
  # Define the directory
  dir <- getwd()
  
  # Create subfolders if they do not exist
  dir_data <- file.path(dir, "data")
  if (!dir.exists(dir_data)) {
    dir.create(dir_data)
  }
  
  dir_misc <- file.path(dir, "misc")
  if (!dir.exists(dir_misc)) {
    dir.create(dir_misc)
  }
  
  # Load necessary libraries
  library(rvest)
  library(xml2)
  library(httr)
  library(chromote)
  
  # Source external functions
  source_code(target_repo = "DfE_data", file_name = "functions.R")
  
  # Determine header information
  headers <- c(
    `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
  )

  # Export headers and directories to the global environment
  assign("headers", headers, envir = .GlobalEnv)
  assign("dir_data", dir_data, envir = .GlobalEnv)
  # assign("dir_misc", dir_misc, envir = .GlobalEnv)
}

##### School workforce in England ##### 

# This release is largely based on the School Workforce Census (SWC). 
# The census, which runs each November, collects information from schools and local 
# authorities on the school workforce in state-funded schools in England.

# Independent schools, non-maintained special schools, sixth-form colleges 
# and further education establishments are not included in the SWC.

# This release includes information on teaching and support staff, their characteristics, 
# teacher retention and pay, qualifications and details of the subjects taught in secondary schools.

# https://explore-education-statistics.service.gov.uk/methodology/school-workforce-in-england

# run all setups (reset environment)
setup_environment()

# download data
webscrape_government_data(dir_out =  file.path(dir_data, "school-workforce-in-england"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/school-workforce-in-england/releases",
                          pattern_to_match = glob2rx("*school-workforce*england/20*|*school-workforce*november-20*"))


##### Schools, pupils and their characteristics ##### 

# This release contains the latest statistics on school and pupil numbers and their characteristics, 
# including age, gender, free school meals (FSM) eligibility, English as an additional language, 
# ethnicity, school characteristics and class sizes.

# The publication combines information from the January (spring) school census, 
# school level annual school census, general hospital school census and alternative provision census.

# run all setups (reset environment)
setup_environment()

# download data: 2018/19 onwards
webscrape_government_data(dir_out =  file.path(dir_data, "school-pupils-and-their-characteristics"),
                          parent_url = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics/releases",
                          pattern_to_match = glob2rx("*school-pupils-and-their-characteristics/20*|*schools-pupils*20*"))