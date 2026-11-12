# Schools, pupils and their characteristics #

# This release contains the latest statistics on school and pupil numbers and their characteristics, 
# including age, gender, free school meals (FSM) eligibility, English as an additional language, ethnicity, school characteristics, class sizes.
# The publication combines information from the school census, school level annual school census, 
# general hospital school census and alternative provision census.

# The most recently published data is from the school census which took place on 18th January 2024 (Spring census)

options(scipen = 999)
# empty work space
rm(list = ls())
gc()

# load libraries
library(dplyr)
library(purrr)
library(rlang)
library(data.table)

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

# source functions
source_code(target_repo = "DfE_data", file_name = "functions.R")

# define directories
get_directory()
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")
dir_in <- file.path(dir_data, "academies-accounts-returns")

# script variable definition #

# determine year list (akin to other data sources)
years_list <- paste0(20, 16:24, 17:25)
lookup <- data.frame(time_period = as.numeric(years_list),
                     academic_year = as.numeric(substr(years_list, 1, 4)))


# determine years of interest
start <- 2016
finish <- 2024

# Define NA values first
na_values <- c(":", "z", "NULL", "Unknown", "x", "..", "", ">", "^")


# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_estab.csv"), encoding = "UTF-8"))

# remove all establishments without an LAESTAB (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$LAESTAB), ]

# select relevant laestab and urn only and relevant columns
gias <- gias[!duplicated(gias), c("LAESTAB", "URN", "ESTAB_NAME")]


#### combine data ####

# Best Practice for Matching Inconsistent Column Names Across Years

## 1. Create a Column Name Correspondence Table
## 2. Automated Renaming Using dplyr and purrr
## 3. Use bind_rows() for Efficient Combining
## 4. Clean id information

# pupils on roll #

# list data files
files <- list.files(path = dir_in,
                    pattern = "AAR",
                    recursive = T,
                    full.names = T)
files

# Create column name correspondence table for pupil data
column_lookup_aar <- tibble(
  standard_name = c(
    # Basic identifiers (specified ones only)
    "time_period", "urn", "laestab", "school_name", "period_covered",
    
    "academy_type",
    "trust_name",
    
    "date_opened",
    "date_closed",
    "date_left",
    "date_joined",
    
    "num_pup",
    "num_teach",
    
    # totals
    "income_tot",
    "expend_tot",
    
    # Direct teaching labour plus closely related teaching support
    "cost_staff_pay_teaching",
    "cost_staff_pay_supply_teaching",
    "cost_staff_pay_agency_supply_teaching", 
    "cost_staff_pay_education_support",
    "cost_staff_development_training",
    "cost_staff_insurance_supply_teacher",
    "cost_staff_insurance_staff_related",
    "cost_staff_indirect_employee_expenses",
    "cost_staff_tot",
    
    # Teaching resources
    "cost_teaching_resources_non_ict",
    "cost_teaching_resources_ict",
    "cost_teaching_resources_exam_fees",
    "cost_teaching_resources_educational_consultancy",
    "cost_teaching_resources_educational_supplies",
    "cost_teaching_resources_brought_in_prof_services"
    
  ),
  
  # All possible variations for each standard name
  variations = list(
    # Basic identifiers
    c("time_period"),
    c("URN"),
    c("LAEstab", "LAEsteb"),
    c("School Name"),
    c("Period covered by return"),
    
    c("MAT SAT or Central Services", "MAT/SAT"),
    c("Trust or Company Name", "Company Name"),
    c("Date opened"),
    c("Date closed"),
    c("Date left if in period", "Date left or closed if in period"),
    c("Date joined if in period", "Date joined or opened if in period"),
    
    c("No Pupils", "Number of pupils in academy (FTE)", "Number of pupils in academy (FTE) plus dual subsidiary registrations"),
    c("No Teachers", "Number of teachers in academy (FTE)"),
    
    c("Total Income"),
    c("Total Expenditure"),
    
    # Direct teaching labour plus closely related teaching support
    c("Teaching staff"),
    c("Supply teaching staff"),
    c("Agency supply teaching staff"), 
    c("Education support staff"),
    c("Staff development and training"),
    c("Supply teacher insurance"),
    c("Indirect employee expenses"),
    c("Staff-related insurance"),
    c("Staff Total", "Total Staff Costs"),
    
    # Teaching resources
    c("Learning resources (not ICT equipment)"),
    c("ICT learning resources"),
    c("Examination fees"),
    c("Educational Consultancy"),
    c("Educational Supplies", "Total Costs of Educational Supplies"),
    c("Brought in Professional Sevices", "Brought in Professional Services", "Costs of Brought in Professional Services")
    
  )
  
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_aar)

# Create the reverse lookup
reverse_lookup_aar <- create_reverse_lookup(column_lookup_aar)

# Initialize empty list to store all processed datasets
ud_all <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  year = c(start:finish)[i]
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  
  time_period <- as.numeric(gsub("-20", "", academic_year, fixed = T))
  # academic_year <- gsub("-20", "-", academic_year, fixed = T)
  
  # read in data
  # 2016/17 - 3:7014
  # 2017/18 - 3:8153
  # 2018/19 - 3:8932
  # 
  # aar <- xlsx::read.xlsx(files[1], sheetIndex = 2, startRow = 3, endRow = 7014)
  if(year >= 2023) skipped <- 1 else skipped = 2
  aar <- readxl::read_excel(path = files[i], sheet = 2, skip = skipped, na = c("", "NA","DNS", "NS", "n/a", "NULL"))
  
  # print(gsub("-20", "-", academic_year, fixed = T))
  # print(names(aar))
  
  # add time period
  aar$time_period <- time_period
  
  # **standardise COLUMN NAMES**
  aar <- standardise_column_names(aar, lookup = reverse_lookup_aar)
  
  # re-order columns
  aar <- aar[, intersect(column_lookup_aar$standard_name, names(aar))]
  
  # print(gsub("-20", "-", academic_year, fixed = T))
  # print(column_lookup_aar$standard_name[! column_lookup_aar$standard_name %in% names(aar)])
  
  # subset to exclude any non-school level data
  aar <- aar %>% filter(!is.na(urn) & urn != "" & urn != 0)
  
  # make sure all dates are encoded correctly
  aar <- aar %>%
    mutate(across(starts_with("date_"), to_posixct_safe))
  
  # # there may be more than one row per URN, e.g., when a SAT joined a MAT throughout the year
  # aar2 <- aar %>%
  #   group_by(time_period, urn, laestab, school_name, num_pup, num_teach) %>%
  #   summarise(
  #     across(
  #       starts_with("period") | starts_with("cost") | starts_with("income") | starts_with("expend"),
  #       ~ sum(.x, na.rm = TRUE)
  #     ), .groups = "drop"
  #   )
  
  aar <- aar %>%
    group_by(urn) %>%
    mutate(row_index = row_number()) %>%
    ungroup()
  
  check <- aar %>% group_by(urn) %>% filter(n() > 1) %>% print(n =9999)
  
  aar1 <- aar %>%
    filter(row_index == 1)
  aar2 <- aar %>%
    filter(row_index == 2)
  
  
  # check if any school has more than one entry
  if (nrow(aar1 %>% group_by(urn) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one URN with more than one entry.", academic_year))
  if (nrow(aar1 %>% group_by(laestab) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one LAESTAB with more than one entry.", academic_year))
  if (nrow(aar2 %>% group_by(urn) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one URN with more than one entry.", academic_year))
  if (nrow(aar2 %>% group_by(laestab) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one LAESTAB with more than one entry.", academic_year))
  
  print(nrow(aar1))
  # match with GIAS: check urns and clean up data
  aar1 <- cleanup_data(data_in = aar1)
  print(nrow(aar1))
  
  print(nrow(aar2))
  # match with GIAS: check urns and clean up data
  aar2 <- cleanup_data(data_in = aar2)
  print(nrow(aar2))
  
  aar <- bind_rows(
    aar1 %>%
      rename(urn_aar = urn_aar1,
             laestab_aar = laestab_aar1),
    aar2 %>%
      rename(urn_aar = urn_aar2,
             laestab_aar = laestab_aar2)
  )
  
  # Store processed dataset in list with academic year (encoded as time_period) as name
  ud_all[[academic_year]] <- aar
  
}


# for (i in seq_along(start:finish)) {
#   year = c(start:finish)[i]
#   
#   # determine academic year
#   academic_year <- paste0(year,"-", year+1)
#   time_period <- as.numeric(gsub("-20", "", academic_year))
#   
#   # read in data: Use fread with more robust settings
#   spc <- fread(files_pupils[i], 
#                  fill = Inf,                    # whole file is read for detecting the number of columns.
#                  sep = ",",                     # Explicitly set separator
#                  quote = "\"",                  # Handle quoted fields
#                  strip.white = TRUE,            # Remove whitespace
#                  blank.lines.skip = TRUE,       # Skip empty lines
#                  encoding = "Latin-1",          # Handle encoding issues
#                  na.strings = na_values,        # IMPORTANT: Convert NA values during read
#                  header = TRUE,                 # use column names from file
#                  nrows = -1,                    # Read all rows
#                  showProgress = FALSE)    
#   
#   # Convert to data.frame
#   spc <- as.data.frame(spc)
#   
#   # change col names
#   names(spc) <- tolower(gsub(".", "_", names(spc), fixed = T))
#   names(spc) <- gsub(" ", "_", names(spc), fixed = T)
#   names(spc) <- gsub("%", "perc", names(spc), fixed = T)
#   
#   # add time period
#   spc$time_period <- time_period
#   
#   # **standardise COLUMN NAMES**
#   spc <- standardise_column_names(spc, lookup = reverse_lookup_pupils)
#   
#   # re-order columns
#   spc <- spc[, intersect(column_lookup_pupils$standard_name, names(spc))]
#   
#   # subset to exclude any non-school level data
#   spc <- spc %>% filter(urn != "" & urn != 0)
#   
#   # replace NA in LAESTAB where possible
#   if("laestab" %in% names(spc)) {
#     
#     spc$laestab <- ifelse(is.na(spc$laestab) & !is.na(spc$old_la_code) & !is.na(spc$estab),
#                           as.numeric(paste0(spc$old_la_code, spc$estab)),
#                           spc$laestab)
#   } else {
#     spc$laestab <- ifelse(!is.na(spc$old_la_code) & !is.na(spc$estab),
#                           as.numeric(paste0(spc$old_la_code, spc$estab)),
#                           NA)
#   }
#   
#   # check if any school has more than one entry
#   if (nrow(spc %>% group_by(urn) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one URN with more than one entry.", academic_year))
#   if (nrow(spc %>% group_by(laestab) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one LAESTAB with more than one entry.", academic_year))
#   
#   # match with GIAS: check urns and clean up data
#   spc <- cleanup_data(data_in = spc)
# 
#   # Store processed dataset in list with academic year (encoded as time_period) as name
#   ud_all_p[[academic_year]] <- spc
#   
#   # Print progress
#   cat(paste("Processed", academic_year, "- Rows:", nrow(spc), "Columns:", ncol(spc), "\n\n\n"))
#   
#   rm(spc)
#   gc()
# }

# **BIND ALL DATASETS TOGETHER**
# Get all unique column names across all datasets
cols <- unique(unlist(lapply(ud_all, names)))

# Ensure all datasets have the same columns (fill missing with NA)
ud_stan <- lapply(ud_all, function(df) {
  missing_cols <- setdiff(cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  return(df[cols])  # Reorder columns consistently
})

# Combine all datasets using bind_rows
ud_aar <- bind_rows(ud_stan, .id = "academic_year")

# check that no column is missing
column_lookup_aar$standard_name[! column_lookup_aar$standard_name %in% names(ud_aar)]

# re-order columns
ud_aar <- ud_aar %>%
  relocate(time_period, ESTAB_NAME, LAESTAB, URN, laestab_aar, urn_aar) %>%
  relocate(intersect(column_lookup_aar$standard_name, names(ud_aar)), .after = urn_aar) %>%
  select(-c(academic_year, row_index)) %>%
  arrange(LAESTAB, time_period) %>%
  as.data.frame()

# check NAs
# NAs for date_closed in 2018/19 and 2020/21 but column names are correct
for (i in 1:length(unique(ud_aar$time_period))) {
  
  print(unique(ud_aar$time_period)[i])

  tmp <- apply(ud_aar[ud_aar$time_period == unique(ud_aar$time_period)[i], ], 2, function(x){sum(is.na(x))})
  print(tmp[tmp == nrow(ud_aar[ud_aar$time_period == unique(ud_aar$time_period)[i], ])])
}



# Print summary of combined dataset
cat("\n--- COMBINED AAR DATASET SUMMARY ---\n")
cat("Total rows:", nrow(ud_aar), "\n")
cat("Total columns:", ncol(ud_aar), "\n")
cat("Academic years included:", length(unique(ud_aar$time_period)), "\n")
cat("Years:", paste(sort(unique(ud_aar$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(ud_aar$time_period)
print(year_counts)



#### save data ####

gc()

# save file
data.table::fwrite(ud_aar, file = file.path(dir_data, "data_aar.csv"), row.names = F)
