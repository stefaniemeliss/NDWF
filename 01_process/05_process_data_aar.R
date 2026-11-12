# Academies' Accounts Returns (AAR) #

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
source_code(target_repo = "NDWF", file_name = "functions.R")

# define directories
get_directory()
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")
dir_in <- file.path(dir_data, "academies-accounts-returns")

# script variable definition #

# determine year list (akin to other data sources)
years_list <- paste0(20, 16:24, 17:25)

# determine years of interest
start <- 2016
finish <- 2024

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
    
    # dates
    "date_opened",
    "date_closed",
    "date_left",
    "date_joined",
    
    # teacher and pupil population
    "fte_teachers",
    "fte_pup",
    "perc_pup_fsm",
    "num_pup_sen_ehcp",
    "perc_pup_sen_support",
    
    # totals
    "income_tot",
    "expend_tot",
    
    # Direct teaching labour plus closely related teaching support
    "cost_staff_pay_tot",
    "cost_staff_pay_teaching",
    "cost_staff_pay_supply_teaching",
    "cost_staff_pay_agency_supply_teaching", 
    "cost_staff_pay_education_support",
    "cost_staff_pay_educational_consultancy",
    
    # Other staff-related costs
    "cost_staff_development_training",
    "cost_staff_insurance_supply_teacher",
    "cost_staff_insurance_staff_related",
    "cost_staff_indirect_expenses",
    
    # Teaching resources
    "cost_education_supplies_tot",
    "cost_education_supplies_non_ict",
    "cost_education_supplies_exam_fee",
    "cost_education_ict"
    
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
    
    # dates
    c("Date opened"),
    c("Date closed"),
    c("Date left if in period", "Date left or closed if in period"),
    c("Date joined if in period", "Date joined or opened if in period"),

    # teacher and pupil population
    c("No Teachers", "Number of teachers in academy (FTE)"),
    c("No Pupils", "Number of pupils in academy (FTE)", "Number of pupils in academy (FTE) plus dual subsidiary registrations"),
    
    c("% of pupils eligible for FSM"),
    c("% of pupils with SEN Statement", "% of pupils with EHCP", "% of pupils with an EHCP"),
    c("% of pupils without SEN Statement", "% of pupils with SEN support", "% of pupil with SEN support"),
    
    # totals
    c("Total Income"),
    c("Total Expenditure"),
    
    # Direct teaching labour plus closely related teaching support
    c("Staff Total", "Total Staff Costs"),
    c("Teaching staff"),
    c("Supply teaching staff"),
    c("Agency supply teaching staff"), 
    c("Education support staff"),
    c("Educational Consultancy"),
    
    # Other staff-related costs
    c("Staff development and training"),
    c("Supply teacher insurance"),
    c("Staff-related insurance"),
    c("Indirect employee expenses"),
    
    # Teaching resources
    c("Educational Supplies", "Total Costs of Educational Supplies"),
    c("Learning resources (not ICT equipment)"),
    c("Examination fees"),
    c("ICT learning resources")
    
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

  # read in data
  if(year >= 2023) skipped <- 1 else skipped = 2
  aar <- readxl::read_excel(path = files[i], sheet = 2, skip = skipped, na = c("", "NA","DNS", "NS", "n/a", "NULL", ".."))
  
  # add time period
  aar$time_period <- time_period
  
  # **standardise COLUMN NAMES**
  aar <- standardise_column_names(aar, lookup = reverse_lookup_aar)
  
  # re-order columns
  aar <- aar[, intersect(column_lookup_aar$standard_name, names(aar))]
  
  # subset to exclude any non-school level data
  aar <- aar %>% filter(!is.na(urn) & urn != "" & urn != 0)
  
  # make sure all dates are encoded correctly
  aar <- aar %>%
    mutate(across(starts_with("date_"), to_posixct_safe))
  
  # there may be more than one row per URN, e.g., when a SAT joined a MAT throughout the year
  aar <- aar %>%
    group_by(urn) %>%
    mutate(row_index = row_number()) %>%
    ungroup()
  
  # check <- aar %>% group_by(urn) %>% filter(n() > 1)
  
  # split data so that URNs are unique in each
  aar1 <- aar %>%
    filter(row_index == 1)
  aar2 <- aar %>%
    filter(row_index == 2)
  
  # check if any school has more than one entry
  if (nrow(aar1 %>% group_by(urn) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one URN with more than one entry.", academic_year))
  if (nrow(aar1 %>% group_by(laestab) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one LAESTAB with more than one entry.", academic_year))
  if (nrow(aar2 %>% group_by(urn) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one URN with more than one entry.", academic_year))
  if (nrow(aar2 %>% group_by(laestab) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one LAESTAB with more than one entry.", academic_year))
  
  # match with GIAS: check urns and clean up data
  aar1 <- cleanup_data(data_in = aar1)

  # match with GIAS: check urns and clean up data
  aar2 <- cleanup_data(data_in = aar2)

  # combine data again
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
  
  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(aar), "Columns:", ncol(aar), "\n\n\n"))
  
  # remove year files
  rm(aar1, aar2, aar)
  gc()
}

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
