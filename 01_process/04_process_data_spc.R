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
source_code(target_repo = "NDWF", file_name = "functions.R")

# define directories
get_directory()
dir <- get_directory()
dir_data <- file.path(dir, "data")
dir_misc <- file.path(dir, "misc")
dir_in <- file.path(dir_data, "school-pupils-and-their-characteristics")

# script variable definition #

# determine year list (akin to other data sources)
years_list <- paste0(20, 10:25, 11:26)
lookup <- data.frame(time_period = as.numeric(years_list),
                     academic_year = as.numeric(substr(years_list, 1, 4)))

# rename folders that currently only have one year included (data collected in Jan)
rename_folders <- F
if (rename_folders) {
  # save df with old and new folder names
  start <- 2010
  finish <- 2019
  # data collection takes place in Jan of the year
  # e.g., data from 2014 is from 2013/14
  tmp <- data.frame(old = c(start:finish)) 
  tmp$new <- paste0(paste0(tmp$old-1,"-", gsub(20, "",tmp$old)))
  # add dirs
  tmp$from <- file.path(dir_in, tmp$old)
  tmp$to <- file.path(dir_in, tmp$new)
  # rename
  file.rename(from = c(tmp$from), to = c(tmp$to))
}

# determine years of interest
start <- 2010
finish <- 2025

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
files_pupils <- list.files(path = dir_in,
                           pattern = "school_level|School_level_school|Schools_Pupils_UD|pupil_characteristics_UD",
                           recursive = T,
                           full.names = T)
files_pupils <- files_pupils[!grepl("Meta|meta|ncyear|class|census|TEMPLATE|api", files_pupils)]
files_pupils


# Create column name correspondence table for pupil data
column_lookup_pupils <- tibble(
  standard_name = c(
    # Basic identifiers (specified ones only)
    "time_period", "urn", "laestab", "old_la_code", "estab", "school_name", 
    
    # Pupil counts (numbers only, no percentages)
    "fte_pupils", "num_pup_tot", "num_pup_boys", "num_pup_girls", 
    "num_pup_boarders", 
    
    # Key Stage and Year Group Measures (appears 2018-2025)
    "num_pup_ey",
    "num_pup_nurs", 
    "num_pup_recep",
    "num_pup_ks1",
    "num_pup_ks2",
    "num_pup_ks3",
    "num_pup_ks4", 
    "num_pup_ks5",
    
    # Free School Meals (numbers and percentages)  
    "num_pup_fsm",
    "perc_pup_fsm",
    "num_pup_fsm_spt",
    "perc_pup_fsm_spt",
    # "num_pup_tot_fsm_calc", # only used in 2011-2013
    "num_pup_tot_fsm_calc_spt",
    
    # Language (numbers and percentages)
    "num_pup_efl",
    "perc_pup_efl",
    "num_pup_eal", 
    "perc_pup_eal", 
    "num_pup_ufl",
    "perc_pup_ufl",
    
    # Focused Ethnicity - White British only
    "num_pup_ethn_white_british",
    "perc_pup_ethn_white_british",
    
    # Focused Ethnicity - Mixed groups
    "num_pup_ethn_mixed_white_and_black_caribbean", 
    "perc_pup_ethn_mixed_white_and_black_caribbean", 
    "num_pup_ethn_mixed_white_and_black_african",
    "perc_pup_ethn_mixed_white_and_black_african",
    "num_pup_ethn_mixed_white_and_asian",
    "perc_pup_ethn_mixed_white_and_asian",
    "num_pup_ethn_mixed_other",
    "perc_pup_ethn_mixed_other",
    
    # Focused Ethnicity - Black groups
    "num_pup_ethn_black_caribbean",
    "perc_pup_ethn_black_caribbean",
    "num_pup_ethn_black_african",
    "perc_pup_ethn_black_african",
    "num_pup_ethn_black_other",
    "perc_pup_ethn_black_other",
    
    # Focused Ethnicity - Asian groups
    "num_pup_ethn_asian_indian",
    "perc_pup_ethn_asian_indian",
    "num_pup_ethn_asian_pakistani", 
    "perc_pup_ethn_asian_pakistani", 
    "num_pup_ethn_asian_bangladeshi",
    "perc_pup_ethn_asian_bangladeshi",
    "num_pup_ethn_asian_chinese",
    "perc_pup_ethn_asian_chinese",
    "num_pup_ethn_asian_other",
    "perc_pup_ethn_asian_other",
    
    # Ethnicity - Unclassified
    "num_pup_ethn_unclassified",
    "perc_pup_ethn_unclassified"
    
    
  ),
  
  # All possible variations for each standard name
  variations = list(
    # Basic identifiers
    c("time_period"),
    c("urn"),
    c("laestab"),
    c("old_la_code"),
    c("estab", "estab_number"),
    c("school_name"),
    
    # Pupil counts - handling gender terminology changes and rounding variations
    c("fte_pupils", "fte_pupils_(unrounded)"),
    c("headcount_of_pupils", "headcount_of_pupils_(unrounded)"),
    c("headcount_total_boys", "headcount_total_male", "headcount_total_boys_(rounded)"),
    c("headcount_total_girls", "headcount_total_female", "headcount_total_girls_(rounded)"),
    c("total_boarders"),
    
    # Key Stage and Year Group Measures (NEW ADDITIONS - appear 2018-2025)
    c("number_of_early_year_pupils_(years_e1_and_e2)", "number_of_early_year_pupils__years_e1_and_e2_"),
    c("number_of_nursery_pupils_(years_n1_and_n2)", "number_of_nursery_pupils__years_n1_and_n2_"), # Number of nursery pupils (years N1 and N2)
    c("number_of_reception_pupils_(year_r)", "number_of_reception_pupils__year_r_"),
    c("number_of_key_stage_1_pupils_(years_1_and_2)", "number_of_key_stage_1_pupils__years_1_and_2_"),
    c("number_of_key_stage_2_pupils_(years_3_to_6)", "number_of_key_stage_2_pupils__years_3_to_6_"),
    c("number_of_key_stage_3_pupils_(years_7_to_9)", "number_of_key_stage_3_pupils__years_7_to_9_"), 
    c("number_of_key_stage_4_pupils_(years_10_and_11)", "number_of_key_stage_4_pupils__years_10_and_11_"),
    c("number_of_key_stage_5_pupils_(years_12_to_14)", "number_of_key_stage_5_pupils__years_12_to_14_"),
    
    # Free School Meals - multiple variations (numbers only)
    c("number_of_pupils_known_to_be_eligible_for_free_school_meals", 
      "number_of_pupils_known_to_be_eligible_for_and_claiming_free_school_meals"),
    c("perc_of_pupils_known_to_be_eligible_for_free_school_meals", 
      "perc_of_pupils_known_to_be_eligible_for_and_claiming_free_school_meals"),
    c("number_of_pupils_known_to_be_eligible_for_free_school_meals_(performance_tables)"),
    c("perc_of_pupils_known_to_be_eligible_for_free_school_meals_(performance_tables)"),
    # c("number_of_pupils_(used_for_fsm_calculation)"), # only used in 2011-2013
    c("number_of_pupils_(used_for_fsm_calculation_in_performance_tables)"),
    
    # Language
    c("number_of_pupils_whose_first_language_is_known_or_believed_to_be_english"),
    c("perc_of_pupils_whose_first_language_is_known_or_believed_to_be_english"),
    c("number_of_pupils_whose_first_language_is_known_or_believed_to_be_other_than_english"),
    c("perc_of_pupils_whose_first_language_is_known_or_believed_to_be_other_than_english"),
    c("number_of_pupils_whose_first_language_is_unclassified"),
    c("perc_of_pupils_whose_first_language_is_unclassified"),
    
    # Focused ethnicity fields
    # White British
    c("number_of_pupils_classified_as_white_british_ethnic_origin"),
    c("perc_of_pupils_classified_as_white_british_ethnic_origin"),
    
    # Mixed ethnicity groups
    c("number_of_pupils_classified_as_white_and_black_caribbean_ethnic_origin"),
    c("perc_of_pupils_classified_as_white_and_black_caribbean_ethnic_origin"),
    c("number_of_pupils_classified_as_white_and_black_african_ethnic_origin"),
    c("perc_of_pupils_classified_as_white_and_black_african_ethnic_origin"),
    c("number_of_pupils_classified_as_white_and_asian_ethnic_origin"),
    c("perc_of_pupils_classified_as_white_and_asian_ethnic_origin"),
    c("number_of_pupils_classified_as_any_other_mixed_background_ethnic_origin"),
    c("perc_of_pupils_classified_as_any_other_mixed_background_ethnic_origin"),
    
    # Black groups
    c("number_of_pupils_classified_as_caribbean_ethnic_origin", 
      "number_of_pupils_classified_as_black_caribbean_ethnic_origin"),
    c("perc_of_pupils_classified_as_caribbean_ethnic_origin",
      "perc_of_pupils_classified_as_black_caribbean_ethnic_origin"),
    c("number_of_pupils_classified_as_african_ethnic_origin", 
      "number_of_pupils_classified_as_black_african_ethnic_origin"),
    c("perc_of_pupils_classified_as_african_ethnic_origin",
      "perc_of_pupils_classified_as_black_african_ethnic_origin"),
    c("number_of_pupils_classified_as_any_other_black_background_ethnic_origin"),
    c("perc_of_pupils_classified_as_any_other_black_background_ethnic_origin"),
    
    # Asian groups
    c("number_of_pupils_classified_as_indian_ethnic_origin"),
    c("perc_of_pupils_classified_as_indian_ethnic_origin"),
    c("number_of_pupils_classified_as_pakistani_ethnic_origin"),
    c("perc_of_pupils_classified_as_pakistani_ethnic_origin"),
    c("number_of_pupils_classified_as_bangladeshi_ethnic_origin"),
    c("perc_of_pupils_classified_as_bangladeshi_ethnic_origin"),
    c("number_of_pupils_classified_as_chinese_ethnic_origin"),
    c("perc_of_pupils_classified_as_chinese_ethnic_origin"),
    c("number_of_pupils_classified_as_any_other_asian_background_ethnic_origin"),
    c("perc_of_pupils_classified_as_any_other_asian_background_ethnic_origin"),
    
    # Unclassified
    c("number_of_pupils_unclassified"),
    c("perc_of_pupils_unclassified")
  )
  
)

# Run the review of the column name lookup
review_lookup_mappings(lookup_table = column_lookup_pupils)

# Create the reverse lookup
reverse_lookup_pupils <- create_reverse_lookup(column_lookup_pupils)

# Initialize empty list to store all processed datasets
ud_all_p <- list()

# LOOP OVER ALL YEARS 
for (i in seq_along(start:finish)) {
  year = c(start:finish)[i]
  
  # determine academic year
  academic_year <- paste0(year,"-", year+1)
  time_period <- as.numeric(gsub("-20", "", academic_year))
  
  # read in data: Use fread with more robust settings
  spc <- fread(files_pupils[i], 
               fill = Inf,                    # whole file is read for detecting the number of columns.
               sep = ",",                     # Explicitly set separator
               quote = "\"",                  # Handle quoted fields
               strip.white = TRUE,            # Remove whitespace
               blank.lines.skip = TRUE,       # Skip empty lines
               encoding = "Latin-1",          # Handle encoding issues
               na.strings = na_values,        # IMPORTANT: Convert NA values during read
               header = TRUE,                 # use column names from file
               nrows = -1,                    # Read all rows
               showProgress = FALSE)    
  
  # Convert to data.frame
  spc <- as.data.frame(spc)
  
  # change col names
  names(spc) <- tolower(gsub(".", "_", names(spc), fixed = T))
  names(spc) <- gsub(" ", "_", names(spc), fixed = T)
  names(spc) <- gsub("%", "perc", names(spc), fixed = T)
  
  # add time period
  spc$time_period <- time_period
  
  # **standardise COLUMN NAMES**
  spc <- standardise_column_names(spc, lookup = reverse_lookup_pupils)
  
  # re-order columns
  spc <- spc[, intersect(column_lookup_pupils$standard_name, names(spc))]
  
  # subset to exclude any non-school level data
  spc <- spc %>% filter(urn != "" & urn != 0)
  
  # replace NA in LAESTAB where possible
  if("laestab" %in% names(spc)) {
    
    spc$laestab <- ifelse(is.na(spc$laestab) & !is.na(spc$old_la_code) & !is.na(spc$estab),
                          as.numeric(paste0(spc$old_la_code, spc$estab)),
                          spc$laestab)
  } else {
    spc$laestab <- ifelse(!is.na(spc$old_la_code) & !is.na(spc$estab),
                          as.numeric(paste0(spc$old_la_code, spc$estab)),
                          NA)
  }
  
  # check if any school has more than one entry
  if (nrow(spc %>% group_by(urn) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one URN with more than one entry.", academic_year))
  if (nrow(spc %>% group_by(laestab) %>% filter(n() > 1)) != 0) message(sprintf("Data from %s has at least one LAESTAB with more than one entry.", academic_year))
  
  # match with GIAS: check urns and clean up data
  spc <- cleanup_data(data_in = spc)
  
  # Store processed dataset in list with academic year (encoded as time_period) as name
  ud_all_p[[academic_year]] <- spc
  
  # Print progress
  cat(paste("Processed", academic_year, "- Rows:", nrow(spc), "Columns:", ncol(spc), "\n\n\n"))
  
  rm(spc)
  gc()
}

# **BIND ALL DATASETS TOGETHER**
# Get all unique column names across all datasets
cols_p <- unique(unlist(lapply(ud_all_p, names)))

# Ensure all datasets have the same columns (fill missing with NA)
ud_stan_p <- lapply(ud_all_p, function(df) {
  missing_cols <- setdiff(cols_p, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  return(df[cols_p])  # Reorder columns consistently
})

# Combine all datasets using bind_rows
ud_pupils <- bind_rows(ud_stan_p, .id = "academic_year")

# check that no column is missing
column_lookup_pupils$standard_name[! column_lookup_pupils$standard_name %in% names(ud_pupils)]

# re-order columns
ud_pupils <- ud_pupils %>%
  relocate(time_period, ESTAB_NAME, LAESTAB, URN, laestab_spc, urn_spc) %>%
  relocate(intersect(column_lookup_pupils$standard_name, names(ud_pupils)), .after = estab) %>%
  select(-c(academic_year, old_la_code, estab)) %>%
  arrange(URN, time_period) %>%
  as.data.frame()

# check NAs
# NOTE that 2010/11-2017/18 does not contain number of pupils per KS
# NOTE that 2018/19 does not contain the percentages for ethnicity
for (i in 1:length(unique(ud_pupils$time_period))) {
  
  print(unique(ud_pupils$time_period)[i])

  tmp <- apply(ud_pupils[ud_pupils$time_period == unique(ud_pupils$time_period)[i], ], 2, function(x){sum(is.na(x))})
  print(tmp[tmp == nrow(ud_pupils[ud_pupils$time_period == unique(ud_pupils$time_period)[i], ])])
}



# Print summary of combined dataset
cat("\n--- COMBINED PUPILS DATASET SUMMARY ---\n")
cat("Total rows:", nrow(ud_pupils), "\n")
cat("Total columns:", ncol(ud_pupils), "\n")
cat("Academic years included:", length(unique(ud_pupils$time_period)), "\n")
cat("Years:", paste(sort(unique(ud_pupils$time_period)), collapse = ", "), "\n")

# Show year distribution
year_counts <- table(ud_pupils$time_period)
print(year_counts)


# sum up pupil ethnicity #

# Black ethnic origin
new_col <- "num_pup_ethn_black" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("num_pup_ethn_black_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("num_pup_ethn_black_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("num_pup_ethn_black_", names(ud_pupils))] <- NULL

new_col <- "perc_pup_ethn_black" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("perc_pup_ethn_black_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("perc_pup_ethn_black_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("perc_pup_ethn_black_", names(ud_pupils))] <- NULL

# Asian ethnic origin
new_col <- "num_pup_ethn_asian" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("num_pup_ethn_asian_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("num_pup_ethn_asian_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("num_pup_ethn_asian_", names(ud_pupils))] <- NULL

new_col <- "perc_pup_ethn_asian" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("perc_pup_ethn_asian_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("perc_pup_ethn_asian_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("perc_pup_ethn_asian_", names(ud_pupils))] <- NULL

# Mixed ethnic origin
new_col <- "num_pup_ethn_mixed" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("num_pup_ethn_mixed_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("num_pup_ethn_mixed_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("num_pup_ethn_mixed_", names(ud_pupils))] <- NULL

new_col <- "perc_pup_ethn_mixed" 
ud_pupils[, new_col] <- rowSums(ud_pupils[, grepl("perc_pup_ethn_mixed_", names(ud_pupils))], na.rm = T)
ud_pupils$na_count <- rowSums(is.na(ud_pupils[, grepl("perc_pup_ethn_mixed_", names(ud_pupils))]))
ud_pupils[, new_col] <- ifelse(ud_pupils$na_count == max(ud_pupils$na_count), NA, ud_pupils[, new_col])
ud_pupils[, grepl("perc_pup_ethn_mixed_", names(ud_pupils))] <- NULL

ud_pupils$na_count <- NULL

#### save data ####

gc()

# save file
data.table::fwrite(ud_pupils, file = file.path(dir_data, "data_spc.csv"), row.names = F)