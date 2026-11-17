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

# Get Information about Schools #

# read in establishment data
gias <- as.data.frame(fread(file.path(dir_data, "data_gias_estab.csv"), encoding = "UTF-8"))

# remove all establishments without an LAESTAB (i.e., Children's centres, British schools overseas, Online providers)
gias <- gias[!is.na(gias$LAESTAB), ]

# GIAS for treated
gias_treated <- gias %>% 
  filter(group_uid == 2939 | URN == 148094) %>%
  arrange(group_uid, URN)
gias_treated <- gias_treated[, c(grepl("URN|ESTAB|phase|statut|nurse|sixth|gender|special", names(gias_treated)))]

# define urn and laestab
urn_treat <- gias_treated$URN
laestab_treat <- gias_treated$LAESTAB

# Read in all relevant data #

# contains running cost related information (e.g., pupil counts, % FSM, % SEN)
# details on actual spend in each cost category, total income and total expenditure
aar <- as.data.frame(fread(file.path(dir_data, "data_aar.csv"), encoding = "UTF-8"))

# contains information on teaching workforce,
# including FTEs, PTRs, absences, vacancies, occasional teachers
swf <- as.data.frame(fread(file.path(dir_data, "data_swf.csv"), encoding = "UTF-8"))

# Prepare cost data #

# filter data
aar_treated <- aar %>%
  filter(URN %in% urn_treat | LAESTAB %in% laestab_treat) %>%
  filter(time_period > 202021)

# process cost data and aggregate at year level
aar_treated <- aar_treated %>%
  group_by(time_period, URN) %>%
  mutate(academy_type = ifelse(n() > 1, "aggregated across more than 1 entry", academy_type)) %>%
  ungroup() %>%
  mutate(
    perc_pup_sen = sum(c(perc_pup_sen_ehcp, perc_pup_sen_support), na.rm = T)
    ) %>%
  group_by(time_period, ESTAB_NAME, LAESTAB, URN, academy_type, fte_teachers, fte_pup, perc_pup_fsm, perc_pup_sen) %>%
  summarise(
    across(
      matches("^(cost|income|expend|period)"),
      ~ sum(.x, na.rm = TRUE)
    ), .groups = "drop"
  )

# Prepare workforce data data #

# filter data
swf_treated <- swf %>%
  filter(URN %in% urn_treat | LAESTAB %in% laestab_treat) %>%
  filter(time_period > 202021)

# compute and rename relevant columns
swf_treated <- swf_treated %>%
  mutate(
    perc_teachers_retained = remained_in_the_same_school/teacher_fte_in_census_year * 100,
    # MISSING: Teachers with qualified teacher status (percentage)
    # can be calculated from qualified_teachers_fte/teachers_fte
    perc_teachers_qualified = qualified_teachers_fte/teachers_fte * 100,
    
    # MISSING: Non-classroom support staff - excluding auxiliary staff (Full Time Equivalent)
    # also excludes TAs
    # ACCOUNT FOR NAs!!
    # this then includes Administrative staff, Leadership - Non Teacher, Other school support staff, School business professionals and technicians
    # fte_administrative_staff, fte_leadership_non_teachers, fte_other_school_support_staff, fte_school_business_professionals, fte_technicians
    # can be calculated as swf$fte_all_support_staff-swf$fte_teaching_assistants-swf$fte_auxiliary_staff
    # or as swf$fte_administrative_staff + swf$fte_leadership_non_teachers + swf$fte_other_school_support_staff + swf$fte_school_business_professionals
    fte_nonclass_support_staff = sum(c(fte_administrative_staff, fte_school_business_professionals, fte_leadership_non_teachers,
                                      fte_technicians, fte_other_school_support_staff), na.rm = T)
    ) %>%
  rename(
    perc_teachers_pt = percent_pt_teacher,
    posts_tempfilled = tempfilled,
    posts_vacant = vacancy,
    ta_to_all_teacher_ratio = ratio_of_teaching_assistants_to_all_teachers
    ) %>%
  as.data.frame()

# select relevant columns
swf_treated <- swf_treated[, c(grepl("URN|time_period|ESTAB|fte_w|fte_all_t|fte_lead|fte_teach|fte_non|fte_aux|hc_occ_to|ratio|^perc|days|posts", names(swf_treated)))]
swf_treated <- swf_treated[, c(!grepl("unqual|adult|qts|_non_", names(swf_treated)))]

# re-arrange columns
swf_treated <- swf_treated %>%
  relocate(time_period, ESTAB_NAME, LAESTAB, URN) %>%
  relocate(contains("ratio"), .after = URN) %>%
  relocate(starts_with("perc_"), .after = URN) %>%
  relocate(starts_with("fte"), .after = URN) %>%
  arrange(URN, time_period) %>%
  as.data.frame()


#### save data ####

gc()

# combine data
df <- gias_treated %>%
  full_join(aar_treated) %>%
  full_join(swf_treated) %>%
  relocate(time_period)

# write file
data.table::fwrite(df, file = file.path(dir, "04_cost_eval", "data_packs.csv"), row.names = F)

#### create data dictionary ####

# automate the extraction of column names and types from main df
data_dict <- data.frame(
  column_name = names(df),
  data_type = sapply(df, function(x) class(x)[1]),
  stringsAsFactors = FALSE
)

# Create a named vector for descriptions
descriptions <- c(
  time_period = "Academic or financial reporting period.",
  URN = "Unique Reference Number for the establishment.",
  LAESTAB = "Local authority establishment code.",
  ESTAB_NAME = "Name of the establishment.",
  phaseofeducation = "Phase of education of the establishment.",
  statutorylowage = "Statutory lower age of pupils at the establishment.",
  statutoryhighage = "Statutory upper age of pupils at the establishment.",
  nurseryprovision = "Whether the establishment has nursery provision.",
  officialsixthform = "Whether the establishment has an official sixth form.",
  gender = "Gender intake of the establishment.",
  specialclasses = "Whether the establishment has special classes.",
  academy_type = "Academy type; where multiple entries exist for a school-year, marked as aggregated across more than one entry.",
  fte_teachers = "Full-time equivalent number of teachers as reported in AAR.",
  fte_pup = "Full-time equivalent number of pupils as reported in AAR.",
  perc_pup_fsm = "Percentage of pupils eligible for free school meals as reported in AAR.",
  perc_pup_sen = "Percentage of pupils with special educational needs, calculated as EHCP plus SEN support, as reported in AAR.",
  period_covered = "Number of periods covered in the financial return.",
  # Totals
  income_tot = "Total income.",
  expend_tot = "Total expenditure.",
  cost_staff_pay_tot = "Total staff pay costs.",
  # Teaching and Teaching support staff
  cost_staff_pay_teaching = "Teaching staff pay costs.",
  cost_staff_pay_supply_teaching = "Supply teaching staff pay costs.",
  cost_staff_pay_agency_supply_teaching = "Agency supply teaching staff pay costs.",
  cost_staff_pay_education_support = "Educational support staff pay costs.",
  cost_staff_pay_educational_consultancy = "Educational consultancy costs.",
  # Other staff costs
  cost_staff_development_training = "Staff development and training costs.",
  cost_staff_insurance_supply_teacher = "Supply teacher insurance costs.",
  cost_staff_insurance_staff_related = "Staff-related insurance costs.",
  cost_staff_indirect_expenses = "Indirect employee expenses.",
  # Non-educational support staff and services
  cost_staff_pay_admin_clerical = "Administrative and clerical staff pay cost",
  cost_staff_pay_auditors = "Auditor costs.",
  cost_staff_pay_other = "Other staff pay costs.",
  cost_staff_pay_legal_professional = "Professional services (non-curriculum) costs.",
  # Educational supplies & ICT
  cost_education_supplies_tot = "Total educational supplies costs.",
  cost_education_supplies_non_ict = "Learning resources costs excluding ICT equipment.",
  cost_education_supplies_exam_fee = "Examination fees costs.",
  cost_education_ict = "Educational ICT costs.",
  # Cost categories
  cost_staff_pay_tot_teach_teachsupp = "Cumulative cost category, combining teaching, supply, agency supply, educational support and educational consultancy staff pay costs.",
  cost_staff_pay_tot_nonedu_support = "Cumulative cost category, combining admin/clerical, auditor, professional services and other staff pay costs.",
  cost_staff_other_tot = "Cumulative cost category, combining indirect employee expenses, staff development and training, staff-related and supply teacher insurance costs.",
  cost_education_tot = "Cumulative cost category, combining examination fees, non-ICT and ICT learning resources.",
  # Spending categories
  cost_staff_tot_teach = "Cumulative spending category, combining teaching staff costs, indirect employee expenses, staff development and training, staff-related insurance costs, but excluding education support staff, educational consultancy and (agency) supply teaching staff costs.",
  cost_staff_tot_teachsupp = "Cumulative spending category, combining education support staff and educational consultancy costs.",
  cost_staff_tot_nonedu_support = "Cumulative spending category, combining indirect employee expenses, staff development and training, staff-related and supply teacher insurance costs.",
  cost_staff_tot_supply = "Cumulative spending category, combining supply teaching and agency supply teaching staff costs and supply teacher insurance.",
  # SWC
  fte_workforce = "Full-time equivalent of the total workforce.",
  fte_all_teachers = "Full-time equivalent of all teachers. Includes classroom teachers and leadership teachers (assistant, deputy and head teachers).",
  fte_leadership_teachers = "Full-time equivalent of leadership teachers (assistant, deputy and head teachers).",
  fte_teaching_assistants = "Full-time equivalent of teaching assistants.",
  fte_auxiliary_staff = "Full-time equivalent of auxiliary staff.",
  fte_nonclass_support_staff = "Full-time equivalent of non-classroom support staff (administrative staff, school business professionals, technicans, leadership non-teachers and other school-support staff), excluding auxiliary staff and teaching assistants.",
  perc_teachers_pt = "Percentage of teachers working part-time.",
  perc_teachers_retained = "Percentage of teachers retained in the same school, calculated as remained in the same school divided by teacher FTE in census year.",
  perc_teachers_qualified = "Percentage of teachers with qualified teacher status, calculated as qualified teachers FTE divided by teachers FTE.",
  ta_to_all_teacher_ratio = "Ratio of teaching assistants to all teachers.",
  pupil_to_qual_teacher_ratio = "Ratio of pupils to qualified teachers.",
  percentage_taking_absence = "Percentage of teachers taking sickness absence.",
  total_number_of_days_lost = "Total number of working days lost to teacher absence.",
  average_number_of_days_taken = "Average number of days taken by teachers who took sickness absence.",
  average_number_of_days_all_teachers = "Average number of days lost per teacher across all teachers.",
  posts_vacant = "Number of vacant teaching posts.",
  posts_tempfilled = "Number of posts temporarily filled.",
  hc_occ_total = "Total headcount of occasional teachers."
)

# add descriptions to dictionary
data_dict$description <- descriptions[data_dict$column_name]

# save file
data.table::fwrite(data_dict, file = file.path(dir, "04_cost_eval", "meta_packs.csv"), row.names = F)

