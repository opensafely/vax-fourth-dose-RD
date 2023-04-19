
################################################################
# This script performs data cleaning and preparation
################################################################


# For running locally only #
# setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/vax-fourth-dose-RD")
# getwd()

# Import libraries #
library('tidyverse')
library('lubridate')
library('arrow')
library('here')
library('reshape2')
library('dplyr')
library('fs')
library('ggplot2')
library('RColorBrewer')
library('lubridate')
library('purrr')


## Create directories
dir_create(here::here("output"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "index"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = TRUE)

## Load functions
source(here::here("analysis", "custom_functions.R"))


###############################################################
# Outcomes start in November - Combine all outcomes files 
###############################################################

# Create list of all weekly outcomes files
list.files <- dir_ls('output/index', regexp = "input_outcomes_2_")
index.dates <- map(list.files, read_feather)

# Combine together all weekly files 
  # Create one outcome file per outcome
combine_files <- function(var){
  
  list <- purrr::map(index.dates, 
                     ~ dplyr::select(., c(contains(var), 
                              patient_id, dob, dod, flu_vax_date)))
  
  combined <- 
    bind_rows(list) %>% 
    unique() %>%
    mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") 
  
  # Wide to long
  data <- combined %>%
    reshape2::melt(id = c("patient_id", "dob", "dod", "flu_vax_date"), 
                   value.name = "date") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d", origin = "1970-01-01")) %>%
    dplyr::select(!c(variable)) %>%
    unique()
  
  print(paste0(var, " no. rows: ", nrow(data)))
  print(paste0(var, " no. people: ", n_distinct(data$patient_id)))
  
  return(data)
}


#### Save files COVID related outcomes ####
covidadmitted <- combine_files("covidadmitted")
write.csv(covidadmitted, here::here("output", "cohort", "outcomes_nov_covidadmitted.csv"),
          row.names = FALSE)

covidemerg <- combine_files("covidemergency")
write.csv(covidemerg, here::here("output", "cohort", "outcomes_nov_covidemergency.csv"),
          row.names = FALSE)

coviddeath <- combine_files("coviddeath")
write.csv(coviddeath, here::here("output", "cohort", "outcomes_nov_coviddeath.csv"),
                                     row.names = FALSE)

covidcomposite <- combine_files("covid")
write.csv(covidcomposite, here::here("output", "cohort", "outcomes_nov_covidcomposite.csv"),
                                row.names = FALSE)

#### Save files other outcomes ####
respdeath <- combine_files("respdeath")
write.csv(respdeath, here::here("output", "cohort", "outcomes_nov_respdeath.csv"),
          row.names = FALSE)

respadmitted <- combine_files("respadmitted")
write.csv(respadmitted, here::here("output", "cohort", "outcomes_nov_respadmitted.csv"),
          row.names = FALSE)

anydeath <- combine_files("any_death")
write.csv(anydeath, here::here("output", "cohort", "outcomes_nov_anydeath.csv"),
          row.names = FALSE)

anyadmitted <- combine_files("admitted_unplanned")
write.csv(anyadmitted, here::here("output", "cohort", "outcomes_nov_anyadmitted.csv"),
          row.names = FALSE)

respcomposite <- combine_files("resp")
write.csv(respcomposite, here::here("output", "cohort", "outcomes_nov_respcomposite.csv"),
          row.names = FALSE)
    


###############################################################
# Check the maximum number of outcomes
###############################################################

# 
# # Combine together all weekly files 
# # Create one outcome file per outcome
# check_max_admit_unplanned <- 
#   bind_rows(
#     purrr::map(index.dates, 
#                ~ dplyr::select(., admitted_unplanned_num))
#   ) %>% 
#   unique() %>%
#   summarise(admitted_unplanned_max = max(admitted_unplanned_num, na.rm = TRUE))
# 
# print(paste0("Max unplanned admissions (n): ", check_max_admit_unplanned))
# 
# check_max_covid_admit <-
#   bind_rows(
#     purrr::map(index.dates,
#                ~ dplyr::select(., covidadmitted_num))
#   ) %>%
#   unique() %>%
#   summarise(covidadmitted_max = max(covidadmitted_num, na.rm = TRUE))
# 
# print(paste0("Max COVID admissions (n): ", check_max_covid_admit))
# 
# check_max_covid_emerg <-
#   bind_rows(
#     purrr::map(index.dates,
#                ~ dplyr::select(., covidemergency_num))
#   ) %>%
#   unique() %>%
#   summarise(covidemergency_max = max(covidemergency_num, na.rm = TRUE))
# 
# print(paste0("Max COVID A&E (n): ", check_max_covid_emerg))
# 
# check_max_resp_admit <-
#   bind_rows(
#     purrr::map(index.dates,
#                ~ dplyr::select(., respadmitted_num))
#   ) %>%
#   unique() %>%
#   summarise(respadmitted_max = max(respadmitted_num, na.rm = TRUE))
# 
# print(paste0("Max respiratory admissions (n): ", check_max_resp_admit))
# 
# 

#######################################
# Outcomes start on November 26
# One dataset by day
#######################################

# Extract outcomes within 6 weeks (42 days) related to a specific start date
bydate1 <- function(start_date, dat, var){
  
  start_date = as.Date(start_date)
  end_date = start_date + 42
  
  data <- dat %>%
   
    mutate(dod = as.Date(dod, format = "%Y-%m-%d"),
           
           # Create variable for age in months
           age_yrs = (dob %--% start_date) %/% years(1),
           age_mos = (dob %--% start_date) %/% months(1),
           
           # Calendar birth month
           birth_month = as.factor(month(dob)),
           
           # Flag for having received flu vax before start date
           flu_vax = if_else(!is.na(flu_vax_date) & flu_vax_date < start_date, 1, 0, 0),
           
           # Flag for outcome during 42 day period (1/0)
           {{var}} := if_else((!is.na(date) & date >= start_date
                               & date <= end_date), 1, 0, 0)) %>%
    
    # Exclude if died before start date
    subset(is.na(dod) | dod >= start_date) %>%
    
    group_by(patient_id, flu_vax, age_mos, age_yrs, birth_month, dod) %>%
    
    # Collapse to one row person (as some people have multiple outcome dates)
    summarise({{var}} := max({{var}}, na.rm = TRUE))
  
  return(data)
  
}

# Combine all outcomes by start date
bydate2 <- function(start_date){
  
  dfs <- list(bydate1(start_date, covidcomposite, covidcomposite),
              bydate1(start_date, coviddeath, coviddeath),
              bydate1(start_date, covidadmitted, covidadmitted),
              bydate1(start_date, covidemerg, covidemerg),
              bydate1(start_date, respcomposite, respcomposite),
              bydate1(start_date, respadmitted, respadmitted),
              bydate1(start_date, respdeath, respdeath),
              bydate1(start_date, anydeath, anydeath),
              bydate1(start_date, anyadmitted, anyadmitted))
  
  outcomes <- dfs %>% reduce(full_join, by=c("patient_id", "flu_vax", "age_yrs", "age_mos", "dod", "birth_month"))
  
  print(paste0(start_date," (no. rows): ", nrow(outcomes)))
  print(paste0(start_date," (no. people): ", n_distinct(outcomes$patient_id)))        
  
  write.csv(outcomes, here::here("output", "cohort", paste0("outcomes_",start_date,".csv")), row.names = FALSE)
}


# Do the above over all relevant start dates
start_dates <- as.Date(0:10, origin="2022-11-26")
sapply(start_dates, bydate2)



