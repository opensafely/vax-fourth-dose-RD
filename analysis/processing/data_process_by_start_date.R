
################################################################
# This script creates files with outcomes
#   occurring within 42 days of each start date
#
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
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort_bydate"), showWarnings = FALSE, recurse = TRUE)

## Load functions
source(here::here("analysis", "custom_functions.R"))


#######################################
# Outcomes start on November 26
# One dataset by day
#######################################

# Extract outcomes within 6 weeks (42 days) related to a specific start date
bydate1 <- function(start_date, dat, var){
  
  start_date = as.Date(start_date)
  end_date = start_date + 42
  
  data <- read_feather(here::here("output", "cohort", paste0("outcomes_",dat,".feather"))) %>%
    
    mutate(dod = as.Date(dod, format = "%Y-%m-%d"),
           
           # Create variable for age in months on start date
           age_yrs = (dob %--% start_date) %/% years(1),
           age_mos = (dob %--% start_date) %/% months(1),
           
           # Flag for having received flu vax before start date
           flu_vax = if_else(!is.na(flu_vax_date) & flu_vax_date < start_date, 1, 0, 0),
           
           # Flag for booster before start date
           boost = if_else(!is.na(boost_date) & boost_date < start_date, 1, 0, 0),
           
           # Flag for outcome during 42 day period (1/0)
           {{var}} := if_else((!is.na(date) & date >= start_date
                               & date <= end_date), 1, 0, 0)) %>%
    
    
    # Exclude if died before start date
    subset(is.na(dod) | dod >= start_date &
             age_yrs >= 45 & age_yrs < 55) %>%
    
    group_by(patient_id, flu_vax, boost, age_mos, age_yrs, dod) %>%
    
    # Collapse to one row person (as some people have multiple outcome dates)
    summarise({{var}} := max({{var}}, na.rm = TRUE)) 
    
  
  return(data)
  
}

# Combine all outcomes by start date
bydate2 <- function(start_date){
  
  dfs <- list(
              bydate1(start_date, "coviddeath", coviddeath),
              bydate1(start_date, "covidadmitted", covidadmitted),
              bydate1(start_date, "covidemergency", covidemergency),
              bydate1(start_date, "respadmitted", respadmitted),
              bydate1(start_date, "respdeath", respdeath),
              bydate1(start_date, "anydeath", anydeath),
              bydate1(start_date, "anyadmitted", anyadmitted)
              )
  
  outcomes <- dfs %>% 
    reduce(full_join, by=c("patient_id", "flu_vax", "boost", "age_yrs", "age_mos", "dod")) %>%
    mutate(covidcomposite = if_else(coviddeath == 1 | covidadmitted == 1 |
                                      covidemergency == 1, 
                                    1, 0, 0),
           respcomposite = if_else(respdeath == 1 | respadmitted == 1,
                                   1, 0, 0))
  
  print(paste0(start_date," (no. rows): ", nrow(outcomes)))
  print(paste0(start_date," (no. people): ", n_distinct(outcomes$patient_id)))
  
  write.csv(outcomes, here::here("output", "cohort_bydate", 
                                 paste0("outcomes_",start_date,".csv")), row.names = FALSE)
}


bydate2("2022-09-03")

# Do the above over all relevant start dates
start_dates <- as.Date(0:15, origin="2022-10-15")
sapply(start_dates, bydate2)




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


