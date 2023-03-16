
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

## Create directories
dir_create(here::here("output"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = TRUE)


#######################################
# Functions
#######################################

## Redaction
redact <- function(vars) {
  case_when(vars > 7 ~ vars)
}

## Function for rounding
rounding <- function(vars) {
  round(vars / 5) * 5
}


#######################################
# Prepare data
#######################################

# Read in and clean data 
baseline <- read_feather(here::here("output", "input_baseline_2022-09-03.feather")) %>%
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%    
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
         
    # Set DOB to mid-month
    dob = dob + 14,
    
    # Flag for above cutoff
    over50 = (age >= 50),
    
    # Flag for clinically vulnerable people
    cv = immunosuppressed | chronic_kidney_disease | chronic_resp_disease | 
      diabetes | chronic_liver_disease | chronic_neuro_disease | 
      chronic_heart_disease | sev_mental | sev_obesity,
    
    # Determine earliest recorded date of flu vaccination 
    flu_vax_date = pmin(flu_vax_med_date, flu_vax_tpp_date, 
                        flu_vax_clinical_date, na.rm = TRUE),
    
    # If received fourth dose prior to campaign start in September
    covid_vax4_early = if_else(covid_vax_4_date < as.Date("2022-09-05"), 1, 0, missing = 0),
    
    # If received third dose prior to booster becoming available to non-immunosuppressed
    covid_vax3_early = if_else(covid_vax_3_date < as.Date("2021-11-29"), 1, 0, missing = 0),
    
    # Received 3rd dose at least 3 months prior to start of campaign
    covid_vax3 = if_else(covid_vax_3_date < as.Date("2022-07-15"), 1, 0, missing = 0),
    
    # Received 2nd dose at least 3 months prior to start of campaign
    covid_vax2 = if_else(covid_vax_2_date < as.Date("2022-07-15"), 1, 0, missing = 0),
    
    # Flag for people prioritised for vaccine (including evidence of having received
       # COVID vaccine before becoming available to general population)
    vax_priority = cv | housebound | carehome | hscworker | covid_vax4_early |
      covid_vax3_early
  ) 

####################################################
# Overview of study population prior to exclusions #
####################################################

total_pop_before_exclusions <- baseline %>%
  group_by(age) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(age, total) %>%
  summarise(
         carehome = sum(carehome == 1),
         housebound = sum(housebound == 1),
         cv = sum(cv == 1),
         hscworker = sum(hscworker == 1),
         covid_vax4_early = sum(covid_vax4_early == 1),
         covid_vax3_early = sum(covid_vax3_early == 1),
         
         covid_vax3 = sum(covid_vax3 == 1),
         covid_vax2 = sum(covid_vax2 == 1),
         
         vax_priority = sum(vax_priority == 1)
         ) %>%
  # Redaction
  mutate_at(c(vars(c(carehome, housebound, cv, hscworker, 
                     vax_priority, covid_vax4_early,
                     covid_vax3_early, covid_vax3, covid_vax2))), redact) %>%
  # Rounding
  mutate_at(c(vars(c(carehome, housebound, cv, hscworker, 
                     vax_priority, covid_vax4_early,
                     covid_vax3_early, covid_vax3, covid_vax2))), round) 
         
# Save
write.csv(total_pop_before_exclusions,
          here::here("output", "descriptive", "total_pop_before_exclusions.csv"),
          row.names = FALSE)


#################################################
# Save final population after exclusions        
#################################################

final <- baseline %>%
  subset(vax_priority == 0) 

#Save
write.csv(final,
          here::here("output", "cohort", "cohort_final.csv"),
          row.names = FALSE)
 
