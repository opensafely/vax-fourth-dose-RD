
################################################################
# This script defines exclusion criteria and extracts
# final study population
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


# Load functions
source(here::here("analysis", "custom_functions.R"))

end_date = as.Date("2023-02-04")


#######################################
# Prepare data
#######################################

# Read in and clean data 
baseline <- read_feather(here::here("output", "input_baseline.feather")) %>%
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%    
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
         
    # Set DOB to mid-month
    dob = dob + 14,
    
    # Flag for clinically vulnerable people
    cv = immunosuppressed | chronic_kidney_disease | chronic_resp_disease | 
      diabetes | chronic_liver_disease | chronic_neuro_disease |  asplenia |
      chronic_heart_disease | sev_mental | sev_obesity | asthma,
    
    # Determine earliest recorded date of flu vaccination 
    flu_vax_date = pmin(flu_vax_med_date, flu_vax_tpp_date, 
                        flu_vax_clinical_date, na.rm = TRUE),
    
    # Booster date (if received)
    boost_date = case_when(
        (!is.na(covid_vax_4_date) & 
              covid_vax_4_date >= as.Date("2022-09-05") & 
              covid_vax_4_date < end_date) ~ covid_vax_4_date,
        (!is.na(covid_vax_3_date) & is.na(covid_vax_4_date) &
              covid_vax_3_date >= as.Date("2022-09-05") & 
              covid_vax_3_date < end_date) ~ covid_vax_3_date,
        TRUE ~ as.Date(NA)),
    
    # Received booster anytime in 2022/23
    booster = if_else(!is.na(boost_date), 1, 0, 0),
   
    # If received fourth dose prior to second booster campaign
    covid_vax4_early = if_else(covid_vax_4_date < as.Date("2022-09-05"), 1, 0, missing = 0),
    
    # If received third dose prior to first booster campaign
    covid_vax3_early = if_else(covid_vax_3_date < as.Date("2021-09-16"), 1, 0, missing = 0),
    
    # Received another COVID vaccine in 3 months prior to start of campaign
    covid_vax_recent = if_else(( 
                                (covid_vax_3_date >= as.Date("2022-07-15") &
                                 covid_vax_3_date <= as.Date("2022-10-15")) |
                                (covid_vax_2_date >= as.Date("2022-07-15") &
                                 covid_vax_2_date <= as.Date("2022-10-15")) |
                                (covid_vax_1_date >= as.Date("2022-07-15") &
                                 covid_vax_1_date <= as.Date("2022-10-15"))
                                ), 1, 0, 0),
    
    # Received 2nd dose at least 3 months prior to start of campaign
    covid_vax2 = if_else(covid_vax_2_date < as.Date("2022-07-15"), 1, 0, 0),
    
    # Received 3rd dose at least 3 months prior to start of campaign
    covid_vax3 = if_else(covid_vax_3_date < as.Date("2022-07-15"), 1, 0, 0),
    
    any_exclusion = (carehome == 1 | cv == 1 | hscworker == 1 |
          endoflife == 1 | housebound == 1 | covid_vax4_early == 1 |
          covid_vax3_early == 1 | covid_vax2 != 1 | covid_vax_recent == 1)
  ) 

####################################################
# Overview of study population prior to exclusions 
####################################################

pop_before_exclusions_byage <- baseline %>%
  subset(age >= 45 & age < 55) %>%
  group_by(age) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(age, total) %>%
  summarise(
         carehome = sum(carehome == 1),
         
         immunosuppressed = sum(immunosuppressed == 1),
         ckd = sum(chronic_kidney_disease == 1),
         chronic_resp_disease = sum(chronic_resp_disease == 1),
         asthma = sum(asthma == 1),
         diabetes = sum(diabetes == 1),
         asplenia = sum(asplenia == 1),
         chronic_liver_disease = sum(chronic_liver_disease == 1),
         chronic_neuro_disease = sum(chronic_neuro_disease == 1),
         chronic_heart_disease = sum(chronic_heart_disease == 1),
         sev_mental = sum(sev_mental == 1),
         sev_obesity = sum(sev_obesity == 1),
         
         cv = sum(cv == 1),
         hscworker = sum(hscworker == 1),
         endoflife = sum(endoflife == 1),
         housebound = sum(housebound == 1),
         
         covid_vax4_early = sum(covid_vax4_early == 1),
         covid_vax3_early = sum(covid_vax3_early == 1),
         
         covid_vax3 = sum(covid_vax3 == 1),
         covid_vax2 = sum(covid_vax2 == 1),
         
         covid_vax_recent = sum(covid_vax_recent == 1),
         
         any_exclusion = sum(any_exclusion)
         ) %>%
  ungroup() %>%
  # Redaction and rounding
  mutate(across(-age, redact), across(-age, rounding)) 

pop_before_exclusions_total <- baseline %>%
  subset(age >= 45 & age < 55) %>%
  mutate(total = n(), age = "All") %>%
  group_by(total, age) %>%
  summarise(
    carehome = sum(carehome == 1),
    
    immunosuppressed = sum(immunosuppressed == 1),
    ckd = sum(chronic_kidney_disease == 1),
    chronic_resp_disease = sum(chronic_resp_disease == 1),
    asthma = sum(asthma == 1),
    diabetes = sum(diabetes == 1),
    asplenia = sum(asplenia == 1),
    chronic_liver_disease = sum(chronic_liver_disease == 1),
    chronic_neuro_disease = sum(chronic_neuro_disease == 1),
    chronic_heart_disease = sum(chronic_heart_disease == 1),
    sev_mental = sum(sev_mental == 1),
    sev_obesity = sum(sev_obesity == 1),
    
    cv = sum(cv == 1),
    hscworker = sum(hscworker == 1),
    endoflife = sum(endoflife == 1),
    housebound = sum(housebound == 1),
    
    covid_vax4_early = sum(covid_vax4_early == 1),
    covid_vax3_early = sum(covid_vax3_early == 1),
    
    covid_vax3 = sum(covid_vax3 == 1),
    covid_vax2 = sum(covid_vax2 == 1),
    
    covid_vax_recent = sum(covid_vax_recent == 1),
    
    any_exclusion = sum(any_exclusion)
  ) %>%
  ungroup() %>%
  # Redaction and rounding
  mutate(across(-age, redact), across(-age, rounding)) 

         
# Combine
pop_before_exclusions <- rbind(pop_before_exclusions_byage,
                               pop_before_exclusions_total)

# Save
write.csv(pop_before_exclusions,
          here::here("output", "descriptive", "total_pop_before_exclusions.csv"),
          row.names = FALSE)


#################################################
# Save final population after exclusions        
#################################################

final <- baseline %>%
  subset(any_exclusion == 0) %>%
  dplyr::select(!c(covid_vax_recent, endoflife,
                   cv, hscworker, carehome, housebound,
                   immunosuppressed, chronic_kidney_disease,
                   chronic_resp_disease, asthma, diabetes, 
                   chronic_liver_disease, chronic_neuro_disease,
                   asplenia, chronic_heart_disease, sev_mental, 
                   sev_obesity, flu_vax_tpp_date, flu_vax_med_date, 
                   flu_vax_clinical_date)
                )

# Number of obs
print(paste0("Final population (n): ", nrow(final)))

#Save
write.csv(final,
          here::here("output", "cohort", "cohort_final_sep.csv"),
          row.names = FALSE)
 
