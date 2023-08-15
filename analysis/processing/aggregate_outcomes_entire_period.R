
################################################################
# This script:
# - Calculates number of outcomes by age for tables and figures
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
library('data.table')

## Create directories
dir_create(here::here("output", "covid_outcomes"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort_bydate"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "covid_outcomes","by_start_date"), showWarnings = FALSE, recurse = TRUE)

# Load functions
source(here::here("analysis", "custom_functions.R"))


#########################################
# Total events by age 
#########################################


# Sep 3
df1 <- read_feather(here::here("output", "input_outcomes_2022-09-03.feather")) %>%
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
         
         # Set DOB to mid-month
         dob = dob + 14,
         
         # Calculate age at index date
         age_yrs = (dob %--% as.Date("2022-09-03")) %/% years(1),
         age_mos = (dob %--% as.Date("2022-09-03")) %/% months(1),
         age_3mos = floor(age_mos / 3),
         over50 = if_else(age_yrs >= 50 & age_yrs < 55, 1, 0, 0),
         sep3 = 1) %>%
  
  # Drop if outside age range or died before index date
  subset(age_yrs >= 45 & age_yrs < 55 & 
           (is.na(dod) | dod >= as.Date("2022-09-03"))) %>%
  dplyr::select(c("patient_id", "covidcomposite", "respcomposite", "anydeath", 
                  "anyadmitted", "over50", "sep3"))

# Oct 15
df2 <- read_feather(here::here("output", "input_outcomes_2022-10-15.feather")) %>%
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
         
         # Set DOB to mid-month
         dob = dob + 14,
         
         # Calculate age at index date
         age_yrs = (dob %--% as.Date("2022-09-03")) %/% years(1),
         age_mos = (dob %--% as.Date("2022-09-03")) %/% months(1),
         age_3mos = floor(age_mos / 3),
         over50 = if_else(age_yrs >= 50 & age_yrs < 55, 1, 0, 0),
         sep3 = 1) %>%
  
  # Drop if outside age range or died before index date
  subset(age_yrs >= 45 & age_yrs < 55 & 
           (is.na(dod) | dod >= as.Date("2022-10-15"))) %>%
  dplyr::select(c("patient_id", "covidcomposite", "respcomposite", "anydeath", 
                  "anyadmitted", "over50", "sep3"))

# Nov 26
df3 <- read_feather(here::here("output", "input_outcomes_2022-11-26.feather")) %>%
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
         
         # Set DOB to mid-month
         dob = dob + 14,
         
         # Calculate age at index date
         age_yrs = (dob %--% as.Date("2022-09-03")) %/% years(1),
         age_mos = (dob %--% as.Date("2022-09-03")) %/% months(1),
         age_3mos = floor(age_mos / 3),
         over50 = if_else(age_yrs >= 50 & age_yrs < 55, 1, 0, 0),
         sep3 = 1) %>%
  
  # Drop if outside age range or died before index date
  subset(age_yrs >= 45 & age_yrs < 55 & 
           (is.na(dod) | dod >= as.Date("2022-11-26"))) %>%
  dplyr::select(c("patient_id", "covidcomposite", "respcomposite", "anydeath", 
                  "anyadmitted", "over50", "sep3"))
  

all <- rbind(df1,df2,df3) %>%
  mutate(covidcomposite = ifelse(covidcomposite == TRUE, 1, 0),
         respcomposite = ifelse(respcomposite == TRUE, 1, 0),
         anydeath = ifelse(anydeath == TRUE, 1, 0),
         anyadmitted = ifelse(anyadmitted == TRUE, 1, 0)) %>%
  group_by(patient_id, over50) %>%
  summarise(covidcomposite = max(covidcomposite),
            respcomposite = max(respcomposite),
            anydeath = max(anydeath),
            anyadmitted = max(anyadmitted),
            sep3 = max(sep3)) %>%
  subset(sep3 == 1)


total <- all %>%
  
  # Calculate total population by age group
  group_by(over50) %>%
  mutate(total = n()) %>%
  ungroup(over50) %>%
  
  # Number of events by age
  group_by(over50, total) %>%
  summarise(n_covidcomposite = sum(covidcomposite == 1, na.rm = TRUE),
            n_respcomposite = sum(respcomposite ==1, na.rm = TRUE),
            n_anydeath = sum(anydeath ==1, na.rm = TRUE),
            n_anyadmitted = sum(anyadmitted == 1, na.rm = TRUE)) %>%
  
  # Rate per 100,000
  mutate(rate_covidcomposite = n_covidcomposite / total * 100000,
         rate_respcomposite = n_respcomposite / total * 100000,
         rate_anydeath = n_anydeath / total * 100000,
         rate_anyadmitted = n_anyadmitted / total * 100000
  ) 

write.csv(total, here::here("output", "covid_outcomes", "outcomes_byage_total.csv"), row.names = FALSE)
  
  
# Same as above, but with redaction/rounding
total_red <- total %>%
    mutate(across(contains("n_"), redact),
         across(contains("n_"), rounding),
         
         total = rounding(total),
         
         rate_covidcomposite = n_covidcomposite / total * 100000,
         rate_respcomposite = n_respcomposite / total * 100000,
         rate_anydeath = n_anydeath / total * 100000,
         rate_anyadmitted = n_anyadmitted / total * 100000) %>%
    rename(total_round5 = total) %>%
    rename_at(vars(contains("n_")), ~ paste0(., '_round5')) %>%
    rename_at(vars(contains("rate_")), ~ paste0(., '_round5'))
  
write.csv(total_red, here::here("output", "covid_outcomes", "outcomes_byage_total_red.csv"), row.names = FALSE)
  

