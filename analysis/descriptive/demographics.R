###################################################################
# This script:
# - Calculates the frequency distribution by age in months
#    using baseline data
#
# Dependency = data_process_baseline
###################################################################


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

## Create directories
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = TRUE)


# Load functions
source(here::here("analysis", "custom_functions.R"))


##########################################
# Read in and prep data 
##########################################

demographics <- read_csv(here::here("output", "cohort", "cohort_final_sep.csv"),
                         col_types = cols(
                           patient_id = col_number(),
                           age_yrs = col_number(),
                           imd = col_number(),
                           region = col_character(),
                           ethnicity = col_character(),
                           sex = col_character(),
                           covid_vax3 = col_integer(),
                           dob = col_date(format = "%Y-%m-%d"),
                           dod = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(c(patient_id, age_yrs, dob, dod, imd, region, ethnicity, sex,
                  covid_vax3)) %>%
  subset(age_yrs >= 45 & age_yrs < 55 &
         (is.na(dod) | dod >= as.Date("2022-09-03"))) %>%
  
  # Create age in months variable
  mutate(age_mos = (dob %--% "2022-09-03") %/% months(1),
         age_3mos = floor(age_mos / 3)) %>%

  # Calculate denominator by age in months
  group_by(age_3mos) %>%
  mutate(total_age_3mos = n()) 


##########################################
# Function for summarising frequency 
# distribution by age in months
##########################################

freq <- function(var){
  demographics %>%
    # Count number in each IMD category by age in months
    group_by(age_3mos, {{var}}, total_age_3mos) %>% 
    tally() %>%
    mutate(
      across(c(n, total_age_3mos), redact),
      across(c(n, total_age_3mos), rounding),
      pcent = n / total_age_3mos * 100) 
}


## Number of vaccinations
doses <- freq(covid_vax3) %>%
  rename(category = covid_vax3) %>%
  mutate(variable = "Number doses",
         category = case_when(
           category == 1 ~ "3 doses",
           category == 0 ~ "2 doses"
         ))

## Sex
sex <- freq(sex) %>%
  rename(category = sex) %>%
  mutate(variable = "Sex", 
         category = case_when(
           category == "M" ~ "Male",
           category == "F" ~ "Female",
           TRUE ~ as.character(NA)
         )) 

## IMD
imd <- freq(imd) %>% 
  rename(category = imd) %>%
  mutate(variable = "IMD", 
         category = case_when(
           category == "1" ~ "1 (most deprived)",
           category == "2" ~ "2",
           category == "3" ~ "3",
           category == "4" ~ "4",
           category == "5" ~ "5 (least deprived)",
           TRUE ~ as.character(NA)
         )) 

## Region
region <- freq(region) %>% 
  rename(category = region) %>%
  mutate(variable = "Region")

## Ethnicity
ethnicity <- freq(ethnicity) %>% 
  rename(category = ethnicity) %>%
  mutate(variable = "Ethnicity")

## Combine into one file ##
demographics_by_age <- rbind(imd, sex, region, ethnicity, doses)

write_csv(demographics_by_age, here::here("output", "descriptive", "demographics_by_age.csv"))


#############################################################
# Read in and prep data - receipt of flu vaccine at Nov 26
#############################################################

fluvax <-  read_csv(here::here("output", "cohort", "cohort_final_sep.csv"),
                    col_types = cols(
                      patient_id = col_number(),
                      flu_vax_date = col_date(format = "%Y-%m-%d"),
                      dob = col_date(format = "%Y-%m-%d"),
                      dod = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(c(patient_id, dob, dod, flu_vax_date)) %>%
  
  # Create age in months variable
  mutate(age_yrs = (dob %--% "2022-11-26") %/% years(1),
         age_mos = (dob %--% "2022-11-26") %/% months(1),
         age_3mos = floor(age_mos / 3),
         flu_vax = if_else(!is.na(flu_vax_date) & 
                             flu_vax_date < "2022-11-26", 1, 0, 0)) %>%
  
  subset(age_yrs >= 45 & age_yrs < 55 &
           (is.na(dod) | dod >= as.Date("2022-11-26"))) %>%
         
  # Calculate denominator by age in months
  group_by(age_3mos) %>%
  mutate(total_age_3mos = n()) 


##########################################
# Function for summarising frequency 
# distribution by age in months
##########################################


## Sex
flu_vax_by_age <- fluvax %>%
  # Count number in each category by age in months
  group_by(age_3mos, flu_vax, total_age_3mos) %>% 
  tally() %>%
  mutate(across(c(n, total_age_3mos), redact),
         across(c(n, total_age_3mos), rounding),
         pcent = n / total_age_3mos * 100)  %>%
  rename(category = flu_vax) %>%
  mutate(variable = "Flu vaccine", 
         category = case_when(
           category == 0 ~ "No",
           category == 1 ~ "Yes")
         ) 


############ Save ########################
write_csv(flu_vax_by_age, here::here("output", "descriptive", "fluvax_byage.csv"))

