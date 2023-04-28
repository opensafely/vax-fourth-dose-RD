###################################################################
# This script:
# - Calculates the frequency distribution by age in months
#    using baseline data
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

demographics <- read_csv(here::here("output", "cohort", "cohort_final_sep.csv")) %>%
  dplyr::select(c(age, dob, dod, imd, region, ethnicity, sex)) %>%
  subset(age >= 45 & age < 55) %>%
  
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
    subset(is.na(dod) | dod >= as.Date("2022-09-03")) %>%
    # Count number in each IMD category by age in months
    group_by(age_3mos, {{var}}, total_age_3mos) %>% 
    tally() %>%
    mutate(across(c(n, total_age_3mos), rounding),
           across(c(n, total_age_3mos), redact),
         pcent = n / total_age_3mos * 100) 
}


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
demographics_by_age <- rbind(imd, sex, region, ethnicity)



#############################################################
# Read in and prep data - receipt of flu vaccine at Nov 26
#############################################################

fluvax <- read_csv(here::here("output", "cohort", "cohort_final_sep.csv")) %>%
  dplyr::select(c(age, dob, dod, flu_vax_date)) %>%
  subset(age >= 45 & age < 55 &
           (is.na(dod) | dod >= as.Date("2022-09-03"))) %>%
  
  # Create age in months variable
  mutate(age_mos = (dob %--% "2022-11-26") %/% months(1),
         age_3mos = floor(age_mos / 3),
         flu_vax = if_else(!is.na(flu_vax_date) & flu_vax_date < "2022-11-26",
                          1, 0, 0)) %>%
  
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
  mutate(across(c(n, total_age_3mos), rounding),
         across(c(n, total_age_3mos), redact),
         pcent = n / total_age_3mos * 100)  %>%
  rename(category = flu_vax) %>%
  mutate(variable = "Flu vaccine", 
         category = case_when(
           category == 0 ~ "No",
           category == 1 ~ "Yes"
         )) 


############ Save ########################
write_csv(demographics_by_age, here::here("output", "descriptive", "demographics_by_age.csv"))
write_csv(flu_vax_by_age, here::here("output", "descriptive", "fluvax_byage.csv"))
