
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

# Read in and prep  data
index <- read_feather(here::here("output", "input_index_2022-09-03.feather")) %>%
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%    
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
  
  # Set DOB to mid-month
  dob = dob + 14) 



###########################################
# Check number of outcomes per person
###########################################

sum <- index %>% 
  summarise(admission_med = quantile(admitted_unplanned_num, .5, na.rm = TRUE),
            admission_max = max(admitted_unplanned_num, na.rm = TRUE),
            
            covidadmission_med = quantile(covidadmitted_num, .5, na.rm = TRUE),
            covidadmission_max = max(covidadmitted_num, na.rm = TRUE),
            
            respadmitted_med = quantile(respadmitted_num, .5, na.rm = TRUE),
            respadmitted_max = max(respadmitted_num, na.rm = TRUE),
            
            covidemergency_med = quantile(covidemergency_num, .5, na.rm = TRUE),
            covidemergency_max = max(covidemergency_num, na.rm = TRUE)) %>%
  gather(key = variable)

# Save
write.csv(sum,
          here::here("output", "descriptive", "num_outcomes_per_person.csv"),
          row.names = FALSE)



###########################################
# Create file for each index date
###########################################
