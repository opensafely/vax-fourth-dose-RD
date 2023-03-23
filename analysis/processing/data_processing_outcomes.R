
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
dir_create(here::here("output"), showWarnings = FALSE, recurse = FALSE)
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = FALSE)
dir_create(here::here("output", "index"), showWarnings = FALSE, recurse = FALSE)
dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = FALSE)

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
# Combine all outcomes files 
#######################################


# Read in all weekly outcomes files
list.files <- dir_ls('output/index', regexp = "FALSE", invert = TRUE)
index.dates <- map(list.files, read_feather)

# Combine together all files 
#  This will create a file containing all outcomes over study period
all.indexes <- 
  bind_rows(index.dates) %>% 
  unique() %>%
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
    dob = dob + 14) # Set DOB to mid-month 

outcomes <- function(var, name){
  all.indexes %>%
    dplyr::select(c(patient_id, contains(var), dod, dob, flu_vax_date)) %>%
    melt(id = c("patient_id", "dob", "dod", "flu_vax_date"), value.name = "date") %>%
    mutate(outcome = ifelse(!is.na(date), name, NA)) %>%
    dplyr::select(!c(variable)) %>%
    unique()
}

#### COVID related outcomes ####
primary_outcomes <- rbind(
  outcomes("covidadmitted_date", "COVID admission"),
  outcomes("covidemergency_date", "COVID emergency"),
  outcomes("coviddeath_date", "COVID death")
)

# Save #
write_csv(primary_outcomes, here::here("output", "cohort", "primary_outcomes.csv"))


##### Other outcomes ####
secondary_outcomes <- rbind(
  outcomes("respdeath_date", "Respiratory death"),
  outcomes("respadmitted_date", "Respiratory admission"),
  outcomes("any_death_date", "Any death"),
  outcomes("admitted_unplanned_date", "Any admission")
  )

# Save #
write_csv(secondary_outcomes, here::here("output", "cohort", "secondary_outcomes.csv"))
