
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


#######################################
# OUtcomes start in September
#######################################

outcomes_sep <- read_feather(here::here("output", "index", "input_outcomes_1_2022-09-03.feather")) %>%
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%
  mutate(# Date of any COVID outcome
        covidcomposite_date = 
          pmin(covidadmitted_date, covidemergency_date, coviddeath_date, na.rm = TRUE),
        
        # Create flag for each outcomes
        covidcomposite = if_else(!is.na(covidcomposite_date), 1, 0, 0),
        covidadmitted = if_else(!is.na(covidadmitted_date), 1, 0, 0),
        coviddeath = if_else(!is.na(coviddeath_date), 1, 0, 0),
        covidemerg = if_else(!is.na(covidemergency_date), 1, 0, 0),
        admitted_unplanned = if_else(!is.na(admitted_unplanned_date), 1, 0, 0),
        any_death = if_else(!is.na(any_death_date), 1, 0, 0),
        respadmitted = if_else(!is.na(respadmitted_date), 1, 0, 0),
        respdeath = if_else(!is.na(respdeath_date), 1, 0, 0)
         ) 

write.csv(outcomes_sep, here::here("output", "cohort", "outcomes_sep_all.csv"), row.names = FALSE)


#######################################
# OUtcomes start in October
#######################################

outcomes_oct <- read_feather(here::here("output", "index", "input_outcomes_1_2022-10-15.feather")) %>%
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%
  mutate(# Date of any COVID outcome
    covidcomposite_date = 
      pmin(covidadmitted_date, covidemergency_date, coviddeath_date, na.rm = TRUE),
    
    # Create flag for each outcomes
    covidcomposite = if_else(!is.na(covidcomposite_date), 1, 0, 0),
    covidadmitted = if_else(!is.na(covidadmitted_date), 1, 0, 0),
    coviddeath = if_else(!is.na(coviddeath_date), 1, 0, 0),
    covidemerg = if_else(!is.na(covidemergency_date), 1, 0, 0),
    admitted_unplanned = if_else(!is.na(admitted_unplanned_date), 1, 0, 0),
    any_death = if_else(!is.na(any_death_date), 1, 0, 0),
    respadmitted = if_else(!is.na(respadmitted_date), 1, 0, 0),
    respdeath = if_else(!is.na(respdeath_date), 1, 0, 0)
  ) 

write.csv(outcomes_oct, here::here("output", "cohort", "outcomes_oct_all.csv"), row.names = FALSE)


###############################################################
# OUtcomes start in November - Combine all outcomes files 
###############################################################

# Create list of all weekly outcomes files
list.files <- dir_ls('output/index', regexp = "input_outcomes_2_")
index.dates <- map(list.files, read_feather)

# Combine together all weekly files 
  # Create one outcome file per outcome
combine_files <- function(var){
  
  list <- purrr::map(index.dates, 
                     ~ dplyr::select(., c(contains(var), patient_id, dod, dob, flu_vax_date)))
  
  combined <- 
    bind_rows(list) %>% 
    unique() %>%
    mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") 

  # Wide to long
  data <- combined %>%
    reshape2::melt(id = c("patient_id", "dob", "dod", "flu_vax_date"), 
                   value.name = "date") %>%
    mutate(var = if_else(!is.na(date), 1, 0, 0)) %>%
    rename(!!paste0(var, "_date") := date) %>%
    dplyr::select(!c(variable)) %>%
    unique()
  
  write.csv(data, here::here("output", "cohort", paste0("outcomes_nov_", var, ".csv")),
            row.names = FALSE)
}


#### Save files COVID related outcomes ####
combine_files("covidadmitted")
combine_files("covidemergency")
combine_files("coviddeath")
combine_files("covid")


#### Save files other outcomes ####
combine_files("respdeath")
combine_files("respadmitted")
combine_files("any_death")
combine_files("admitted_unplanned")




###############################################################
# Check the maximum number of outcomes
###############################################################


# Combine together all weekly files 
# Create one outcome file per outcome
check_max <- 
  bind_rows(
      purrr::map(index.dates, 
        ~ dplyr::select(., admitted_unplanned_num))
              ) %>% 
  unique() %>%
  summarise(admitted_unplanned_max = max(admitted_unplanned_num, na.rm = TRUE))
  
print(check_max)


