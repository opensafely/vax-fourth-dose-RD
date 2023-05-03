
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

####################################################
# Function extracting outcomes in follow-up period
####################################################

out <- function(start_date){
  
  
  start_date = as.Date(start_date)
  end_date = start_date + 42
  
  read_feather(here::here("output", "index", paste0("input_outcomes_1_",start_date,".feather"))) %>%
    mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%
  
    # Exclude if died before start date
    subset(is.na(any_death_date) | any_death_date >= as.Date(start_date)) %>%
    mutate(dob = as.Date(dob, format = "%Y-%m-%d"),
         dod = as.Date(dod, format = "%Y-%m-%d"),
         
         # Age in months
         age_mos = (dob %--% start_date) %/% months(1),
         age_yrs = (dob %--% start_date) %/% years(1),
         
         # Calendar birth month
         birth_month = as.factor(month(dob)),
         
         # Flag for having received flu vax before start date
         flu_vax = if_else(!is.na(flu_vax_date) & flu_vax_date < as.Date(start_date), 1, 0, 0),
         
         # Flag for bosster before start date
         boost = if_else(!is.na(boost_date) & boost_date < as.Date(start_date), 1, 0, 0),
         
         # Create flag for each outcomes
         
         anydeath = if_else(!is.na(any_death_date) & (any_death_date >= start_date) &
                              (any_death_date <= end_date), 1, 0, 0),
  
         respdeath = if_else(!is.na(respdeath_date) & (respdeath_date >= start_date) &
                       (respdeath_date <= end_date), 1, 0, 0),

         coviddeath = if_else(!is.na(coviddeath_date) & (coviddeath_date >= start_date) &
                      (coviddeath_date <= end_date), 1, 0, 0),

         covidadmitted = if_else(!is.na(covidadmitted_date), 1, 0, 0),
         coviddeath = if_else(!is.na(coviddeath_date), 1, 0, 0),
         covidemerg = if_else(!is.na(covidemergency_date), 1, 0, 0),
         covidcomposite = if_else(covidadmitted == 1 | coviddeath == 1 |
                                    covidemerg == 1, 1, 0, 0),
         
         anyadmitted = if_else(!is.na(admitted_unplanned_date), 1, 0, 0),
         respadmitted = if_else(!is.na(respadmitted_date), 1, 0, 0),
         respdeath = if_else(!is.na(respdeath_date), 1, 0, 0),
         respcomposite = if_else(respadmitted == 1 | respdeath == 1, 1, 0, 0),
    
  ) %>%
  select(!c(contains("_date")))
}


#################################################
# Outcomes by start date
#################################################

### Outcomes starting in September (control)
outcomes_sep <- out("2022-09-03")

print(paste0("September cohort (no. rows): ", nrow(outcomes_sep)))
print(paste0("September cohort (no. people): ", n_distinct(outcomes_sep$patient_id)))     

write.csv(outcomes_sep, here::here("output", "cohort", "outcomes_2022-09-03.csv"), row.names = FALSE)


### Outcomes starting in October (control)
outcomes_oct <- out("2022-10-15")

print(paste0("October cohort (no. rows): ", nrow(outcomes_oct)))
print(paste0("October cohort (no. people): ", n_distinct(outcomes_oct$patient_id)))        
      
write.csv(outcomes_oct, here::here("output", "cohort", "outcomes_2022-10-15.csv"), row.names = FALSE)



