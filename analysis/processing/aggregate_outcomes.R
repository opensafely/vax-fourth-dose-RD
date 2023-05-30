
################################################################
# This script:
# - Calculates number of outcomes by age in months and years 
#   for tables and figures
#
# Dependency = outcomes_sep03, outcomes_oct15, outcomes_nov26
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

# No redaction

agg <- function(start_date, grp, age){
  
  # No redaction
  dat <- read_feather(here::here("output", paste0("input_outcomes_",start_date,".feather"))) %>%
    mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),

           # Set DOB to mid-month
           dob = dob + 14,
           
           # Calculate age at index date
           age_yrs = (dob %--% as.Date("2022-09-03")) %/% years(1),
           age_mos = (dob %--% as.Date("2022-09-03")) %>% months(1),
           age_3mos = floor(age_mos / 3),
           over50 = if_else(age_yrs >= 50 & age_yrs < 55, 1, 0, 0)) %>%
    
    # Drop if outside age range or died before index date
    subset(age_yrs >= 45 & age_yrs < 55 & 
             (is.na(dod) | dod >= as.Date(start_date))) %>%
    
    # Calculate total population by age group
    group_by({{age}}) %>%
    mutate(total = n()) %>%
    ungroup({{age}}) %>%
    
    # Number of events by age
    group_by({{age}}, total) %>%
    summarise(n_covidcomposite = sum(covidcomposite == 1, na.rm = TRUE),
              n_covidadmitted = sum(covidadmitted == 1, na.rm = TRUE),
              n_coviddeath = sum(coviddeath == 1, na.rm = TRUE),
              n_covidemerg = sum(covidemergency == 1, na.rm = TRUE),
              n_respcomposite = sum(respcomposite ==1, na.rm = TRUE),
              n_respdeath = sum(respdeath == 1, na.rm = TRUE),
              n_respadmitted = sum(respadmitted == 1, na.rm = TRUE),
              n_anydeath = sum(anydeath ==1, na.rm = TRUE),
              n_anyadmitted = sum(anyadmitted == 1, na.rm = TRUE)) %>%
    
    # Rate per 100,000
    mutate(rate_covidcomposite = n_covidcomposite / total * 100000,
           rate_covidadmitted = n_covidadmitted / total * 100000,
           rate_coviddeath = n_coviddeath / total * 100000,
           rate_covidemerg = n_covidemerg / total * 100000,
           rate_respcomposite = n_respcomposite / total * 100000,
           rate_respdeath = n_respdeath / total * 100000,
           rate_respadmitted = n_respadmitted / total * 100000,
           rate_anydeath = n_anydeath / total * 100000,
           rate_anyadmitted = n_anyadmitted / total * 100000
    ) 
  
  
  write.csv(dat, here::here("output", "covid_outcomes", "by_start_date", 
                            paste0("outcomes_byage_",grp,"_",start_date,".csv")), row.names = FALSE)
  
  # Same as above, but with redaction/rounding
  dat_red <- dat %>%
    mutate(across(contains("n_"), redact),
         across(contains("n_"), round),
         
         total = round(total),
         
         rate_covidcomposite = n_covidcomposite / total * 100000,
         rate_covidadmitted = n_covidadmitted / total * 100000,
         rate_coviddeath = n_coviddeath / total * 100000,
         rate_covidemerg = n_covidemerg / total * 100000,
         rate_respcomposite = n_respcomposite / total * 100000,
         rate_respdeath = n_respdeath / total * 100000,
         rate_respadmitted = n_respadmitted / total * 100000,
         rate_anydeath = n_anydeath / total * 100000,
         rate_anyadmitted = n_anyadmitted / total * 100000) %>%
    rename(total_round5 = total) %>%
    rename_at(vars(contains("n_")), ~ paste0(., '_round5')) %>%
    rename_at(vars(contains("rate_")), ~ paste0(., '_round5'))
  
  write.csv(dat_red, here::here("output", "covid_outcomes", "by_start_date", 
                                paste0("outcomes_byage_",grp,"_",start_date,"_red.csv")), row.names = FALSE)
  
}

### Do the above over all relevant start dates
agg("2022-09-03", "yrs", age_yrs)
agg("2022-09-03", "over50", over50)

agg("2022-10-15", "yrs", age_yrs)
agg("2022-10-15", "over50", over50)

agg("2022-11-26", "yrs", age_yrs)
agg("2022-11-26", "over50", over50)




########################################

# Create data frame with number of patients per cohort
# pat <- function(start_date){
#   data <- read.csv(here::here("output", "cohort", paste0("outcomes_",start_date,".csv"))) %>%
# 
#     mutate(n_pat = n_distinct(patient_id),
#            start = as.character(start_date),
#            n_pat = redact(n_pat),
#            n_pat = rounding(n_pat))
#   
#   return(data)
# }
# 
# extract <- sapply(start_dates, pat) %>%
#   t() 
# 
# start <- do.call(rbind, lapply(extract[,2], as.data.frame)) 
# colnames(start)[1] = "start"
# 
# n_pat <- do.call(rbind, lapply(extract[,1], as.data.frame)) 
# colnames(n_pat)[1] = "n_pat"
# 
# total_n_by_date <- cbind(n_pat, start) %>%
#   as.data.frame() %>%
#   mutate(n_pat = as.integer(n_pat),
#          start = as.character(start))
# 
# write_csv(total_n_by_date, here::here("output", "descriptive", "total_n_by_date.csv"))



