
################################################################
# This script:
# - Calculates number of outcomes by age for tables and figures
#
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
df1 <- read_feather(here::here("output", "input_outcomes_2022-11-26.feather")) %>%
  mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
         
         # Set DOB to mid-month
         dob = dob + 14,
         
         # Calculate age at index date
         age_yrs = (dob %--% as.Date("2022-11-26")) %/% years(1),
         age_mos = (dob %--% as.Date("2022-11-26")) %/% months(1),
         age_3mos = floor(age_mos / 3),
         over50 = if_else(age_yrs >= 50 & age_yrs < 55, 1, 0, 0),
         nov26 = 1) %>%
  
  # Drop if outside age range or died before index date
  subset(age_yrs >= 45 & age_yrs < 55 & 
           (is.na(dod) | dod >= as.Date("2022-11-26"))) %>%
  dplyr::select(c("patient_id", "covidcomposite", "respcomposite", "anydeath", 
                  "anyadmitted", "over50", "nov26"))
  
  
# No redaction
read <- function(start_date){
  
  # No redaction
  dat <- read_feather(here::here("output", paste0("input_outcomes_",start_date,".feather"))) %>%
    mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),

           # Set DOB to mid-month
           dob = dob + 14,
           
           # Calculate age at index date
           age_yrs = (dob %--% as.Date(start_date)) %/% years(1),
           age_mos = (dob %--% as.Date(start_date)) %/% months(1),
           age_3mos = floor(age_mos / 3),
           over50 = if_else(age_yrs >= 50 & age_yrs < 55, 1, 0, 0),
           nov26 = 0) %>%
    
    # Drop if outside age range or died before index date
    subset(age_yrs >= 45 & age_yrs < 55 & 
             (is.na(dod) | dod >= as.Date(start_date))) %>%
    dplyr::select(c("patient_id", "covidcomposite", "respcomposite", "anydeath", 
                    "anyadmitted", "over50", "nov26"))
  
  return(dat)
  
}

df2 <- read("2022-11-27")
df3 <- read("2022-11-28")
df4 <- read("2022-11-29")
df5 <- read("2022-11-30")
df6 <- read("2022-12-01")
df7 <- read("2022-12-02")
df8 <- read("2022-12-03")
df9 <- read("2022-12-04")
df10 <- read("2022-12-05")
df11 <- read("2022-12-06")
df12 <- read("2022-12-07")
df13 <- read("2022-12-08")
df14 <- read("2022-12-09")
  
all <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14) %>%
  mutate(covidcomposite = ifelse(covidcomposite == TRUE, 1, 0),
         respcomposite = ifelse(respcomposite == TRUE, 1, 0),
         anydeath = ifelse(anydeath == TRUE, 1, 0),
         anyadmitted = ifelse(anyadmitted == TRUE, 1, 0)) %>%
  group_by(patient_id, over50) %>%
  summarise(covidcomposite = max(covidcomposite),
            respcomposite = max(respcomposite),
            anydeath = max(anydeath),
            anyadmitted = max(anyadmitted),
            nov26 = max(nov26)) %>%
  subset(nov26 == 1)


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
  

