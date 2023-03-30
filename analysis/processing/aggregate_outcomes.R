
################################################################
# This script:
# - Calculates number of outcomes by week by age
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
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "covid_outcomes","by_start_date"), showWarnings = FALSE, recurse = TRUE)

# Load functions
source(here::here("analysis", "custom_functions.R"))


#########################################
# Total events by age in months
#########################################

agg1 <- function(start_date){
  
data <- read.csv(here::here("output", "cohort", paste0("outcomes_",start_date,".csv"))) %>%
  group_by(age_mos) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  group_by(age_mos, total) %>%
  summarise(n_covidcomposite = sum(covidcomposite == 1, na.rm = TRUE),
            n_covidadmitted = sum(covidadmitted == 1, na.rm = TRUE),
            n_coviddeath = sum(coviddeath == 1, na.rm = TRUE),
            n_covidemerg = sum(covidemerg == 1, na.rm = TRUE),
            n_respcomposite = sum(respcomposite ==1, na.rm = TRUE),
            n_respdeath = sum(respdeath == 1, na.rm = TRUE),
            n_respadmitted = sum(respadmitted == 1, na.rm = TRUE),
            n_anydeath = sum(anydeath ==1, na.rm = TRUE),
            n_anyadmitted = sum(anyadmitted == 1, na.rm = TRUE)) 

write.csv(data, here::here("output", "covid_outcomes", "by_start_date", paste0("outcomes_byage_",start_date,".csv")), row.names = FALSE)


data2 <- data %>%
  mutate(across(contains("n_"), redact),
         across(contains("n_"), rounding),
         rate_covidcomposite = n_covidcomposite / total * 100000,
         rate_covidadmitted = n_covidadmitted / total * 100000,
         rate_coviddeath = n_coviddeath / total * 100000,
         rate_covidemerg = n_covidemerg / total * 100000,
         rate_respcomposite = n_respcomposite / total * 100000,
         rate_respdeath = n_respdeath / total * 100000,
         rate_respadmitted = n_respadmitted / total * 100000,
         rate_anydeath = n_anydeath / total * 100000,
         rate_anyadmitted = n_anyadmitted / total * 100000)

write.csv(data2, here::here("output", "covid_outcomes", "by_start_date", paste0("outcomes_byage_",start_date,"_red.csv")), row.names = FALSE)

}


# Do the above over all relevant start dates
agg1("2022-09-03")
agg1("2022-10-15")

start_dates <- as.Date(0:10, origin = "2022-11-26")
sapply(start_dates,agg1)

