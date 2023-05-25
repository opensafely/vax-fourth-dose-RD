
################################################################
# This script combines all outcomes files into one
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


###############################################################
# Outcomes start in November - Combine all outcomes files 
###############################################################

# Create list of all weekly outcomes files
list.files <- list("output/index/input_outcomes_2_2022-10-15.feather",
                   "output/index/input_outcomes_2_2022-10-22.feather",
                   "output/index/input_outcomes_2_2022-10-29.feather",
                   "output/index/input_outcomes_2_2022-11-05.feather",
                   "output/index/input_outcomes_2_2022-11-12.feather",
                   "output/index/input_outcomes_2_2022-11-19.feather",
                   "output/index/input_outcomes_2_2022-11-26.feather",
                   "output/index/input_outcomes_2_2022-12-03.feather",
                   "output/index/input_outcomes_2_2022-12-10.feather",
                   "output/index/input_outcomes_2_2022-12-17.feather",
                   "output/index/input_outcomes_2_2022-12-24.feather",
                   "output/index/input_outcomes_2_2022-12-31.feather",
                   "output/index/input_outcomes_2_2023-01-07.feather",
                   "output/index/input_outcomes_2_2023-01-14.feather",
                   "output/index/input_outcomes_2_2023-01-21.feather"
                #   "output/index/input_outcomes_2_2023-01-28.feather"
                )
index.dates <- map(list.files, read_feather)


# Combine together all weekly files 
  # Create one outcome file per outcome
combine_files <- function(var){
  
  list <- purrr::map(index.dates, 
                     ~ dplyr::select(., c(contains(var), patient_id, dob, dod, flu_vax_date, boost_date)))
  
  combined <- 
    bind_rows(list) %>% 
    unique() %>%
    mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") 
  
  # Wide to long
  data <- combined %>%
    reshape2::melt(id = c("patient_id", "dob", "dod", "boost_date", "flu_vax_date"), 
                   value.name = "date") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d", origin = "1970-01-01")) %>%
    dplyr::select(!c(variable)) %>%
    unique()
  
  print(paste0(var, " no. rows: ", nrow(data)))
  print(paste0(var, " no. people: ", n_distinct(data$patient_id)))
  
  return(data)
}


#### Save files COVID related outcomes ####
covidadmitted <- combine_files("covidadmitted")
write_feather(covidadmitted, here::here("output", "cohort", "outcomes_covidadmitted.feather"))

covidemerg <- combine_files("covidemergency")
write_feather(covidemerg, here::here("output", "cohort", "outcomes_covidemergency.feather"))

coviddeath <- combine_files("coviddeath")
write_feather(coviddeath, here::here("output", "cohort", "outcomes_coviddeath.feather"))

#### Save files other outcomes ####
respdeath <- combine_files("respdeath")
write_feather(respdeath, here::here("output", "cohort", "outcomes_respdeath.feather"))

respadmitted <- combine_files("respadmitted")
write_feather(respadmitted, here::here("output", "cohort", "outcomes_respadmitted.feather"))

anydeath <- combine_files("any_death")
write_feather(anydeath, here::here("output", "cohort", "outcomes_anydeath.feather"))

anyadmitted <- combine_files("admitted_unplanned")
write_feather(anyadmitted, here::here("output", "cohort", "outcomes_anyadmitted.feather"))
    