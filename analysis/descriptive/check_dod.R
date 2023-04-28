

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


###########################################
# Check that death variables looks reasonable
###########################################


check <- read_feather(here::here("output", "input_baseline.feather")) 

deaths <- check %>%
  group_by(dod) %>%
  summarise(n_died = n()) 

write_csv(deaths, here::here("output", "descriptive", "dod_check_inputbaseline.csv"))


####


check <- read_feather(here::here("output", "index", "input_outcomes_1_2022-09-03.feather")) 

deaths <- check %>%
  group_by(dod) %>%
  summarise(n_died = n()) 

write_csv(deaths, here::here("output", "descriptive", "dod_check_input2022-09-03.csv"))
          
deaths <- check %>%
  group_by(any_death_date) %>%
  summarise(n_died = n()) 

write_csv(deaths, here::here("output", "descriptive", "anydeath_check_input2022-09-03.csv"))


####


check <- read_feather(here::here("output", "index", "input_outcomes_2_2022-11-26.feather")) 

deaths <- check %>%
  group_by(dod) %>%
  summarise(n_died = n()) 

write_csv(deaths, here::here("output", "descriptive", "dod_check_input2022-11-26.csv"))

deaths <- check %>%
  group_by(any_death_date) %>%
  summarise(n_died = n()) 

write_csv(deaths, here::here("output", "descriptive", "anydeath_check_input2022-11-26.csv"))


####


check <- read_csv(here::here("output", "cohort",  "outcomes_2022-09-03.csv")) 

deaths <- check %>%
  group_by(dod) %>%
  summarise(n_died = n()) 

write_csv(deaths, here::here("output", "descriptive", "dod_check_outcome_2022-09-03.csv"))


####


check <- read_csv(here::here("output", "cohort", "outcomes_2022-11-26.csv")) 

deaths <- check %>%
  group_by(dod) %>%
  summarise(n_died = n()) 

write_csv(deaths, here::here("output", "descriptive", "dod_check_outcome_2022-11-26.csv"))