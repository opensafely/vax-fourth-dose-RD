
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
# Combine all outcomes files 
#######################################

# Create list of all weekly outcomes files
list.files <- dir_ls('output/index', regexp = "FALSE", invert = TRUE)
index.dates <- map(list.files, read_feather)

# Combine together all weekly files 
# Create one outcome file per outcome
  list <- purrr::map(index.dates, 
                     ~ dplyr::select(., admitted_unplanned_num) %>%
                        subset(!is.na(admitted_unplanned_num)))
  
  combined <- 
    bind_rows(list) %>% 
    unique() %>%
    summarise(admitted_unplanned_max = max(admitted_unplanned_num, na.rm = TRUE))
  
 print(combined)