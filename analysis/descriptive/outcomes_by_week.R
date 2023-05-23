################################################################
# This script:
# - Plot outcome rate over time
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

## Create directories
dir_create(here::here("output", "index"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = TRUE)

## Load functions
source(here::here("analysis", "custom_functions.R"))


#####################################################
### Read in data                                  ###
#####################################################


byweek <- function(dat){
  
  
  df <- feather::read_feather(here::here("output", "index", paste0("index_outcomes_2_", dat, ".feather"))) %>%
          mutate(week = dat, total = n()) %>%
          group_by(week, total) %>%
          summarise(n_covidadmitted = sum(!is.na(covidadmitted_date_1)),
                    n_covidemerg = sum(!is.na(covidemergency_date_1)),
                    n_covidcomp = sum(!is.na(covidadmitted_date_1) |
                                        !is.na(covidemergency_date_1) |
                                        (!is.na(coviddeath_date) &
                                           coviddeath_date >= as.Date(dat) &
                                           coviddeath_date < as.Date(dat) + 6)),
                    n_respcomp = sum(!is.na(respadmitted_date_1) |
                                       (!is.na(respdeath_date) &
                                          respdeath_date >= as.Date(dat) &
                                          respdeath_date < as.Date(dat) + 6)),
                    n_anydeath = sum(!is.na(anydeath_date) &
                                       anydeath_date >= as.Date(dat) &
                                       anydeath_date < as.Date(dat) + 6)) %>%
          mutate(n_roundmid6 = roundmid_any(n, 6),
                    total_roundmid6 = roundmid_any(total, 6),
                    rate_roundmid6 = n / total * 100000) %>%
          select(!c(n, total))
  
 return(df)   
  
}


# Do the above over all relevant start dates
start_dates <- seq(as.Date("2022-09-03"), as.Date("2022-12-10"), by = 7)
sapply(start_dates, byweek)

all <- rbind(
              byweek("2022-09-03"),
              byweek("2022-09-10"),
              byweek("2022-09-17"),
              byweek("2022-09-24"),
              byweek("2022-10-01"),
              byweek("2022-10-08"),
              byweek("2022-10-15"),
              byweek("2022-10-22"),
              byweek("2022-10-29"),
              byweek("2022-11-05"),
              byweek("2022-11-12"),
              byweek("2022-11-19"),
              byweek("2022-11-26"),
              byweek("2022-12-03"),
              byweek("2022-12-10"),
              byweek("2022-12-17"),
              byweek("2022-12-24")
          )

write.csv(all, here::here("output", "descriptive", "outcomes_by_week.csv"))
