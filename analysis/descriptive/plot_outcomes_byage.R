
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

# Load functions
source(here::here("analysis", "custom_functions.R"))


#########################################
# Total events by age in months
#########################################

# Extract outcomes for pre-vax campaign and during campaign
index_date <- "2022-09-03"

covidcomposite_sep <- read_csv(here::here("output", "cohort", "outcomes_sep_all.csv"),
                               col_select = c(covidcomposite_date, dod, dob, patient_id),
                               col_types = cols(
                                 covidcomposite_date = col_date(format = "%Y-%m-%d"),
                                 dod = col_date(format = "%Y-%m-%d"),
                                 dob = col_date(format = "%Y-%m-%d"))) %>%
  mutate(start_date = "September 3",
         # Calculate age on index date
         age_mos = (dob %--% index_date) %/% months(1),
         age_mos = as.integer(age_mos),
         covidcomposite = if_else(!is.na(covidcomposite_date), 1, 0, 0)) %>%
  
  # Exclude people who died prior to index date
  subset((age_mos > 564 & age_mos < 636) &
           is.na(dod) | dod >= as.Date(index_date)) %>%
  
  group_by(age_mos) %>%
  mutate(# Denominator by age in months
    total = data.table::uniqueN(patient_id)) %>%
  group_by(age_mos, total, start_date) %>%
  # Create flag for people with outcome within follow-up window
  summarise(n_covidcomposite = sum(covidcomposite),
            n_covidcomposite = redact(n_covidcomposite),
            n_covidcomposite = rounding(n_covidcomposite),
            total = redact(total), 
            total = rounding(total),
            rate = n_covidcomposite / total * 100000) 

###

index_date = "2022-11-26"

covidcomposite_nov <- read_csv(here::here("output", "cohort", "outcomes_nov_covid.csv"),
                               col_select = c(covid_date, dod, dob, patient_id),
                               col_types = cols(
                                 covid_date = col_date(format = "%Y-%m-%d"),
                                 dod = col_date(format = "%Y-%m-%d"),
                                 dob = col_date(format = "%Y-%m-%d"))) %>%
  mutate(covidcomposite = if_else(covid_date >= as.Date("2022-11-26") &
                                    covid_date <= as.Date("2022-11-26") + 28,
                                  1, 0, 0)) %>%
  select(c(patient_id, dob, dod, covidcomposite)) %>%
  unique() %>%
  mutate(start_date = "November 26",
         # Calculate age on index date
         age_mos = (dob %--% index_date) %/% months(1),
         age_mos = as.integer(age_mos)) %>%
  
  # Exclude people who died prior to index date
  subset((age_mos > 564 & age_mos < 636) &
           is.na(dod) | dod >= as.Date(index_date)) %>%
  
  group_by(age_mos) %>%
  mutate(# Denominator by age in months
    total = data.table::uniqueN(patient_id)) %>%
  group_by(age_mos, total, start_date) %>%
  # Create flag for people with outcome within follow-up window
  summarise(n_covidcomposite = sum(covidcomposite),
            n_covidcomposite = redact(n_covidcomposite),
            n_covidcomposite = rounding(n_covidcomposite),
            total = redact(total), 
            total = rounding(total),
            rate = n_covidcomposite / total * 100000) 
  

# Combine data for plot and save
covidcomposite <- rbind(covidcomposite_sep, covidcomposite_nov)
  
# Save file for plot
write.csv(covidcomposite, here::here("output", "covid_outcomes", "plot_covidcomposite_age.csv"),
          row.names = FALSE)
  

############################################################
### Plot event rate by age in months and index date
############################################################

ggplot(subset(covidcomposite, age_mos > 564 & age_mos < 636)) + 
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_point(aes(x = age_mos / 12, y = rate, 
                 group = start_date, col = start_date)) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  scale_x_continuous(breaks = seq(47,53,1)) +
  xlab(NULL) + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "plot_covid_composite_age_date.png"),
       dpi = 300, units = "in", width = 8, height = 6.25)

