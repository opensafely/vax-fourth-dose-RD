
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
covidcomposite_sep <- read_csv(here::here("output", "cohort", "outcomes_sep_all.csv"))

print(nrow(covidcomposite_sep))

covidcomposite_sep <- covidcomposite_sep %>%
  dplyr::select(c(covidcomposite, dod, dob, patient_id)) %>%
  mutate(start_date = "September 3",
         # Calculate age on index date
         age_mos = (dob %--% "2022-09-03") %/% months(1)) %>%
  # Exclude people who died prior to index date
  subset(age_mos > 564 & age_mos < 636 &
           (is.na(dod) | dod >= as.Date("2022-09-03"))) %>%
  group_by(age_mos) %>%
  mutate(# Denominator by age in months
    total = n()) %>%
  ungroup() %>%
  group_by(age_mos, total, start_date) %>%
  # Create flag for people with outcome within follow-up window
  summarise(n_covidcomposite = sum(covidcomposite == 1)) %>%
  mutate(rate = n_covidcomposite / total * 100000) 

covidcomposite_sep_red <- covidcomposite_sep %>%
  mutate(n_covidcomposite = redact(n_covidcomposite),
            n_covidcomposite = rounding(n_covidcomposite),
            total = redact(total), 
            total = rounding(total),
            rate = n_covidcomposite / total * 100000) 

###

covidcomposite_nov <- read_csv(here::here("output", "cohort", "outcomes_nov_covid.csv"))

print(nrow(covidcomposite_nov))

covidcomposite_nov <- covidcomposite_nov %>%
  mutate(covidcomposite = if_else(var == 1, 1, 0, 0)) %>%
  dplyr::select(c(patient_id, dob, dod, covidcomposite)) %>%
  unique() %>%
  mutate(start_date = "November 26",
         # Calculate age on index date
         age_mos = (dob %--% "2022-11-26") %/% months(1)) %>%
  
  # Exclude people who died prior to index date
  subset((age_mos > 564 & age_mos < 636) &
           (is.na(dod) | dod >= as.Date("2022-11-26"))) %>%
  
  group_by(age_mos) %>%
  mutate(# Denominator by age in months
    total = n()) %>%
  ungroup() %>%
  group_by(age_mos, total, start_date) %>%
  # Create flag for people with outcome within follow-up window
  summarise(n_covidcomposite = sum(covidcomposite == 1)) %>%
  mutate(rate = n_covidcomposite / total * 100000) 

covidcomposite_nov_red <- covidcomposite_nov %>%
  mutate(n_covidcomposite = redact(n_covidcomposite),
         n_covidcomposite = rounding(n_covidcomposite),
         total = redact(total), 
         total = rounding(total),
         rate = n_covidcomposite / total * 100000) 

# Combine data for plot and save
covidcomposite <- rbind(covidcomposite_sep, covidcomposite_nov)
covidcomposite_red <- rbind(covidcomposite_sep_red, covidcomposite_nov_red)

# Save file for plot
write.csv(covidcomposite, here::here("output", "covid_outcomes", "plot_covidcomposite_age.csv"),
          row.names = FALSE)
write.csv(covidcomposite_red, here::here("output", "covid_outcomes", "plot_covidcomposite_age_redacted.csv"),
          row.names = FALSE)
  

############################################################
### Plot event rate by age in months and index date
############################################################

ggplot(subset(covidcomposite_red, age_mos > 564 & age_mos < 636),
       aes(x = age_mos / 12, y = rate,
           group = start_date, col = start_date)) + 
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  scale_x_continuous(breaks = seq(47,53,1)) +
  facet_wrap(~ start_date, nrow = 2) +
  xlab("Age in months") + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "plot_covidcomposite_age_date.png"),
       dpi = 300, units = "in", width = 8, height = 6.25)

