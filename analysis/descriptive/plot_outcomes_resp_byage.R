
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
respadmit_sep <- read_csv(here::here("output", "cohort", "outcomes_sep_all.csv"))

print(nrow(respadmit_sep))

respadmit_sep <- respadmit_sep %>%
  dplyr::select(c(respadmitted, dod, dob, patient_id)) %>%
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
  summarise(n_respadmit = sum(respadmitted == 1)) %>%
  mutate(rate = n_respadmit / total * 100000) 

respadmit_sep_red <- respadmit_sep %>%
  mutate(n_respadmit = redact(n_respadmit),
            n_respadmit = rounding(n_respadmit),
            total = redact(total), 
            total = rounding(total),
            rate = n_respadmit / total * 100000) 

###

respadmit_nov <- read_csv(here::here("output", "cohort", "outcomes_nov_respadmitted.csv"))

print(nrow(respadmit_nov))

respadmit_nov <- respadmit_nov %>%
  mutate(respadmit = if_else(var == 1, 1, 0, 0)) %>%
  dplyr::select(c(patient_id, dob, dod, var)) %>%
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
  summarise(n_respadmit = sum(var == 1)) %>%
  mutate(rate = n_respadmit / total * 100000) 

respadmit_nov_red <- respadmit_nov %>%
  mutate(n_respadmit = redact(n_respadmit),
         n_respadmit = rounding(n_respadmit),
         total = redact(total), 
         total = rounding(total),
         rate = n_respadmit / total * 100000) 

# Combine data for plot and save
respadmit <- rbind(respadmit_sep, respadmit_nov)
respadmit_red <- rbind(respadmit_sep_red, respadmit_nov_red)

# Save file for plot
write.csv(respadmit, here::here("output", "covid_outcomes", "plot_respadmit_age.csv"),
          row.names = FALSE)
write.csv(respadmit_red, here::here("output", "covid_outcomes", "plot_respadmit_age_redacted.csv"),
          row.names = FALSE)
  

############################################################
### Plot event rate by age in months and index date
############################################################

ggplot(subset(respadmit_red, age_mos > 564 & age_mos < 636),
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

ggsave(here::here("output", "covid_outcomes", "plot_respadmit_age_date.png"),
       dpi = 300, units = "in", width = 8, height = 6.25)

