
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

## Create directories
dir_create(here::here("output", "covid_outcomes"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)

## Function for rounding
redact <- function(vars) {
  case_when(vars > 7 ~ vars)
}

rounding <- function(vars) {
  round(vars / 5) * 5
}

#######################################
# Read in data
#######################################

primary_outcomes <- read_csv(here::here("output", "cohort", "primary_outcomes.csv")) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

secondary_outcomes <- read_csv(here::here("output", "cohort", "secondary_outcomes.csv")) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))


#########################################
# Total events by age 
#########################################

#### Function to calculate total COVID events by index date ####
primary <- function(index_date){

primary <- primary_outcomes  %>%
  # Exclude people who died prior to index date
  subset(is.na(dod) | dod >= as.Date(index_date)) %>%
  
  # Calculate age on index date
  mutate(age_mos = (dob %--% as.Date(index_date)) %/% months(1),
         age_yrs = (dob %--% as.Date(index_date)) %/% years(1)) %>%

  group_by(age_mos) %>%
  mutate(# Denominator by age in months
         total = uniqueN(patient_id),
         
         event = if_else(date > as.Date(index_date) + 28 |
                                  date < as.Date(index_date),
                                  1, 0, 0)) %>%
  group_by(patient_id, age_mos, total) %>%
  # Create flag for people with outcome within follow-up window
  summarise(event = max(event))

primary_by_age <- primary %>%
  group_by(age_mos, total) %>%
  summarise(n_covid_composite = sum(event)) %>%
  mutate_at(c(vars(n_covid_composite, total)), redact) %>%
  mutate_at(c(vars(n_covid_composite, total)), rounding) %>%
  mutate(rate_covid_composite = n_covid_composite / total * 100000,
         index_date = index_date)

return(primary_by_age)

}

primary_by_age_sep06 <- primary("2022-09-06")
primary_by_age_oct15 <- primary("2022-10-15")
primary_by_age_nov26 <- primary("2022-11-26")

# 
# #### Function to calculate total secondary events by index date ####
# secondary <- function(index_date){
#   
#   secondary <- secondary_outcomes  %>%
#     # Exclude people who died prior to index date
#     subset(is.na(dod) | dod >= as.Date(index_date)) %>%
#     
#     # Calculate age on index date
#     mutate(age_mos = (dob %--% as.Date(index_date)) %/% months(1),
#            age_yrs = (dob %--% as.Date(index_date)) %/% years(1)) %>%
#     
#     group_by(age_mos) %>%
#     mutate(# Denominator by age in months
#       total = uniqueN(patient_id),
#       
#       event = if_else(date > as.Date(index_date) + 28 |
#                         date < as.Date(index_date),
#                       1, 0, 0)) %>%
#     group_by(patient_id, age_mos, total) %>%
#     # Create flag for people with outcome within follow-up window
#     summarise(event = max(event))
#   
#   secondary_by_age <- secondary %>%
#     group_by(age_mos, total) %>%
#     summarise(n_covid_composite = sum(event)) %>%
#     mutate_at(c(vars(n_covid_composite, total)), redact) %>%
#     mutate_at(c(vars(n_covid_composite, total)), rounding) %>%
#     mutate(pcent_covid_composite = n_covid_composite / total * 100)
#   
#   return(secondary_by_age)
#}


#######################################
# Plots
#######################################

primary_by_age_all <- 
  rbind(primary_by_age_sep06, primary_by_age_oct15, primary_by_age_nov26)

write.csv(primary_by_age_all, here::here("output", "covid_outcomes", "primary_by_age_all.csv"),
          row.names = FALSE)

### Number of event by week
ggplot(subset(primary_by_age_all, age_mos > 564 & age_mos < 636)) + 
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_point(aes(x = age_mos / 12, y = pcent_covid_composite, 
                group = index_date, col = index_date)) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  scale_x_continuous(breaks = seq(47,53,1)) +
  facet_wrap(~ index_date, ncol = 2, scales = "free_y") +
  xlab(NULL) + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "plot_outcomes_byage.png"),
       dpi = 300, units = "in", width = 8, height = 6.25)
