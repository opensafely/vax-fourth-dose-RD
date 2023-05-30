
################################################################
# This script combines weekly measures into one
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
dir_create(here::here("output", "covid_outcomes"), showWarnings = FALSE, recurse = TRUE)

## Load functions
source(here::here("analysis", "custom_functions.R"))


#################################################################
# Read in individual measures files
#################################################################

covidcomposite <- read_csv(here::here("output", "measure_covidcomposite.csv"),
                               col_types = cols(
                                 date = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(!value)

covidadmitted <- read_csv(here::here("output", "measure_covidadmitted.csv"),
                           col_types = cols(
                             date = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(!c(population, value))

covidemergency <- read_csv(here::here("output", "measure_covidemergency.csv"),
                          col_types = cols(
                            date = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(!c(population, value))

anyadmitted <- read_csv(here::here("output", "measure_anyadmitted.csv"),
                          col_types = cols(
                            date = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(!c(population, value))

anydeath <- read_csv(here::here("output", "measure_anydeath.csv"),
                        col_types = cols(
                          date = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(!c(population, value))

respcomposite <- read_csv(here::here("output", "measure_respcomposite.csv"),
                        col_types = cols(
                          date = col_date(format = "%Y-%m-%d"))) %>%
  dplyr::select(!c(population, value))


# Combine into one
combined <- list(covidcomposite, anyadmitted, covidadmitted, covidemergency,
                 anydeath, respcomposite) %>% 
  reduce(full_join, by='date')

long <- combined %>% 
  reshape2::melt(id = c("date", "population"), value.name = "count") %>%
  mutate(count_mid6 = roundmid_any(count),
         population_mid6 = roundmid_any(population),
         week = as.Date(date)) %>%
  dplyr::select(!date)

long$variable <- factor(long$variable, levels = c("covidcomposite", "covidadmitted", "covidemergency",
                                                  "respcomposite", "anyadmitted", "anydeath"),
                        labels = c("COVID-19 unplanned admission,\nA&E attendance, death",
                                   "COVID-19 unplanned admission",
                                   "COVID-19 A&E attendance",
                                   "Respiratory unplanned admission",
                                   "Any unplanned admission",
                                   "Any death"))

# Save
write.csv(long, here::here("output", "descriptive", "outcomes_by_week.csv"), row.names = FALSE)

# Apply rounding
long_mid6 <- long %>%
  dplyr::select(!c(population, count)) %>%
  mutate(rate = count_mid6 / population_mid6 * 100000)

# Save
write.csv(here::here("output", "descriptive"), "outcomes_by_week_mid6.csv", row.names = FALSE)


ggplot(long_mid6, aes(x = week, y = rate, group = variable)) +
  geom_line(aes(col = variable)) +
  facet_wrap(~ variable, scales = "free_y") + 
  ylab("Rate per 100,000 population") + xlab("Week") +
  scale_x_continuous(breaks = c(as.Date("2022-09-01"), 
                                as.Date("2022-10-01"), 
                                as.Date("2022-11-01"),
                                as.Date("2022-12-01"),
                                as.Date("2023-01-01"),
                                as.Date("2023-02-01"))) +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(here::here("output", "descriptive", "outcomes_by_week.png"), dpi = 300,
       height = 5, width = 10, units = "in")
