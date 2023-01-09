###################################################################
# This script:
# - Calculates cumulative uptake of flu vaccination by week by age
###################################################################


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
dir_create(here::here("output", "cumulative_rates"), showWarnings = FALSE, recurse = TRUE)

## Function for rounding
rounding <- function(vars) {
  round(vars / 7) * 7
}

#######################################
# Prepare data
#######################################

# Read in data
flu_vax <- read_feather(here::here("output", "input_fourth.feather")) %>% 
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%
  subset(age >= 45 & age < 55) %>%
  
  # Consider earliest date the date of vaccination
  mutate(
    flu_vax_date = 
      pmin(flu_vax_tpp_date, flu_vax_med_date, flu_vax_clinical_date, na.rm = TRUE)) %>%
  group_by(age) %>%
  # Calculate denominator (num people per age year)
  # Everyone alive at 1 Oct 22 (ignore deaths for now)
  mutate(total_age1 = n(),
         week = floor_date(as.Date(flu_vax_date), unit="week")) %>%
  ungroup() %>%
  dplyr::select(c(patient_id, age, total_age1, flu_vax_date, week))


# Calculate number of outcomes of each type per week
# Count all events for now (not just first)
flu_vax_sum <- flu_vax %>%
  group_by(age, total_age1, week) %>%
  summarise(n_flu_vax = n_distinct(patient_id)) %>%

  # Fill in missing weeks
  complete(week = seq(min(as.Date("2022-07-01")),
                      max(as.Date("2022-12-25")), by = '1 week')) %>%
  
  group_by(age, total_age1) %>%
  mutate(n_flu_vax = replace_na(n_flu_vax, 0),
        cum_flu_vax = cumsum(n_flu_vax),
         cum_flu_vax = round(cum_flu_vax / 7) * 7,
         total_age1 = round(total_age1 / 7) * 7,
         # Calculate rates per 100
         rate = cum_flu_vax / total_age1 * 100,
         age = as.character(age)) %>%
  subset(week >= as.Date("2022-10-01"))

# Save
write_csv(flu_vax_sum, here::here("output", "cumulative_rates", "flu_vax_by_week.csv"))

#######################################
# Plots
#######################################

ggplot(subset(flu_vax_sum, age > 45 & age < 54)) +
  geom_line(aes(x = week, y = rate, group = age, col = age),
            size = 1.25) +
  scale_color_brewer(palette = "RdBu") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = c(as.Date("2022-10-02"), 
                                as.Date("2022-10-23"),
                                as.Date("2022-11-13"),
                                as.Date("2022-12-04"),
                                as.Date("2022-12-24")),
                     labels = c("Oct 2", "Oct 23", "Nov 13", "Dec 4", "Dec 24")) +
  xlab(NULL) + ylab("No. vaccinated (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0), 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "cumulative_rates", "plot_flu_vax_byage.png"),
       dpi = 300, units = "in", width = 6, height = 3.25)

