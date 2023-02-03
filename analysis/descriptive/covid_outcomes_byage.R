
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

## Function for rounding
redact <- function(vars) {
  case_when(vars > 5 ~ vars)
}

rounding <- function(vars) {
  round(vars / 7) * 7
}

#######################################
# Prepare data
#######################################

# Read in data
outcomes <- read_feather(here::here("output", "input_fourth.feather")) %>% 
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%
  subset(age >= 45 & age < 55) %>%
  group_by(age) %>%
  # Calculate denominator (num people per age year)
    # Everyone alive at 1 Oct 22 (ignore deaths for now)
  mutate(total_age1 = n()) %>%
  ungroup() %>%
  dplyr::select(!c(sex, age_cat, flu_vax_med_date, 
                   flu_vax_tpp_date, flu_vax_clinical_date,
                   ethnicity, region, imd,
                   contains("covid_vax")))

# Convert to long
outcomes_long <- outcomes %>%
  melt(id = c("patient_id", "age", "total_age1"),
       value.name = "date", na.rm = TRUE) %>%
  # Create week variable
  mutate(week = floor_date(as.Date(date), unit="week", week_start = 1)) %>%
  subset(week >= as.Date("2022-10-02"))

# Calculate number of outcomes of each type per week
  # Count all events for now (not just first)
outcomes_sum_1 <- outcomes_long %>%
  group_by(age, total_age1, week, variable) %>%
  summarise(cnt = n_distinct(patient_id)) %>%
  mutate(outcome = case_when(
                      variable == "covidadmitted_date" ~ "COVID admission",
                      variable == "coviddeath_date" ~ "COVID death",
                      variable == "any_death_date" ~ "Any death",
                      variable == "admitted_unplanned_date" ~ "Any unplanned admission",
                      variable == "covidemergency_date" ~ "COVID ED",
                      variable == "emergency_date" ~ "Any ED")) %>%
  select(!variable)

# Calculate number of composite outcome (COVID admission/death) of each type per week
  # Count all events for now (not just first)
  # If multiple in a given week count once
outcomes_sum_2 <- outcomes_long %>%
  subset(variable %in% c("covidadmitted_date", "coviddeath_date", "covidemergency_date")) %>%
  group_by(age, total_age1, week) %>%
  summarise(cnt = n_distinct(patient_id)) %>%
  mutate(outcome = "COVID death or admission or ED")

# Combine all outcomes counts together into one file
outcomes_byweek <- rbind(outcomes_sum_1, outcomes_sum_2) %>%
  arrange(age, outcome, week) %>%
  group_by(age, total_age1, outcome) %>%
  # Fill in missing weeks
  complete(week = seq(min(as.Date("2022-10-03")),
                      max(as.Date("2022-12-25")), by = '1 week')) %>%
  # Rounding
  mutate(
         cnt = case_when(cnt > 5 ~ cnt),
         cnt = round(cnt / 7) * 7,
         total_age1 = case_when(total_age1 > 5 ~ total_age1),
         total_age1 = round(total_age1 / 7) * 7,
         # Calculate rates per 100,000
         rate = cnt / total_age1 * 100000,
         age = as.character(age))

# Save 
write_csv(outcomes_byweek, here::here("output", "covid_outcomes", "covid_outcomes_by_week.csv"))


#############################################
# Number of events over entire study period 
#  (Oct 14 to latest available)
#############################################

outcomes_overall <- outcomes %>%
  group_by(age, total_age1) %>%
  # Count number of events by age
  summarise(covid_hosp = sum(covidadmitted_date > as.Date("2022-10-14"), na.rm= TRUE),
            covid_dth = sum(coviddeath_date > as.Date("2022-10-14"), na.rm= TRUE),
            covid_ed = sum(covidemergency_date > as.Date("2022-10-14"), na.rm= TRUE),
            any_dth = sum(any_death_date > as.Date("2022-10-14"), na.rm= TRUE),
            any_hosp = sum(admitted_unplanned_date > as.Date("2022-10-14"), na.rm= TRUE),
            any_ed = sum(emergency_date > as.Date("2022-10-14"), na.rm = TRUE),
            covid_composite = sum(
                  ((coviddeath_date > as.Date("2022-10-14"))|
                  (covidadmitted_date > as.Date("2022-10-14"))|
                  (covidemergency_date > as.Date("2022-10-14"))),
                  na.rm = TRUE)
                  ) %>%
  # Redaction
  mutate_at(c(vars(c("covid_hosp", "covid_dth", "any_dth", "any_hosp", "covid_ed",
                     "covid_composite","total_age1", "any_ed"))), redact) %>%
  # Rounding
  mutate_at(c(vars(c("covid_hosp", "covid_dth", "any_dth", "any_hosp", "covid_ed",
                     "covid_composite","total_age1", "any_ed"))), rounding) %>%
  # Calculate rates
  mutate(covid_hosp_rate = covid_hosp / total_age1 * 100000,
         covid_dth_rate = covid_dth / total_age1 * 100000,
         any_dth_rate = any_dth / total_age1 * 100000,
         covid_comp_rate = covid_composite / total_age1 * 100000,
         any_hosp_rate = any_hosp / total_age1 * 100000,
         covid_ed_rate = covid_ed / total_age1 * 100000,
         any_ed_rate = any_ed / total_age1 * 100000) 

# Save
write_csv(outcomes_overall, here::here("output", "covid_outcomes", "covid_outcomes_overall.csv"))


#######################################
# Plots
#######################################

### Number of event by week
ggplot(subset(outcomes_byweek, outcome != "Flu vaccination")) +
  geom_line(aes(x = week, y = rate, group = age, col = age),
            size = 1.25) +
  scale_color_brewer(palette = "RdBu") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .2))) +
  expand_limits(y=0) +
  scale_x_continuous(breaks = c(as.Date("2022-10-02"),
                                as.Date("2022-10-23"),
                                as.Date("2022-11-13"),
                                as.Date("2022-12-04"),
                                as.Date("2022-12-24")),
                     labels = c("Oct 2", "Oct 23", "Nov 13", "Dec 4", "Dec 24")) +
  facet_wrap(~ outcome, ncol = 2, scales = "free_y") +
  xlab(NULL) + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "plot_outcomes_byage.png"),
       dpi = 300, units = "in", width = 8, height = 6.25)

