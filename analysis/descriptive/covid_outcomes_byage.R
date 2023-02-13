
################################################################
# This script:
# - Calculates number of outcomes by week and by age
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
  case_when(vars > 7 ~ vars)
}

rounding <- function(vars) {
  round(vars / 5) * 5
}

#######################################
# Prepare data
#######################################

# Read in data
outcomes <- read_feather(here::here("output", "input_fourth.feather")) %>% 
  mutate_at(c(vars(c(contains("_date")))), as.Date, format = "%Y-%m-%d") %>%
  subset(age >= 40 & age < 60) %>%
  ungroup() %>%
  dplyr::select(!c(sex, age_cat, flu_vax_med_date, 
                   flu_vax_tpp_date, flu_vax_clinical_date,
                   ethnicity, region, imd,
                   contains("covid_vax"))) %>%
  # Total number of people
  mutate(total = n_distinct(patient_id)) %>%
  # Number of people per year of age
  group_by(age) %>%
  mutate(total_age1 = n_distinct(patient_id))
  
# Convert to long
outcomes_long <- outcomes %>%
  melt(id = c("patient_id", "age", "total_age1", "total"),
       value.name = "date", na.rm = TRUE) %>%
  # Create week variable
  mutate(week = floor_date(as.Date(date), unit="week", week_start = 5)) %>%
  subset(week >= as.Date("2022-10-28") & week < as.Date("2023-01-27")) 



#################################################################
# Calculate number of outcomes of each type per week overall
#################################################################

  # Count all events for now (not just first)
outcomes_sum_1 <- outcomes_long %>%
  group_by(week, variable, total) %>%
  mutate(cnt = n_distinct(patient_id),
         outcome = case_when(
           variable == "covidadmitted_date" ~ "COVID admission",
           variable == "coviddeath_date" ~ "COVID death",
           variable == "any_death_date" ~ "Any death",
           variable == "admitted_unplanned_date" ~ "Any unplanned admission",
           variable == "covidemergency_date" ~ "COVID ED",
           variable == "emergency_date" ~ "Any ED")) %>%
  group_by(week, outcome, total, cnt) %>%
  summarise()


# Calculate number of composite outcome (COVID admission/death) of each type per week
  # Count all events for now (not just first)
  # If multiple in a given week count once
outcomes_sum_2 <- outcomes_long %>%
  subset(variable %in% c("covidadmitted_date", "coviddeath_date", "covidemergency_date")) %>%
  group_by(week, total) %>%
  mutate(cnt = n_distinct(patient_id),
         outcome = "COVID death or admission or ED") %>%
  group_by(week, outcome, total, cnt) %>%
  summarise()

# Combine all outcomes counts together into one file
outcomes_byweek <- rbind(outcomes_sum_1, outcomes_sum_2) %>%
  arrange(outcome, week) %>%
  group_by(outcome) %>%
  # Fill in missing weeks
  complete(week = seq(min(as.Date("2022-10-28")),
                      max(as.Date("2023-01-20")), by = '1 week')) %>%
  # Rounding
  mutate(
         cnt = case_when(cnt > 7 ~ cnt),
         cnt = round(cnt / 5) * 5,
         total = case_when(total > 7 ~ total),
         total = round(total / 5) * 5,
         # Calculate rates per 100,000
         rate = cnt / total * 100000)

# Save 
write_csv(outcomes_byweek, here::here("output", "covid_outcomes", "covid_outcomes_by_week.csv"))


#############################################
# Number of events over entire study period 
#  (Oct 28 to latest available)
#############################################

# Individual outcomes - starting 2 weeks post-campaign
outcomes_overall <- outcomes_long %>%
  group_by(age, total_age1, variable) %>%
  mutate(cnt = n_distinct(patient_id),
         index_dt = "2 weeks post-campaign",
         variable = case_when(
           variable == "covidadmitted_date" ~ "COVID unplanned admission",
           variable == "coviddeath_date" ~ "COVID death",
           variable == "covidemergency_date" ~ "COVID A&E visit",
           variable == "any_death_date" ~ "Any death",
           variable == "admitted_unplanned_date" ~ "Any unplanned admission",
           variable == "emergency_date" ~ "Any A&E visit"
         )) %>%
  group_by(index_dt, age, cnt, total_age1, variable) %>%
  summarise()
  
# Composite outcome - starting 2 weeks post-campaign
outcomes_comp <- outcomes_long %>% 
  subset(variable %in% c("covidadmitted_date","coviddeath_date","covidemergency_date")) %>%
  group_by(age, total_age1) %>%
  mutate(cnt = n_distinct(patient_id),
         index_dt = "2 weeks post-campaign",
         variable = "COVID death or hospitalisation or A&E visit") %>%
  group_by(index_dt, age, cnt, total_age1, variable) %>%
  summarise()


# Individual outcomes - starting 4 weeks post-campaign
outcomes_overall_2 <- outcomes_long %>%
  subset(date >= as.Date("2022-11-11")) %>%
  group_by(age, total_age1, variable) %>%
  mutate(cnt = n_distinct(patient_id),
         index_dt = "4 weeks post-campaign",
         variable = case_when(
           variable == "covidadmitted_date" ~ "COVID unplanned admission",
           variable == "coviddeath_date" ~ "COVID death",
           variable == "covidemergency_date" ~ "COVID A&E visit",
           variable == "any_death_date" ~ "Any death",
           variable == "admitted_unplanned_date" ~ "Any unplanned admission",
           variable == "emergency_date" ~ "Any A&E visit"
         )) %>%
  group_by(index_dt, age, cnt, total_age1, variable) %>%
  summarise()

# Composite outcome - starting 4 weeks post-campaign
outcomes_comp_2 <- outcomes_long %>% 
  subset(date >= as.Date("2022-11-11") &
           variable %in% c("covidadmitted_date","coviddeath_date","covidemergency_date")) %>%
  group_by(age, total_age1) %>%
  mutate(cnt = n_distinct(patient_id),
         index_dt = "4 weeks post-campaign",
         variable = "COVID death or hospitalisation or A&E visit") %>%
  group_by(index_dt, age, cnt, total_age1, variable) %>%
  summarise()

# Combine
outcomes_overall_both <- rbind(outcomes_overall, outcomes_comp, outcomes_overall_2,
                               outcomes_comp_2) %>%
  rename(outcome = variable) %>%
  mutate(cnt = redact(cnt),
         cnt = rounding(cnt),
         total_age1 = redact(total_age1),
         total_age1 = rounding(total_age1),
         rate = cnt / total_age1 * 100000)

          
# Save
write_csv(outcomes_overall_both, here::here("output", "covid_outcomes", "covid_outcomes_overall.csv"))


#######################################
# Plots
#######################################

### Number of event by week
ggplot(subset(outcomes_byweek, outcome != "Flu vaccination")) +
  geom_line(aes(x = week, y = rate, col = outcome),
            size = 1.25) +
  scale_color_brewer(palette = "Spectral") +
  scale_y_continuous(expand = c(0.2, 0)) +
  scale_x_continuous(breaks = c(as.Date("2022-10-01"),
                                as.Date("2022-11-01"),
                                as.Date("2022-12-01"),
                                as.Date("2023-01-01")),
                     labels = c("Oct 1","Nov 1","Dec 1","Jan 1")) +
  facet_wrap(~ outcome, ncol = 2, scales = "free_y") +
  xlab(NULL) + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "plot_outcomes_byage.png"),
       dpi = 300, units = "in", width = 6.5, height = 6.25)



### Total events by age

# 2 weeks post-campaign
ggplot(subset(outcomes_overall_both, index_dt %in% c("2 weeks post-campaign"))) +
  geom_line(aes(x = age, y = rate, group = outcome, col = outcome),size = 1.25) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  scale_x_continuous(breaks = seq(40,60,5)) +
  scale_y_continuous(expand = c(0.2, 0)) +
  scale_color_brewer(palette = "Spectral") +
  facet_wrap(~ outcome, ncol = 3, scales = "free_y") +
  xlab(NULL) + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),legend.position = "none")

ggsave(here::here("output", "covid_outcomes", "plot_outcomes_byage_2wk.png"),
       dpi = 300, units = "in", width = 8, height = 6.25)


# 4 weeks post-campaign
ggplot(subset(outcomes_overall_both,  index_dt %in% c("2 weeks post-campaign"))) +
  geom_line(aes(x = age, y = rate, group = outcome, col = outcome),size = 1.25) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  scale_x_continuous(breaks = seq(40,60,5)) +
  scale_y_continuous(expand = c(.2, 0)) +
  scale_color_brewer(palette = "Spectral") +
  facet_wrap(~ outcome, ncol = 3, scales = "free_y") +
  xlab(NULL) + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(),legend.position = "none")

ggsave(here::here("output", "covid_outcomes", "plot_outcomes_byage_4wk.png"),
       dpi = 300, units = "in", width = 8.5, height = 6.25)