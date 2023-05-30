# 
# ################################################################
# # This script performs data cleaning and preparation
# ################################################################
# 
# 
# # For running locally only #
# # setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/vax-fourth-dose-RD")
# # getwd()
# 
# # Import libraries #
# library('tidyverse')
# library('lubridate')
# library('arrow')
# library('here')
# library('reshape2')
# library('dplyr')
# library('fs')
# library('ggplot2')
# library('RColorBrewer')
# library('lubridate')
# library('purrr')
# 
# 
# dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)
# dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = TRUE)
# 
# 
# anyadmitted <- read.csv(here::here("output", "cohort", "outcomes_nov_anyadmitted.csv")) %>%
#   mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
#   subset(!is.na(date) & date >= as.Date("2022-11-01") & date < as.Date("2023-03-01"))
# 
# anydeath <- read.csv(here::here("output", "cohort", "outcomes_nov_anydeath.csv")) %>%
#   mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
#   subset(!is.na(date) & date >= as.Date("2022-11-01") & date < as.Date("2023-03-01"))
# 
# anyadmit_bydate <- anyadmitted %>%
#   group_by(date) %>%
#   summarise(n = n())
# 
# anydeath_bydate <- anydeath %>%
#   group_by(date) %>%
#   summarise(n = n())
# 
# 
# # Check latest admission date
# ggplot(anyadmit_bydate) +
#   geom_bar(aes(x = date, y = n), stat = "identity") +
#   theme_bw()
# 
# ggsave(here::here("output", "descriptive", "over_time_admit.png"),
#        dpi = 300, units = "in", width = 3, height = 3)
# 
# 
# # Check latest admission date
# ggplot(anydeath_bydate) +
#   geom_bar(aes(x = date, y = n), stat = "identity") +
#   theme_bw()
# 
# ggsave(here::here("output", "descriptive", "over_time_death.png"),
#        dpi = 300, units = "in", width = 3, height = 3)