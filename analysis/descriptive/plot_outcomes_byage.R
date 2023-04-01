
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
dir_create(here::here("output", "covid_outcomes", "by_start_date"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "covid_outcomes", "figures"), showWarnings = FALSE, recurse = TRUE)

# Load functions
source(here::here("analysis", "custom_functions.R"))


data <- read.csv(here::here("output", "covid_outcomes", "by_start_date", "outcomes_byage_3mon_2022-11-26_red.csv"))
  
data2 <- data %>% dplyr::select(c(contains("rate_"), age_mos3)) %>%
  reshape2::melt(id = c("age_mos3"))


############################################################
### Plot event rate by age in months and index date
############################################################

ggplot(subset(data2, age_mos3 > 564 & age_mos3 < 636),
       aes(x = age_mos3 / 12, y = value)) + 
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_point() +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  scale_x_continuous(breaks = seq(47,53,1)) +
  facet_wrap(~ variable, ncol = 3, scales = "free_y") +
  xlab("Age in months") + ylab("No. events per 100,000") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "figures", "plot_outcomes.png"),
       dpi = 300, units = "in", width = 10, height = 7.5)

