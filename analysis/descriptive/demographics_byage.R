###################################################################
# This script:
# - Calculates other variables of interest by week by age
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
dir_create(here::here("output", "variables_by_age"), showWarnings = FALSE, recurse = TRUE)

## Function for rounding
rounding <- function(vars) {
  round(vars / 7) * 7
}

#######################################
# IMD
#######################################

# Read in data
imd <- read_feather(here::here("output", "input_fourth.feather")) %>% 
  subset(age >=40 & age < 60) %>%
  group_by(age) %>%
  mutate(total_age1 = n()) %>%
  group_by(age, imd, total_age1) %>% 
  tally() %>%
  mutate(n = round(n / 7) * 7,
         total_age1 = round(total_age1 / 7) * 7,
         pcent = n / total_age1 * 100,
         var = "IMD") %>%
  rename(category = imd)

imd$category <- factor(imd$category, levels = c("0", "1", "2", "3", "4", "5"),
                  labels = c("Missing", "1 most deprived", "2", "3", "4", "5 least deprived"))


ggplot(subset(imd, category != "Missing")) +
  geom_point(aes(x = age, y = pcent, group = age, col = category), 
            size = 1.25) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_continuous(breaks = seq(40,60,5)) +
  scale_color_brewer(palette = "Spectral") +
  facet_wrap(~ category, ncol = 2) +
  xlab("Age") + ylab("Percentage (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0), 
        legend.title = element_blank())

ggsave(here::here("output", "variables_by_age", "plot_imd_by_age.png"),
       dpi = 300, units = "in", width = 7.5, height = 6)




#######################################
# Sex
#######################################

# Read in data
sex <- read_feather(here::here("output", "input_fourth.feather")) %>% 
  subset(age >=40 & age < 60) %>%
  group_by(age) %>%
  mutate(total_age1 = n()) %>%
  group_by(age, sex, total_age1) %>% 
  tally() %>%
  mutate(n = round(n / 7) * 7,
         total_age1 = round(total_age1 / 7) * 7,
         pcent = n / total_age1 * 100,
         var = "Sex")%>%
  rename(category = sex)

sex$category <- factor(sex$category, levels = c("M", "F"),
                  labels = c("Male", "Female"))


ggplot(subset(sex, category == "Male")) +
  geom_point(aes(x = age, y = pcent, group = age, col = category), 
             size = 1.25) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(40,60,5)) +
  scale_color_brewer(palette = "Spectral") +
  xlab("Age") + ylab("Percentage (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0), 
        legend.title = element_blank())

ggsave(here::here("output", "variables_by_age", "plot_sex_by_age.png"),
       dpi = 300, units = "in", width = 5, height = 2.5)



#######################################
# Region
#######################################

# Read in data
region <- read_feather(here::here("output", "input_fourth.feather")) %>% 
  subset(age >=40 & age < 60) %>%
  group_by(age) %>%
  mutate(total_age1 = n()) %>%
  group_by(age, region, total_age1) %>% 
  tally() %>%
  mutate(n = round(n / 7) * 7,
         total_age1 = round(total_age1 / 7) * 7,
         pcent = n / total_age1 * 100,
         var = "Region")%>%
  rename(category = region)


ggplot(subset(region, category!= "Missing")) +
  geom_point(aes(x = age, y = pcent, group = age, col = category), 
             size = 1.25) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_continuous(breaks = seq(40,60,5)) +
  scale_color_brewer(palette = "Spectral") +
  facet_wrap(~ category, ncol = 2) +
  xlab("Age") + ylab("Percentage (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0), 
        legend.title = element_blank())

ggsave(here::here("output", "variables_by_age", "plot_region_by_age.png"),
       dpi = 300, units = "in", width = 7.5, height = 8)




#######################################
# Ethnicity
#######################################

# Read in data
ethnicity <- read_feather(here::here("output", "input_fourth.feather")) %>% 
  subset(age >=40 & age < 60) %>%
  group_by(age) %>%
  mutate(total_age1 = n()) %>%
  group_by(age, ethnicity, total_age1) %>% 
  tally() %>%
  mutate(n = round(n / 7) * 7,
         total_age1 = round(total_age1 / 7) * 7,
         pcent = n / total_age1 * 100,
         var = "Ethnicity")%>%
  rename(category = ethnicity)


ggplot(ethnicity) +
  geom_point(aes(x = age, y = pcent, group = age, col =category), 
             size = 1.25) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(40,60,5)) +
  scale_color_brewer(palette = "Spectral") +
  facet_wrap(~ category, ncol = 2) +
  xlab("Age") + ylab("Percentage (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0), 
        legend.title = element_blank())

ggsave(here::here("output", "variables_by_age", "plot_ethnicity_by_age.png"),
       dpi = 300, units = "in", width = 7.5, height = 6)

########################


demographics_by_age <- rbind(imd, sex, region, ethnicity)

# Save
write_csv(demographics_by_age, here::here("output", "variables_by_age", "demographics_by_age.csv"))
