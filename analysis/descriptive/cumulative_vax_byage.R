
################################################################
# This script:
# - Calculates cumulative uptake of booster dose/second booster
#    COVID-19 vaccine by age
################################################################


# For running locally only #
# setwd("C:/Users/aschaffer/OneDrive - Nexus365/Documents/GitHub/vax-booster-dose-RD")
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
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)

end_date = as.Date("2023-02-04")

#####################################################
# Functions                                       ###
#####################################################

# Factorise ----
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}


#####################################################
### Read in data                                  ###
#####################################################

booster <- read_csv(here::here("output", "cohort", "cohort_final_sep.csv")) %>%
    # Age at Nov 26
    mutate(age_mos = (dob %--% "2022-11-26") %/% months(1),
           age_yrs = (dob %--% "2022-11-26") %/% years(1)) %>%
    # Exclude if died
    subset(dod > as.Date("2022-11-26") | is.na(dod)) %>%
    dplyr::select(c(age_mos, age_yrs, boost_date, booster)) 
  

#####################################################
### Cumulative vaccine update by 1-year age group ###
#####################################################

### Calculate cumulative proportion of people receiving booster dose
booster_age1_byday <- booster %>%
  subset(age_yrs < 55 & age_yrs > 44) %>% 
  group_by(age_yrs) %>%
  mutate(total = n()) %>% # Calculate denominator (total count per age category)
  ungroup() %>%
  subset(booster == 1) %>% # Remove people with no booster vax
  group_by(age_yrs, total, boost_date) %>%
  summarise(boost_n = n()) %>% # Num vaccinated each day
  ungroup() %>%
  arrange(age_yrs, boost_date) %>%
  group_by(age_yrs, total) %>%
  mutate(boost_sum = cumsum(boost_n), # Cumulative num vaccinated each day
         boost_sum = case_when(boost_sum > 7 ~ boost_sum), # Redaction
         boost_sum = round(boost_sum / 5) * 5, # Rounding
         total = round(total / 5) * 5, # Rounding
         rate = boost_sum / total * 100) %>% # Cumulative % vaccinated each day
  complete(boost_date = seq(min(as.Date(boost_date)),
                                  max(end_date), by = '1 day')) %>%
  fill(c(boost_sum, rate)) %>% # Create rows for days with zero vaccinations
  ungroup() %>%
  select(!boost_n) %>%
  mutate(age_yrs = as.character(age_yrs))

# Save
booster_age1_byday <- booster_age1_byday %>% subset(boost_date >= as.Date("2022-09-06"))
write.csv(booster_age1_byday,
          here::here("output", "cumulative_rates", "final_dose4_cum_byage1.csv"), row.names = FALSE)

### Plot cumulative booster dose over time
ggplot(subset(booster_age1_byday, boost_date >= as.Date("2022-09-05") |
                boost_date < end_date)) +
  geom_line(aes(x = boost_date, y = rate/100, group = age_yrs, col = age_yrs),
            size = 1.25) +
  geom_vline(aes(xintercept = as.Date("2022-10-14")), linetype = "longdash") +
  scale_color_brewer(palette = "RdBu") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = c(as.Date("2022-09-01"), as.Date("2022-10-01"),
                                as.Date("2022-10-14"), as.Date("2022-11-01"),
                                as.Date("2022-12-01"), as.Date("2023-01-01"),
                                as.Date("2023-02-01")),
                     labels = c("Sep 1", "Oct 1", "Oct 14", "Nov 1", "Dec 1", 
                                "Jan 1", "Feb 1")) +
  xlab(NULL) + ylab("Received autumn booster") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "cumulative_rates", "plot_dose4_cum_age1.png"),
       dpi = 300, units = "in", width = 6, height = 3.25)



######################################
### % vaccinated by age in months  ###
######################################

booster_nov26 <- booster %>%
    group_by(age_mos) %>%
    mutate(boost_nov26 = if_else(boost_date <= as.Date("2022-11-26"), 1, 0, 0),
           total = n()) %>%
    ungroup() %>%
    group_by(age_mos, total) %>%
    summarise(n_boost = sum(boost_nov26)) %>%
    mutate(n_boost = case_when(n_boost > 7 ~ n_boost), # Redaction
              n_boost = round(n_boost / 5) * 5, # Rounding
              total = round(total / 5) * 5, # Rounding
              pcent_boost = n_boost / total * 100,
              age_mos = as.numeric(age_mos)) 

# Save
write.csv(booster_nov26,
          here::here("output", "cumulative_rates", "final_vax4_age_months.csv"), row.names = FALSE)

### Plot cumulative booster dose over time
ggplot(subset(booster_nov26, age_mos > 564 & age_mos < 636)) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_point(aes(x = age_mos / 12, y = pcent_boost), 
             col = "dodgerblue3") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(47,53,1)) +
  xlab(NULL) + ylab("Received second booster\nCOVID-19 vaccine (%)") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "cumulative_rates", "plot_dose4_age_months.png"),
       dpi = 300, units = "in", width = 6, height = 3.25)



###################
##### Checks ######
###################

# # Plot of when people received each dose (see if it makes sense)
doses_by_day <- read_csv(here::here("output", "cohort", "cohort_final_sep.csv")) %>%
  select(c(patient_id, covid_vax_1_date, covid_vax_2_date,
            covid_vax_3_date, covid_vax_4_date)) %>%
  melt(id = c("patient_id")) %>%
  rename(vax = variable, date = value) %>%
  mutate(vax = fct_case_when(
    vax == "covid_vax_1_date" ~ "First dose",
    vax == "covid_vax_2_date" ~ "Second dose",
    vax == "covid_vax_3_date" ~ "Third dose",
    vax == "covid_vax_4_date" ~ "Fourth dose"
  )) %>%
  subset(!is.na(date)) %>%
  group_by(vax, date) %>%
  summarise(vax_n = n()) %>%
  group_by(vax) %>%
  complete(date = seq(min(as.Date(date)),
                                  max(end_date),
                      by = '1 day')) %>%
  fill(vax_n) %>% # Create rows for days with zero vaccinations
  ungroup()


ggplot(doses_by_day, aes(x = date, y = vax_n)) +
  geom_line(aes(col = vax)) +
  geom_ribbon(aes(group = vax, fill = vax, ymax = vax_n),  ymin = 0, alpha = 0.5) +
  xlab("") + ylab("Received vaccine (n)") +
  scale_colour_manual(values = c('#00496f', '#0f85a0', '#edd746', '#dd4124'))+
  scale_fill_manual(values = c('#00496f', '#0f85a0', '#edd746', '#dd4124')) +
  scale_x_continuous(breaks = c(as.Date("2021-01-01"), as.Date("2021-07-01"),
                                as.Date("2022-01-01"), as.Date("2022-07-01"),
                                as.Date("2023-01-01")),
                     labels = c("Jan 2021", "Jul 2021", "Jan 2022", "Jul 2022",
                                "Jan 2023")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "cumulative_rates", "plot_all_doses_over_time.png"),
       dpi = 300, units = "in", width = 6, height = 3.25)
