
################################################################
# This script:
# - Calculates cumulative uptake of fourth dose/second booster
#    COVID-19 vaccine by age
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

## Create directories
dir_create(here::here("output", "cumulative_rates"), showWarnings = FALSE, recurse = TRUE)

# Custom functions
# Factorise ----
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}


# Read in data
fourth <- read_csv(here::here("output", "input_fourth.csv"),
                    col_types = cols(
                      age_cat = col_character(),
                      sex = col_character(),
                      covid_vax_1_date = col_date(format = "%Y-%m-%d"),
                      covid_vax_2_date = col_date(format = "%Y-%m-%d"),
                      covid_vax_3_date = col_date(format = "%Y-%m-%d"),
                      covid_vax_4_date = col_date(format = "%Y-%m-%d"))) %>%
  mutate(age_cat = fct_case_when(
           age_cat == 0 ~ "Missing",
           age_cat == 1 ~ "40-44 y",
           age_cat == 2 ~ "45-49 y",
           age_cat == 3 ~ "50-54 y",
           age_cat == 4 ~ "55-59 y",
           TRUE ~ NA_character_
         )
  ) %>%
  arrange(age_cat, desc(covid_vax_4_date)) 


# Calculate cumulative proportion of people receiving fourth dose
fourth_by_day <- fourth %>% 
  group_by(age_cat) %>%
  mutate(total = n()) %>% # Calculate denominator (total count per age category)
  ungroup() %>%
  subset(!is.na(covid_vax_4_date)) %>% # Remove people with no 4th vax
  group_by(age_cat, total, covid_vax_4_date) %>% 
  summarise(vax_4_n = n()) %>% # Num vaccinated each day
  ungroup() %>%
  group_by(age_cat, total) %>%
  mutate(vax_4_sum = cumsum(vax_4_n), # Cumulative num vaccinated each day
         vax_4_sum = case_when(vax_4_sum > 5 ~ vax_4_sum), # Redaction
           vax_4_sum = round(vax_4_sum / 7) * 7, # Rounding
         rate = vax_4_sum / total * 100) %>% # Cumulative % vaccinated each day
  complete(covid_vax_4_date = seq(min(as.Date(covid_vax_4_date)), 
                                max(as.Date("2022-11-30")), by = '1 day')) %>%
  fill(c(vax_4_sum, rate)) %>% # Create rows for days with zero vaccinations
  ungroup() %>%
  select(!vax_4_n)

# Save
fourth_by_day <- fourth_by_day %>% subset(covid_vax_4_date >= as.Date("2022-09-01"))
write.csv(fourth_by_day, 
          here::here("output", "cumulative_rates", "final_dose4_cum_byage.csv"), row.names = FALSE)

# Plot cumulative fourth dose over time
ggplot(subset(fourth_by_day, age_cat != "Missing" &
                covid_vax_4_date >= "2022-09-01" & 
                covid_vax_4_date < "2022-12-01")) +
  geom_line(aes(x = covid_vax_4_date, y = rate/100, group = age_cat, col = age_cat),
            size = 1.5) +
  geom_vline(aes(xintercept = as.Date("2022-10-14")), linetype = "longdash") +
  scale_colour_manual(values = c('#00496f', '#0f85a0', '#edd746', '#dd4124'))+
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(as.Date("2022-09-01"), as.Date("2022-10-01"), 
                                as.Date("2022-10-14"), as.Date("2022-11-01"),
                                as.Date("2022-12-01")),
                     labels = c("Sep 1", "Oct 1", "Oct 14", "Nov 1", "Dec 1")) +
  xlab("") + ylab("Received second booster") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "cumulative_rates", "plot_dose4_cum.png"), 
       dpi = 300, units = "in", width = 6, height = 3.25)


##### Checks ######

# Get counts to check
fourth_by_day %>% 
  distinct(age_cat, total)

# Proportion with each number of doses on Nov 1
dose_counts <- fourth %>%
  mutate(# Count receiving each dose
         first_dose = ifelse(is.na(covid_vax_1_date)|covid_vax_1_date > "2022-11-01", 0, 1),
         second_dose = ifelse(is.na(covid_vax_2_date)|covid_vax_2_date > "2022-11-01", 0, 1),
         third_dose = ifelse(is.na(covid_vax_3_date)|covid_vax_3_date > "2022-11-01", 0, 1),
         fourth_dose = ifelse(is.na(covid_vax_4_date)|covid_vax_4_date > "2022-11-01", 0, 1),
         no_dose = ifelse(is.na(covid_vax_1_date), 1, 0),
         
         # Variable representing number of doses
         num_dose = ifelse(fourth_dose == 1, 4,
                           ifelse(third_dose == 1, 3,
                                  ifelse(second_dose == 1, 2,
                                         ifelse(first_dose == 1, 1, 0)))))

# Plot of when people received each dose (see if it makes sense)
doses_by_day <- fourth %>%
  select(!c(age_cat, age, sex)) %>%
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
                                  max(as.Date("2022-11-30")), 
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
                                as.Date("2022-01-01"), as.Date("2022-07-01")),
                     labels = c("Jan 2021", "Jul 2021", "Jan 2022", "Jul 2022")) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "cumulative_rates", "plot_all_doses_over_time.png"), 
       dpi = 300, units = "in", width = 6, height = 3.25)
