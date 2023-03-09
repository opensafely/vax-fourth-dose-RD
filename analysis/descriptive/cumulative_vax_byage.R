
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
library('RColorBrewer')

## Create directories
dir_create(here::here("output", "cumulative_rates"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "checks"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output"), showWarnings = FALSE, recurse = TRUE)

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

end_date <- as.Date("2023-03-02", format = "%Y-%m-%d")


#####################################################
### Read in data                                  ###
#####################################################

fourth <- arrow::read_feather(here::here("output", "input_fourth_2022-09-02.feather")) %>%
  mutate_at(c(vars(c(contains("_date")), "dob")), as.Date, format = "%Y-%m-%d") %>%
  
  # Calculate age in months
  mutate(age_month = (dob %--% as.Date("2022-09-02")) %/% months(1),
         months = age_month - age*12) %>%
  dplyr::select(!c(sex,imd,ethnicity,region))


######################################################
### Check that age/DOB variables are consistent    ###
######################################################

age_check <- fourth %>%
  mutate(age_check = (dob %--% as.Date("2022-09-02")) %/% years(1),
         months_check = age_month - age_check*12,
         too_many_months = ifelse(months > 12, 1, 0),
         negative_months = ifelse(months < 0, 1, 0),
         months_differ = ifelse(months_check == months, 0, 1),
         years_differ = ifelse(age_check == age, 0, 1)) %>%
  summarise(n = n(),
        too_many_months = sum(too_many_months, na.rm = TRUE),
        negative_months = sum(negative_months, na.rm = TRUE),
        months_differ = sum(months_differ, na.rm = TRUE),
        years_differ = sum(years_differ, na.rm = TRUE),
        dob_missing = sum(is.na(dob)))

write.csv(age_check,
          here::here("output", "checks", "age_check.csv"), row.names = FALSE)


#####################################################
### Cumulative vaccine update by 1-year age group ###
#####################################################

### Calculate cumulative proportion of people receiving fourth dose
fourth_age1_byday <- fourth %>%
  subset(age < 54 & age > 45) %>% # Restrict to 4 years before/after cutoff
  group_by(age) %>%
  mutate(total = n()) %>% # Calculate denominator (total count per age category)
  ungroup() %>%
  subset(!is.na(covid_vax_4_date)) %>% # Remove people with no 4th vax
  group_by(age, total, covid_vax_4_date) %>%
  summarise(vax_4_n = n()) %>% # Num vaccinated each day
  ungroup() %>%
  arrange(age, covid_vax_4_date) %>%
  group_by(age, total) %>%
  mutate(vax_4_sum = cumsum(vax_4_n), # Cumulative num vaccinated each day
         vax_4_sum = case_when(vax_4_sum > 7 ~ vax_4_sum), # Redaction
         vax_4_sum = round(vax_4_sum / 5) * 5, # Rounding
         total = round(total / 5) * 5, # Rounding
         rate = vax_4_sum / total * 100) %>% # Cumulative % vaccinated each day
  complete(covid_vax_4_date = seq(min(as.Date(covid_vax_4_date)),
                                  max(as.Date("2023-02-01")), by = '1 day')) %>%
  fill(c(vax_4_sum, rate)) %>% # Create rows for days with zero vaccinations
  ungroup() %>%
  select(!vax_4_n) %>%
  mutate(age = as.character(age))

# Save
fourth_age1_byday <- fourth_age1_byday %>% subset(covid_vax_4_date >= as.Date("2022-09-01"))
write.csv(fourth_age1_byday,
          here::here("output", "cumulative_rates", "final_dose4_cum_byage1.csv"), row.names = FALSE)

### Plot cumulative fourth dose over time
ggplot(subset(fourth_age1_byday, 
                covid_vax_4_date >= "2022-09-01" &
                covid_vax_4_date < end_date)) +
  geom_line(aes(x = covid_vax_4_date, y = rate/100, group = age, col = age),
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
  xlab(NULL) + ylab("Received second booster") +
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

fourth_nov25 <- fourth %>%
    subset(!is.na(age_month) & (!is.na(any_death_date)|any_death_date>=as.Date("2022-11-25"))) %>%
    group_by(age_month) %>%
    mutate(covid_vax_4 = if_else(covid_vax_4_date < as.Date("2022-11-25"), 1, 0, missing = 0),
           total = n()) %>%
    ungroup() %>%
    group_by(age_month, total) %>%
    summarise(n_covid_vax_4 = sum(covid_vax_4)) %>%
    mutate(n_covid_vax_4 = case_when(n_covid_vax_4 > 7 ~ n_covid_vax_4), # Redaction
              n_covid_vax_4 = round(n_covid_vax_4 / 5) * 5,
              total = round(total / 5) * 5,
              pcent_covid_vax_4 = n_covid_vax_4 / total * 100,
              age_month = as.numeric(age_month)) # Rounding

# Save
write.csv(fourth_nov25,
          here::here("output", "cumulative_rates", "final_vax4_age_months.csv"), row.names = FALSE)

### Plot cumulative fourth dose over time
ggplot(subset(fourth_nov25, age_month > 564 & age_month < 636)) +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_point(aes(x = age_month / 12, y = pcent_covid_vax_4), 
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
# 
# # Proportion with each number of doses on Nov 1
# dose_counts <- fourth %>%
#   mutate(# Count receiving each dose
#          first_dose = ifelse(is.na(covid_vax_1_date)|covid_vax_1_date > "2023-02-01", 0, 1),
#          second_dose = ifelse(is.na(covid_vax_2_date)|covid_vax_2_date > "2023-02-01", 0, 1),
#          third_dose = ifelse(is.na(covid_vax_3_date)|covid_vax_3_date > "2023-02-01", 0, 1),
#          fourth_dose = ifelse(is.na(covid_vax_4_date)|covid_vax_4_date > "2023-02-01", 0, 1),
#          no_dose = ifelse(is.na(covid_vax_1_date), 1, 0),
# 
#          # Variable representing number of doses
#          num_dose = case_when(fourth_dose == 1 ~ 4,
#                               third_dose == 1 ~ 3,
#                               second_dose == 1 ~ 2,
#                               first_dose == 1~ 1,
#                               TRUE ~ 0)
#   )
# 
# write.csv(dose_counts,
#           here::here("output", "cumulative_rates", "doses_at_nov1.csv"), row.names = FALSE)
# 
# 
# # Plot of when people received each dose (see if it makes sense)
# doses_by_day <- fourth %>%
#   select(!c(age_cat, age, sex, imd, region, ethnicity)) %>%
#   melt(id = c("patient_id")) %>%
#   rename(vax = variable, date = value) %>%
#   mutate(vax = fct_case_when(
#     vax == "covid_vax_1_date" ~ "First dose",
#     vax == "covid_vax_2_date" ~ "Second dose",
#     vax == "covid_vax_3_date" ~ "Third dose",
#     vax == "covid_vax_4_date" ~ "Fourth dose"
#   )) %>%
#   subset(!is.na(date)) %>%
#   group_by(vax, date) %>%
#   summarise(vax_n = n()) %>%
#   group_by(vax) %>%
#   complete(date = seq(min(as.Date(date)),
#                                   max(end_date),
#                       by = '1 day')) %>%
#   fill(vax_n) %>% # Create rows for days with zero vaccinations
#   ungroup()
# 
# 
# ggplot(doses_by_day, aes(x = date, y = vax_n)) +
#   geom_line(aes(col = vax)) +
#   geom_ribbon(aes(group = vax, fill = vax, ymax = vax_n),  ymin = 0, alpha = 0.5) +
#   xlab("") + ylab("Received vaccine (n)") +
#   scale_colour_manual(values = c('#00496f', '#0f85a0', '#edd746', '#dd4124'))+
#   scale_fill_manual(values = c('#00496f', '#0f85a0', '#edd746', '#dd4124')) +
#   scale_x_continuous(breaks = c(as.Date("2021-01-01"), as.Date("2021-07-01"),
#                                 as.Date("2022-01-01"), as.Date("2022-07-01"),
#                                 as.Date("2023-01-01")),
#                      labels = c("Jan 2021", "Jul 2021", "Jan 2022", "Jul 2022", "Jan 2023")) +
#   theme_bw() +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave(here::here("output", "cumulative_rates", "plot_all_doses_over_time.png"),
#        dpi = 300, units = "in", width = 6, height = 3.25)

