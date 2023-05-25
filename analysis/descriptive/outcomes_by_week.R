################################################################
# This script:
# - Plot outcome rate by week over time
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
dir_create(here::here("output", "index"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "descriptive"), showWarnings = FALSE, recurse = TRUE)

## Load functions
source(here::here("analysis", "custom_functions.R"))


##################################################################
### Function to calculate no. people with an event in each week                                
##################################################################

byweek <- function(dat){
  
  # Read in data for each week
  df <- read_feather(here::here("output", "index", paste0("input_outcomes_2_", dat, ".feather"))) %>%
    
          # Calculate age at index date
          mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
                 dob = dob + 14,
                 age_yrs = (dob %--% as.Date("2022-09-03")) %/% years(1)) %>%
    
          # Exclude if died before index date or not in age range
          subset(dod >= as.Date(dat) & age_yrs >= 45 & age_yrs < 55) %>%
    
          # Create variable for week
          mutate(week = dat, n_total = n()) %>%
    
          # Calculate no. events in that week (one per person only)
          group_by(week, n_total) %>%
          summarise(n_covidadmit = sum(!is.na(covidadmitted_date_1)),
                    n_covidemerg = sum(!is.na(covidemergency_date_1)),
                    n_covidcomp = sum(!is.na(covidadmitted_date_1) |
                                        !is.na(covidemergency_date_1) |
                                        (!is.na(coviddeath_date) &
                                           coviddeath_date >= as.Date(dat) &
                                           coviddeath_date < as.Date(dat) + 6)),
                    n_respcomp = sum(!is.na(respadmitted_date_1) |
                                       (!is.na(respdeath_date) &
                                          respdeath_date >= as.Date(dat) &
                                          respdeath_date < as.Date(dat) + 6)),
                    n_anyadmit = sum(!is.na(admitted_unplanned_date_1)),
                    n_anydeath = sum(!is.na(any_death_date) &
                                       any_death_date >= as.Date(dat) &
                                       any_death_date < as.Date(dat) + 6)) %>%
    
          # Rounding
          mutate((across(starts_with("n_"), roundmid_any)),
                 rate_covidadmit_mid6 = n_covidadmit / n_total * 100000,
                 rate_covidemerg_mid6 = n_covidemerg / n_total * 100000,
                 rate_covidcomp_mid6 = n_covidcomp / n_total * 100000,
                 rate_respcomp_mid6 = n_respcomp / n_total * 100000,
                 rate_anyadmit_mid6 = n_anyadmit / n_total * 100000,
                 rate_anydeath_mid6 = n_anydeath / n_total * 100000) %>%
          rename_at(vars(starts_with("n_")), ~ paste0(., '_mid6'))
  
 return(df)   
  
}


# Do the above over all relevant start dates
start_dates <- seq(as.Date("2022-09-03"), as.Date("2022-12-10"), by = 7)
sapply(start_dates, byweek)

# Combine into one file
wide <- rbind(
              byweek("2022-09-03"),
              byweek("2022-09-10"),
              byweek("2022-09-17"),
              byweek("2022-09-24"),
              byweek("2022-10-01"),
              byweek("2022-10-08"),
              byweek("2022-10-15"),
              byweek("2022-10-22"),
              byweek("2022-10-29"),
              byweek("2022-11-05"),
              byweek("2022-11-12"),
              byweek("2022-11-19"),
              byweek("2022-11-26"),
              byweek("2022-12-03"),
              byweek("2022-12-10"),
              byweek("2022-12-17"),
              byweek("2022-12-24")
          )

write.csv(wide, here::here("output", "descriptive", "outcomes_by_week.csv"))


####### Plot ############################
#########################################

# Convert to long for plotting
long <- wide %>% 
  dplyr::select(!contains("n_")) %>%
  melt(id = "week")

long$variable <- factor(long$variable, levels = c("rate_covidcomp_mid6", "rate_covidadmit_mid6", 
                                                "rate_covidemerg_mid6", "rate_respcomp_mid6",
                                                "rate_anyadmit_mid6", "rate_anydeath_mid6"),
                       labels = c("COVID composite", "COVID unplanned admission",
                                  "COVID A&E", "Respiratory composite",
                                  "Any unplanned admission", "Any death"))

# Plot
ggplot(long, aes(x = week, y = value, group = variable)) +
  geom_line(aes(col = variable)) +
  facet_wrap(~ variable, scales = "free_y") + 
  ylab("Rate per 100,000") + xlab("Week") +
  scale_x_discrete(breaks = c("2022-09-03", "2022-10-15", "2022-11-26")) +
  theme_bw()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
        legend.title = element_blank())

ggsave(here::here("output", "descriptive", "outcomes_by_week.png"), dpi = 300,
       height = 5, width = 15, units = "in")