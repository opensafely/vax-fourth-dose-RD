################################################################
# This script:
# - Conducts sharp regression discontinuity
#   regression model and plots predicted values
# - Sensitivity analyses excluding people age = 50
#
# Dependency = outcomes_*
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
library('rdrobust')

dir_create(here::here("output", "covid_outcomes"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "covid_outcomes", "by_start_date"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "final"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "figures"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort_bydate"), showWarnings = FALSE, recurse = TRUE)


# Load functions
source(here::here("analysis", "custom_functions.R"))

### Function to run sharp RD analysis, output coefficients and predicted values
sharp <- function(start_date){
  
  # Read in data
  data <- read_feather(here::here("output", paste0("input_outcomes_",start_date,".feather"))) %>%
    mutate(
             # Set DOB to mid-month
             dob = dob + 14,
             age_yrs = (dob %--% as.Date(start_date)) %/% years(1),
             age_mos = (dob %--% as.Date(start_date)) %/% months(1),
             age_3mos = floor(age_mos / 3),
             age_3mos_c = as.numeric(age_3mos - 200),
             over50 = if_else(age_3mos >= 200, 1, 0, 0),
          
             covidcomposite = as.integer(covidcomposite),
             respcomposite = as.integer(respcomposite),
             dod = as.integer(dod),
             anyadmitted = as.integer(anyadmitted)) %>%
    
    subset(!is.na(age_3mos) & age_3mos >= 180 & age_3mos < 220
           
           # Exclude age 50
           & age_3mos != 200 
           & (is.na(dod) | dod >= as.Date(start_date)) ) 
  
  mod <- function(out, name, suffix){
    
    # Prep data
    df <- data  %>%
      group_by(age_3mos_c, age_3mos, over50) %>%
      summarise(n = n(), 
                outcome = sum({{out}})) %>%
      mutate(n_mid6 = roundmid_any(n, 6),
             outcome_mid6 = roundmid_any(outcome, 6),
             p_outcome = outcome / n * 100000,
             p_outcome_mid6 = outcome_mid6 / n_mid6 * 100000)
    
    # Write data for plots
    write.csv(df,
              here::here("output", "modelling", paste0("plot_data_sens_",suffix,"_",start_date,".csv")),
              row.names = FALSE)
    
    # 6-month age groups 
    df_6mos <- df %>%
      mutate(age_6mos = floor(age_3mos / 2)) %>%
      group_by(age_6mos) %>%
      summarise(n = sum(n),
                outcome = sum(outcome)) %>%
      mutate(n_mid6 = roundmid_any(n, 6),
             outcome_mid6 = roundmid_any(outcome, 6),
             p_outcome = outcome / n * 100000,
             p_outcome_mid6 = outcome_mid6 / n_mid6 * 100000) %>%
      dplyr::select(c(age_6mos, outcome_mid6, n_mid6, p_outcome_mid6))
    
    write.csv(df_6mos,
               here::here("output", "modelling", paste0("plot_data_6mos_sens_",suffix,"_",start_date,".csv")))
    
    # Model
    mod <- lm(p_outcome ~ age_3mos_c*over50, data = df, weights = df$n)

    # Save coefficients and 95% CIs
    coef <-  data.frame(est = mod$coefficients)
    coef2 <- coef %>%  data.frame() %>%
      mutate(var = row.names(coef)) %>%
      cbind(confint(mod), aic = AIC(mod)) %>%
      rename(lci = `2.5 %`, uci = `97.5 %`) %>%
      mutate(start_date = start_date, 
            outcome = name,
            est = est,
            lci = lci,
            uci = uci) 
    
    # Save coefficients
    write.csv(coef2, 
              here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_",start_date,".csv")),
              row.names = FALSE)

  }
  
  # Run for each outcome
  mod(covidcomposite, "COVID unplanned admission/A&E/death", "covidcomposite")
  mod(respcomposite, "Respiratory composite", "respcomposite")
  mod(anyadmitted, "All cause unplanned admission", "anyadmitted")  
  mod(anydeath, "All cause death", "anydeath")
  
}



# Create list of dates
start_dates <- c(as.Date("2022-09-03"), 
                 as.Date("2022-10-15"), 
                 as.Date(0:13, origin = "2022-11-26")) 

# Run function over all dates
sapply(start_dates, sharp)


### Combine all coefficients files into one ###
comb <- function(suffix){

  all_coef <- bind_rows(
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-09-03.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-10-15.csv"))),      
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-11-26.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-11-27.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-11-28.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-11-29.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-11-30.csv"))),          
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-01.csv"))),          
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-02.csv"))),  
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-03.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-04.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-05.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-06.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-07.csv"))),
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-08.csv"))),          
          read_csv(here::here("output", "modelling", paste0("coef_sharp_sens_",suffix,"_2022-12-09.csv")))
  )

  write.csv(all_coef, here::here("output", "modelling", "final", paste0("coef_sharp_sens_",suffix,"_","all.csv")), 
            row.names = FALSE)

}

comb("covidcomposite")
comb("respcomposite")
comb("anyadmitted")
comb("anydeath")

