################################################################
# This script:
# - Conducts sharp regression discontinuity
#   regression model and plots predicted values
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

dir_create(here::here("output", "modelling", "bandwidth"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "final"), showWarnings = FALSE, recurse = TRUE)


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
           & (is.na(dod) | dod >= as.Date(start_date)) ) 
  
  mod <- function(out, name, suffix){
    
    # Prep data
    df <- data  %>%
      group_by(age_3mos_c, age_3mos, over50) %>%
      summarise(n = n(), 
                outcome = sum({{out}})) %>%
      mutate(p_outcome = outcome / n * 100000)
    
    # Model
    mod_4yrs <- lm(p_outcome ~ age_3mos_c*over50, 
              data = subset(df, age_3mos >= 184 & age_3mos < 216), 
              weights = subset(df, age_3mos >= 184 & age_3mos < 216)$n)

    # Save coefficients and 95% CIs
    coef_4yrs <-  data.frame(est = mod_4yrs$coefficients)
    coef2_4yrs <- coef_4yrs %>%  data.frame() %>%
      mutate(var = row.names(coef_4yrs)) %>%
      cbind(confint(mod_4yrs), aic = AIC(mod_4yrs)) %>%
      rename(lci = `2.5 %`, uci = `97.5 %`) %>%
      mutate(start_date = start_date, 
            outcome = name,
            bandwidth = "4 years",
            est = est,
            lci = lci,
            uci = uci) 

    # Model
    mod_3yrs <- lm(p_outcome ~ age_3mos_c*over50, 
                   data = subset(df, age_3mos >= 188 & age_3mos < 212), 
                   weights = subset(df, age_3mos >= 188 & age_3mos < 212)$n)
    
    # Save coefficients and 95% CIs
    coef_3yrs <-  data.frame(est = mod_3yrs$coefficients)
    coef2_3yrs <- coef_3yrs %>%  data.frame() %>%
      mutate(var = row.names(coef_3yrs)) %>%
      cbind(confint(mod_3yrs), aic = AIC(mod_3yrs)) %>%
      rename(lci = `2.5 %`, uci = `97.5 %`) %>%
      mutate(start_date = start_date, 
             outcome = name,
             bandwidth = "3 years",
             est = est,
             lci = lci,
             uci = uci) 
    
    # Model
    mod_2yrs <- lm(p_outcome ~ age_3mos_c*over50, 
                   data = subset(df, age_3mos >= 192 & age_3mos < 208), 
                   weights = subset(df, age_3mos >= 192 & age_3mos < 208)$n)
    
    # Save coefficients and 95% CIs
    coef_2yrs <-  data.frame(est = mod_2yrs$coefficients)
    coef2_2yrs <- coef_2yrs %>%  data.frame() %>%
      mutate(var = row.names(coef_2yrs)) %>%
      cbind(confint(mod_2yrs), aic = AIC(mod_2yrs)) %>%
      rename(lci = `2.5 %`, uci = `97.5 %`) %>%
      mutate(start_date = start_date, 
             outcome = name,
             bandwidth = "2 years",
             est = est,
             lci = lci,
             uci = uci) 
    
    # Model
    mod_1yrs <- lm(p_outcome ~ age_3mos_c*over50, 
                   data = subset(df, age_3mos >= 196 & age_3mos < 204), 
                   weights = subset(df, age_3mos >= 196 & age_3mos < 204)$n)
    
    # Save coefficients and 95% CIs
    coef_1yrs <-  data.frame(est = mod_1yrs$coefficients)
    coef2_1yrs <- coef_1yrs %>%  data.frame() %>%
      mutate(var = row.names(coef_1yrs)) %>%
      cbind(confint(mod_1yrs), aic = AIC(mod_1yrs))  %>%
      rename(lci = `2.5 %`, uci = `97.5 %`) %>%
      mutate(start_date = start_date, 
             outcome = name,
             bandwidth = "1 year",
             est = est,
             lci = lci,
             uci = uci) 
    
    coef_bw <- rbind(coef2_4yrs, coef2_3yrs, coef2_2yrs, coef2_1yrs)
    
    write.csv(coef_bw, 
              here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_",start_date,".csv")),
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
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-09-03.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-10-15.csv"))),      
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-11-26.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-11-27.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-11-28.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-11-29.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-11-30.csv"))),          
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-01.csv"))),          
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-02.csv"))),  
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-03.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-04.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-05.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-06.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-07.csv"))),
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-08.csv"))),          
          read_csv(here::here("output", "modelling", "bandwidth", paste0("coef_sharp_sens_bw_",suffix,"_2022-12-09.csv")))
  )

  write.csv(all_coef, here::here("output", "modelling", "final", paste0("coef_sharp_sens_bw_",suffix,"_","all.csv")), 
            row.names = FALSE)

}

comb("covidcomposite")
comb("respcomposite")
comb("anyadmitted")
comb("anydeath")

