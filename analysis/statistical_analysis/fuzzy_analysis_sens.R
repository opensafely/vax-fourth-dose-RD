################################################################
# This script:
# - Conducts fuzzy regression discontinuity 
#   using instrumental variables analysis 
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
dir_create(here::here("output", "modelling", "iv", "sens"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "final"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort_bydate"), showWarnings = FALSE, recurse = TRUE)


# Function for instrumental variable analysis
fuzzy <- function(start_date){
  
  # Read in data
  data <- read_feather(here::here("output", paste0("input_outcomes_",start_date,".feather"))) %>%
    mutate(dob = as.Date(as.character(as.POSIXct(dob)), format = "%Y-%m-%d"),
           
           # Set DOB to mid-month
           dob = dob + 14,
           age_yrs = (dob %--% as.Date(start_date)) %/% years(1),
           age_mos = (dob %--% as.Date(start_date)) %/% months(1),
           age_3mos = floor(age_mos / 3),
           age_3mos_c = as.numeric(age_3mos - 200),
           over50 = if_else(age_3mos >= 200, 1, 0, 0),
           
           covidcomposite = as.integer(covidcomposite),
           respcomposite = as.integer(respcomposite),
           anydeath = as.integer(anydeath),
           anyadmitted = as.integer(anyadmitted),
           
           # Flag for having received flu vax before start date
           flu_vax = if_else(!is.na(flu_vax_date) & flu_vax_date < start_date, 1, 0, 0),
           
           # Flag for booster before start date
           boost = if_else(!is.na(boost_date) & boost_date < start_date, 1, 0, 0)) %>%
    
    subset(!is.na(age_3mos) & age_3mos >= 180 & age_3mos < 220
           & (is.na(dod) | dod >= as.Date(start_date))) 
  
  mod <- function(out, name, suffix){
    
    # Prep data
    df <- data  %>%
      group_by(age_3mos_c, over50) %>%
      summarise(n = n(), 
                boost = sum(boost),
                outcome = sum({{out}})) %>%
      mutate(p_boost = boost / n * 100,
             p_outcome = outcome / n * 100)
    
    write.csv(df, here::here("output", "modelling", "iv", "sens",
                             paste0("data_iv_sens_", suffix,"_", start_date,".csv")),
              row.names = FALSE)
    
    rdd <- rdrobust(y = df$p_outcome, x = df$age_3mos_c, c = 0,
                       fuzzy = df$p_boost, p = 1, h = 20,
                       kernel = "uniform", weights = df$n)
    
    # Save model summary
    sink(here::here("output","modelling", "iv", "sens",
                    paste0("summ_iv_sens_", suffix, "_",start_date,".txt")))
    print(summary(rdd))
    sink()
    
  
    # Save coefficients and 95% CIs
    coef <- data.frame(estimate = rdd$coef[1] ,
                lci = rdd$ci[1,1] ,
                uci = rdd$ci[1,2] ,
                outcome = name,
                start_date = start_date)
    
    # Save coefficients
    write.csv(coef, here::here("output", "modelling", "iv", "sens",
              paste0("coef_iv_sens_",suffix,"_",start_date,".csv")),
              row.names = FALSE)
    
  }
  
  # Run for each outcome
  mod(covidcomposite, "COVID unplanned admission/A&E/death", "covidcomposite")
  mod(respcomposite, "Respiratory composite", "respcomposite")
  mod(anyadmitted, "All cause unplanned admission", "anyadmitted") 
  mod(anydeath, "All cause death", "anydeath")  
  
  
}


# Create list of dates
start_dates <- c(as.Date(0:13, origin = "2022-11-26")) 

# Run function over all dates
sapply(start_dates, fuzzy)


### Combine all coefficients files into one ###
comb <- function(suffix){
  
  all_coef <- bind_rows(
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-11-26.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-11-27.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-11-28.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-11-29.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-11-30.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-01.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-02.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-03.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-04.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-05.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-06.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-07.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-08.csv"))),
    read_csv(here::here("output", "modelling", "iv", "sens", paste0("coef_iv_sens_",suffix,"_2022-12-09.csv")))
  )
  
  write.csv(all_coef, here::here("output", "modelling", "final", paste0("coef_iv_sens_",suffix,"_","all.csv")), 
            row.names = FALSE)
  
}

comb("covidcomposite")
comb("respcomposite")
comb("anyadmitted")
comb("anydeath")



