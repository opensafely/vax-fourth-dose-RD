################################################################
# This script:
# - Conducts fuzzy regression discontinuity 
#   using instrumental variables analysis 
#
# - Sensitivity analysis - different bandwidths
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

dir_create(here::here("output", "modelling", "iv", "bandwidth"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "final"), showWarnings = FALSE, recurse = TRUE)


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

           # Flag for booster before start date
           boost = if_else(!is.na(boost_date) & boost_date < start_date, 1, 0, 0)) %>%
    
    subset(!is.na(age_3mos) & age_3mos >= 180 & age_3mos < 220
           & (is.na(dod) | dod >= as.Date(start_date))) 
  
  mod <- function(out, name, suffix){
    
    # Prep data
    df <- data  %>%
      group_by(age_3mos_c, age_3mos, over50) %>%
      summarise(n = n(), 
                boost = sum(boost),
                outcome = sum({{out}})) %>%
      mutate(p_boost = boost / n * 100,
             p_outcome = outcome / n * 100)
    
    # Optimal bandwidth
    bw <- rdbwselect(df$p_outcome, df$age_3mos_c, bwrestrict = TRUE)
    
    sink(here::here("output","modelling", "iv", "bandwidth", paste0("bw_iv_", suffix, "_",start_date,".txt")))
    print(summary(bw))
    sink()
    
    # Model - 4 years bandwidth
    df_4yrs <- subset(df, age_3mos >= 184 & age_3mos < 216)
    rdd_4yrs <- rdrobust(y = df_4yrs$p_outcome, x = df_4yrs$age_3mos_c, c = 0,
                       fuzzy = df_4yrs$p_boost, p = 1, h = 20,
                       kernel = "uniform", weights = df_4yrs$n)
    
    # Save coefficients and 95% CIs
    coef_4yrs <- data.frame(estimate = rdd_4yrs$coef[1] ,
                lci = rdd_4yrs$ci[1,1] ,
                uci = rdd_4yrs$ci[1,2] ,
                bandwidth = "4 years",
                outcome = name,
                start_date = start_date)
    
    # Model - 3 years bandwidth
    df_3yrs <- subset(df, age_3mos >= 188 & age_3mos < 212)
    rdd_3yrs <- rdrobust(y = df_3yrs$p_outcome, x = df_3yrs$age_3mos_c, c = 0,
                           fuzzy = df_3yrs$p_boost, p = 1, h = 20,
                           kernel = "uniform", weights = df_3yrs$n)
    
    # Save coefficients and 95% CIs
    coef_3yrs <- data.frame(estimate = rdd_3yrs$coef[1] ,
                            lci = rdd_3yrs$ci[1,1] ,
                            uci = rdd_3yrs$ci[1,2] ,
                            bandwidth = "3 years",
                            outcome = name,
                            start_date = start_date)
    
    # Model - 2 years bandwidth
    df_2yrs <- subset(df, age_3mos >= 192 & age_3mos < 208)
    rdd_2yrs <- rdrobust(y = df_2yrs$p_outcome, x = df_2yrs$age_3mos_c, c = 0,
                         fuzzy = df_2yrs$p_boost, p = 1, h = 20,
                         kernel = "uniform", weights = df_2yrs$n)
    
    # Save coefficients and 95% CIs
    coef_2yrs <- data.frame(estimate = rdd_2yrs$coef[1] ,
                            lci = rdd_2yrs$ci[1,1] ,
                            uci = rdd_2yrs$ci[1,2] ,
                            bandwidth = "2 years",
                            outcome = name,
                            start_date = start_date)
    
    # Model - 1 years bandwidth
    df_1yrs <- subset(df, age_3mos >= 184 & age_3mos < 216)
    rdd_1yrs <- rdrobust(y = df_1yrs$p_outcome, x = df_1yrs$age_3mos_c, c = 0,
                         fuzzy = df_1yrs$p_boost, p = 1, h = 20,
                         kernel = "uniform", weights = df_1yrs$n)
    
    # Save coefficients and 95% CIs
    coef_1yrs <- data.frame(estimate = rdd_1yrs$coef[1] ,
                            lci = rdd_1yrs$ci[1,1] ,
                            uci = rdd_1yrs$ci[1,2] ,
                            bandwidth = "1 years",
                            outcome = name,
                            start_date = start_date)
    
    
    coef_bw <- rbind(coef_4yrs, coef_3yrs, coef_2yrs, coef_1yrs)
    
    # Save coefficients
    write.csv(coef_bw, here::here("output", "modelling", "iv", "bandwidth",
              paste0("coef_iv_sens_bw_",suffix,"_",start_date,".csv")),
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
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-11-26.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-11-27.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-11-28.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-11-29.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-11-30.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-01.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-02.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-03.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-04.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-05.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-06.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-07.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-08.csv"))),
    read_csv(here::here("output", "modelling", "iv", "bandwidth", paste0("coef_iv_sens_bw_",suffix,"_2022-12-09.csv")))
  )
  
  write.csv(all_coef, here::here("output", "modelling", "final", paste0("coef_iv_sens_bw_",suffix,"_","all.csv")), 
            row.names = FALSE)
  
}

comb("covidcomposite")
comb("respcomposite")
comb("anyadmitted")
comb("anydeath")



