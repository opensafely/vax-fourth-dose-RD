################################################################
# This script:
# - Conducts regression model and plots predicted values
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
dir_create(here::here("output", "modelling", "iv"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "final"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)


# Function for instrumental variable analysis
fuzzy <- function(start_date){
  
  # Read in data
  data <- read.csv(here::here("output", "cohort", paste0("outcomes_",start_date,".csv"))) %>%
    mutate(age_3mos = floor(age_mos / 3),
           over50 = if_else(age_3mos >= 200, 1, 0, 0),
           age_3mos_c = as.numeric(age_3mos - 200)) %>%
    subset(!is.na(age_3mos) & age_3mos >= 180 & age_3mos < 220)
  
  mod <- function(out, name, suffix){
    
    # Prep data
    df <- data  %>%
      group_by(age_3mos_c, flu_vax, over50) %>%
      summarise(n = n(), 
                boost = sum(boost),
                p_boost = boost / n * 100,
                outcome = sum({{out}}),
                p_outcome = outcome / n * 100) 
    
    rdd = rdrobust(y = df$p_outcome , x = df$age_3mos_c, c = 0,
                       fuzzy = df$p_boost, covs = df$flu_vax, p = 1, h = 60, 
                       kernel="uniform", weights = df$n)
  
    # Save coefficients and 95% CIs
    coef <- data.frame(estimate = rdd$coef[1],
                lci = rdd$ci[1,1],
                uci = rdd$ci[1,2])
    
    # Save coefficients
    write.csv(coef, here::here("output", "modelling", "iv",
              paste0("coef_iv_",suffix,"_",start_date,".csv")),
              row.names = FALSE)
    
  }
  
  # Run for each outcome
  mod(covidcomposite, "COVID unplanned admission/A&E/death", "covidcomp")
  mod(covidadmitted, "COVID unplanned admission", "covidadmit")
  mod(covidemerg, "COVID A&E", "covidemerg")
  mod(respcomposite, "Respiratory composite", "respcomp")
  mod(respadmitted, "Respiratory admission", "respadmit")
  mod(anyadmitted, "All cause unplanned admission", "anyadmit")  
  
}


# Create list of dates
start_dates <- c(as.Date("2022-09-03"), as.Date("2022-10-15"), 
                 as.Date(0:10, origin = "2022-11-26")) 

# Run function over all dates
sapply(start_dates, fuzzy)


### Combine all coefficients files into one ###
comb <- function(suffix){
  
  all_coef <- bind_rows(
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-09-03.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-10-15.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-11-26.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-11-27.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-11-28.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-11-29.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-11-30.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-12-01.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-12-02.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-12-03.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-12-04.csv"))),
    read_csv(here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_2022-12-05.csv")))
  )
  
  write.csv(all_coef, here::here("output", "modelling", "final",paste0("coef_iv_",suffix,"_","all_.csv")), 
            row.names = FALSE)
  
}

comb("covidcomp")
comb("covidadmit")
comb("covidemerg")
comb("respcomp")
comb("respadmit")
comb("anyadmit")



