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

dir_create(here::here("output", "covid_outcomes"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "covid_outcomes", "by_start_date"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "iv"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "final"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)


# Function for instrumental variable analysis
fuzzy <- function(start_date){
  
  # Read in data
  data <- read.csv(here::here("output", "cohort", paste0("outcomes_",start_date,".csv"))) %>%
    mutate(age_3mos = floor(age_mos / 3)) %>%
    subset(!is.na(age_3mos) & age_3mos >= 180 & age_3mos < 220) 
  
  mod <- function(out, name, suffix){
    
    # Prep data
    len <- nrow(data)
    df <- data %>%
      mutate(over50 = if_else(age_3mos >= 200, 1, 0, 0),
             age_3mos_c = as.numeric(age_3mos - 200)) %>%
      rename(outcome = {{out}}) 
    
    # Stage 1 - estimate association between instrument (over 50)
    #   and receiving booster
    model1 <- lm(boost ~ age_3mos_c*over50 + flu_vax, data = df)
    
    # Extract predicted values and merge into dataset
    pred_boost <- predict(model1)
    model1_pred <- cbind(df, pred_boost = pred_boost) 
    
    # Stage 2 - estimate association between predicted values from
    #   first model and outcome (here, the predicted values replace
    #   the instrument)
    model2 <- lm(outcome ~ age_3mos_c*pred_boost + flu_vax, data = model1_pred)
    
    # Save coefficients and 95% CIs
    coef <-  data.frame(est = model2$coefficients)
    coef2 <- coef %>%  data.frame() %>%
      mutate(var = row.names(coef)) %>%
      cbind(confint(model2), aic = AIC(model2)) %>%
      mutate(start_date = start_date,
             outcome = name) %>%
      rename(lci = `2.5 %`, uci = `97.5 %`)
    
    # Save coefficients
    write.csv(coef2, here::here("output", "modelling", "iv", paste0("coef_iv_",suffix,"_",start_date,".csv")),
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
  
  files <- list.files(here::here("output", "modelling", "iv"),
                      pattern = paste0("coef_iv_",suffix), recursive = TRUE, 
                      full.names = TRUE)
  
  all_coef <- read_csv(files) %>% bind_rows() 
  
  write.csv(all_coef, here::here("output", "modelling", "final",paste0("coef_iv_",suffix,"_","all_.csv")), 
            row.names = FALSE)
  
}

comb("covidcomp")
comb("covidadmit")
comb("covidemerg")
comb("respcomp")
comb("respadmit")
comb("anyadmit")



