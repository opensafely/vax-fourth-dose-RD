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
dir_create(here::here("output", "modelling"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling", "final"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "modelling","figures"), showWarnings = FALSE, recurse = TRUE)
dir_create(here::here("output", "cohort"), showWarnings = FALSE, recurse = TRUE)


### Function to run sharp RD analysis, output coefficients and predicted values
### Plot crude rates

sharp <- function(start_date){
  
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
    
    # Model
    mod <- lm(outcome ~ age_3mos_c*over50, data = df)

    # Save coefficients and 95% CIs
    coef <-  data.frame(est = mod$coefficients)
    coef2 <- coef %>%  data.frame() %>%
      mutate(var = row.names(coef)) %>%
      cbind(confint(mod), aic = AIC(mod)) %>%
      mutate(start_date = start_date,
              outcome = name) %>%
      rename(lci = `2.5 %`, uci = `97.5 %`)
    
    # Save coefficients
    write.csv(coef2, here::here("output", "modelling", paste0("coef_",suffix,"_",start_date,".csv")),
              row.names = FALSE)
    
    # Predicted values 
    origdata <- df %>% distinct(age_3mos, age_3mos_c, over50) %>%
      arrange(age_3mos_c) 
    
    pred.df1 <- predict(mod, se.fit = TRUE, type = "response",
                        newdata = origdata) %>% 
      data.frame() %>%
      mutate(pred1 = fit,
             lci1 = fit - 1.96*se.fit, 
             uci1 = fit + 1.96*se.fit) %>%
      select(c("pred1","lci1","uci1"))
    
    # Predicted counterfactual values
    newdata <- df %>% distinct(age_3mos, age_3mos_c, over50) %>%
      arrange(age_3mos_c) %>%
      mutate(over50 = 0) 
    
    pred.df2 <- predict(mod, se.fit=TRUE, type = "response", 
                        newdata=newdata) %>%
      data.frame() %>%
      mutate(pred2= fit,
             lci2 = fit - 1.96*se.fit, 
             uci2 = fit + 1.96*se.fit) %>%
      select(c("pred2","lci2","uci2"))
    
    # Combine with original data
    df_pred <- cbind(pred.df1, pred.df2, origdata) %>%
      mutate(start = start_date,
             outcome = name)
    
    write.csv(df_pred, here::here("output", "modelling", paste0("predicted_",suffix,"_",start_date,".csv")), row.names = FALSE)
  
    ggplot() + 
      geom_ribbon(data=subset(df_pred, age_3mos <= 200), 
                  aes(x=age_3mos / 4, ymin=lci2, ymax=uci2), alpha=0.2, fill = "gray50") +
      geom_ribbon(data=subset(df_pred, age_3mos >= 200), 
                  aes(x=age_3mos / 4, ymin=lci1, ymax=uci1), alpha=0.2, fill = "gray50") +
      #geom_point(data=df_pred, aes(x = age_3mos / 4, y = rate), size = 1.25, alpha= .5) +
      geom_vline(data=df_pred, aes(xintercept = 50), linetype = "longdash") +
      geom_line(data=subset(df_pred, age_3mos <= 200), 
                aes(x=age_3mos / 4, y = pred2), size = .8, 
                linetype = "longdash") +
      geom_line(data=subset(df_pred, age_3mos >= 200), 
                aes(x=age_3mos / 4, y = pred1), size = .8, 
                linetype = "longdash") +
      scale_colour_manual(values = c("dodgerblue3", "maroon", "forestgreen")) +
      scale_y_continuous(expand = expansion(mult = c(.1, .1))) +
      xlab("Age") + ylab("No. events per 100,000 (predicted)") +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.background = element_blank(),
            strip.text = element_text(hjust = 0),
            legend.title = element_blank(), legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave(here::here("output", "modelling", "figures", paste0("plot_pred_",suffix,start_date,".png")),
           dpi = 300, units = "in", width = 6, height = 8)
    
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
start_dates <- c(as.Date("2022-09-03"), as.Date( "2022-10-15"), as.Date(0:10, origin = "2022-11-26")) 

# Run function over all dates
sapply(start_dates, sharp)



### Combine all coefficients files into one ###
comb <- function(suffix){
  
  files <- list.files(here::here("output", "modelling"),
                    pattern = paste0("coef_",suffix), recursive = TRUE, 
                    full.names = TRUE)

  all_coef <- read_csv(files) %>% bind_rows() 

  write.csv(all_coef, here::here("output", "modelling", "final", paste0("coef_",suffix,"_","all_.csv")), row.names = FALSE)

}

comb("covidcomp")
comb("covidadmit")
comb("covidemerg")
comb("respcomp")
comb("respadmit")
comb("anyadmit")


