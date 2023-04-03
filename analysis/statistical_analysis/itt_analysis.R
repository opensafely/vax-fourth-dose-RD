
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
dir_create(here::here("output", "covid_outcomes", "figures"), showWarnings = FALSE, recurse = TRUE)


data_nov <- read.csv(here::here("output", "covid_outcomes", "by_start_date", "outcomes_byage_3mon_2022-11-26.csv")) %>%
  subset(!is.na(age_mos3) & age_mos3 >= 540 & age_mos3 < 660)

data_sep <- read.csv(here::here("output", "covid_outcomes", "by_start_date", "outcomes_byage_3mon_2022-09-03.csv")) %>%
  subset(!is.na(age_mos3) & age_mos3 >= 540 & age_mos3 < 660)


mod_pred <- function(data, out, start, name){
  
  len <- nrow(data)
  
  df <- data %>%
    mutate(over50 = if_else(age_mos3 >= 600, 1, 0, 0),
           age_mos3_c = as.numeric(age_mos3 - 600), 
           quarter = rep(1:4, length.out = len)) %>%
    rename(outcome = {{out}}) 
  
  mod <- glm(outcome / 100000 ~ age_mos3_c*over50 , data = df, 
             family = binomial("logit"), weights = total)
  
  summary(mod)
  
  newdata <- df %>% mutate(over50 = 0)
  
  pred.df1 <- predict(mod, se.fit = TRUE, type = "response") %>% 
    data.frame() %>%
    mutate(pred1 = fit,
           lci1 = fit - 1.96*se.fit, 
           uci1 = fit + 1.96*se.fit) %>%
    select(c("pred1","lci1","uci1"))
  
  pred.df2 <- predict(mod, se.fit=TRUE, type = "response", 
                      newdata=newdata) %>%
    data.frame() %>%
    mutate(pred2= fit,
           lci2 = fit - 1.96*se.fit, 
           uci2 = fit + 1.96*se.fit) %>%
    select(c("pred2","lci2","uci2"))
  
  # Combined with original data
  df_pred <- cbind(age_mos3 = df$age_mos3, 
                total = df$total,
                pred.df1, pred.df2) %>%
    mutate(pred1 = pred1 * 100000,
           lci1 = lci1 * 100000,
           uci1 = uci1 * 100000,
           
           pred2 = pred2 * 100000,
           lci2 = lci2 * 100000,
           uci2 = uci2 * 100000,
           
           start = start,
           outcome = name)
  
  
  return(df_pred)
  
}


covidcomp_nov <- mod_pred(data_nov, rate_covidcomposite, "Index date: November 26", "COVID admission/A&E/death")
covidcomp_sep <- mod_pred(data_sep, rate_covidcomposite, "Index date: September 3", "COVID admission/A&E/death")


covidadmit_nov <- mod_pred(data_nov, rate_covidadmitted, "Index date: November 26", "COVID admission")
covidadmit_sep <- mod_pred(data_sep, rate_covidadmitted, "Index date: September 3", "COVID admission")

respcomp_nov <- mod_pred(data_nov, rate_respcomposite, "Index date: November 26", "Respiratory admission/death")
respcomp_sep <- mod_pred(data_sep, rate_respcomposite, "Index date: September 3", "Respiratory admission/death")

respadmit_nov <- mod_pred(data_nov, rate_respadmitted, "Index date: November 26", "Respiratory admission")
respadmit_sep <- mod_pred(data_sep, rate_respadmitted, "Index date: September 3", "Respiratory admission")

anyadmit_nov <- mod_pred(data_nov, rate_anyadmitted, "Index date: November 26", "Any unplanned admission")
anyadmit_sep <- mod_pred(data_sep, rate_anyadmitted, "Index date: September 3", "Any unplanned admission")


covidcomp <- rbind(covidcomp_nov, covidcomp_sep)
write.csv(covidcomp, here::here("output", "covid_outcomes", "predicted_covidcomp.csv"), row.names = FALSE)

covidadmit <- rbind(covidadmit_nov, covidadmit_sep)
write.csv(covidadmit, here::here("output", "covid_outcomes", "predicted_covidadmit.csv"), row.names = FALSE)

respcomp <- rbind(respcomp_nov, respcomp_sep)
write.csv(respcomp, here::here("output", "covid_outcomes", "predicted_respcomp.csv"), row.names = FALSE)

respadmit <- rbind(respadmit_nov, respadmit_sep)
write.csv(respcomp, here::here("output", "covid_outcomes", "predicted_respadmit.csv"), row.names = FALSE)

anyadmit <- rbind(anyadmit_nov, anyadmit_sep)
write.csv(anyadmit, here::here("output", "covid_outcomes", "predicted_anyadmit.csv"), row.names = FALSE)


################


plot <- function(outcome){
  
ggplot() + 
  geom_ribbon(data=subset(outcome, age_mos3 <= 600), 
              aes(x=age_mos3 / 12, ymin=lci2, ymax=uci2), alpha=0.2, fill = "gray50") +
  geom_ribbon(data=subset(outcome, age_mos3 >= 600), 
              aes(x=age_mos3 / 12, ymin=lci1, ymax=uci1), alpha=0.2, fill = "gray50") +
  geom_vline(data = outcome, aes(xintercept = 50), linetype = "longdash") +
  geom_line(data=subset(outcome, age_mos3 <= 600), 
            aes(x=age_mos3 / 12, y = pred2, col = start), size = .8, 
            linetype = "longdash") +
  geom_line(data=subset(outcome, age_mos3 >= 600), 
            aes(x=age_mos3 / 12, y = pred1, col = start), size = .8, 
            linetype = "longdash") +
  scale_colour_manual(values = c("dodgerblue3", "maroon")) +
  scale_y_continuous(expand = expansion(mult = c(.1, .1))) +
  xlab("Age") + ylab("No. events per 100,000 (predicted)") +
  facet_wrap(~ start, nrow = 2, scales = "free_y") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
}

plot(covidcomp)


ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_covidcomp.png"),
       dpi = 300, units = "in", width = 6, height = 6)

plot(covidadmit)

ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_covidadmit.png"),
       dpi = 300, units = "in", width = 6, height = 6)

plot(respcomp)

ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_respcomp.png"),
       dpi = 300, units = "in", width = 6, height = 6)

plot(respadmit)

ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_respadmit.png"),
       dpi = 300, units = "in", width = 6, height = 6)

plot(anyadmit)

ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_anyadmit.png"),
       dpi = 300, units = "in", width = 6, height = 6)