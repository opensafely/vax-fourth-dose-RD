
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


data_nov <- read.csv(here::here("output", "covid_outcomes", "by_start_date", "outcomes_byage_3mon_2022-11-26.csv")) 
data_sep <- read.csv(here::here("output", "covid_outcomes", "by_start_date", "outcomes_byage_3mon_2022-09-03.csv")) 


mod_pred <- function(data, out, start, name){
  
  len <- length(data)
  
  df <- data %>%
    subset(!is.na(age_mos3)) %>%
    mutate(over50 = (age_mos3 >= 600),
           age_mos3_c = age_mos3 - 600, 
           quarter = as.factor(rep(1:4, len))) %>%
    rename(outcome= {{out}}) 
  
  mod <- glm(outcome / 100000 ~ age_mos3_c*over50 + as.factor(quarter), data = df, 
             family = binomial("logit"), weights = total)
  
  pred <- predict(mod, type = "response", se.fit = TRUE)
  
  df_pred <- cbind(age_mos3 = df$age_mos3, birth_qtr = df$quarter, 
                   predicted = pred$fit, se = pred$se.fit) %>%
    as.data.frame() %>%
    mutate(
           lci = (predicted - 1.96 * se) * 100000, 
           uci = (predicted + 1.96 * se) * 100000,
           predicted = predicted * 100000,
           start = start,
           outcome = name) %>%
    dplyr::select(!se)
  
  return(df_pred)
  
}

covidcomp_nov <- mod_pred(data_nov, rate_covidcomposite, "Index date: November 26", "COVID admission/A&E/death")
covidcomp_sep <- mod_pred(data_sep, rate_covidcomposite, "Index date: September 3", "COVID admission/A&E/death")

respcomp_nov <- mod_pred(data_nov, rate_respcomposite, "Index date: November 26", "Respiratory admission/death")
respcomp_sep <- mod_pred(data_sep, rate_respcomposite, "Index date: September 3", "Respiratory admission/death")

anyadmit_nov <- mod_pred(data_nov, rate_anyadmitted, "Index date: November 26", "Any unplanned admission")
anyadmit_sep <- mod_pred(data_sep, rate_anyadmitted, "Index date: September 3", "Any unplanned admission")


covidcomp <- rbind(covidcomp_nov, covidcomp_sep)
write.csv(covidcomp, here::here("output", "covid_outcomes", "predicted_covidcomp.csv"), row.names = FALSE)

respcomp <- rbind(respcomp_nov, respcomp_sep)
write.csv(respcomp, here::here("output", "covid_outcomes", "predicted_respcomp.csv"), row.names = FALSE)

anyadmit <- rbind(anyadmit_nov, anyadmit_sep)
write.csv(anyadmit, here::here("output", "covid_outcomes", "predicted_anyadmit.csv"), row.names = FALSE)


################

ggplot(covidcomp, aes(x = age_mos3 / 12)) + 
  geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.2, fill = "gray50") +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_line(aes(y = predicted, col = start)) +
  scale_colour_manual(values = c("dodgerblue3", "maroon")) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  xlab("Age") + ylab("No. events per 100,000 (predicted)") +
  facet_wrap(~ start, nrow = 2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_covidcomp.png"),
       dpi = 300, units = "in", width = 6, height = 6)


ggplot(respcomp, aes(x = age_mos3 / 12)) + 
  geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.2, fill = "gray50") +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_line(aes(y = predicted, col = start)) +
  scale_colour_manual(values = c("dodgerblue3", "maroon")) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  xlab("Age") + ylab("No. events per 100,000 (predicted)") +
  facet_wrap(~ start, nrow = 2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_respcomp.png"),
       dpi = 300, units = "in", width = 6, height = 6)


ggplot(anyadmit, aes(x = age_mos3 / 12)) + 
  geom_ribbon(aes(ymin=lci, ymax=uci), alpha=0.2, fill = "gray50") +
  geom_vline(aes(xintercept = 50), linetype = "longdash") +
  geom_line(aes(y = predicted, col = start)) +
  scale_colour_manual(values = c("dodgerblue3", "maroon")) +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  xlab("Age") + ylab("No. events per 100,000 (predicted)") +
  facet_wrap(~ start, nrow = 2) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.title = element_blank(), legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("output", "covid_outcomes", "figures", "plot_pred_anyadmit.png"),
       dpi = 300, units = "in", width = 6, height = 6)