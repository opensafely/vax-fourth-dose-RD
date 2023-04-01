


#######################################
# Functions
#######################################

## Redaction
redact <- function(vars) {
  case_when(vars > 7 ~ vars)
}

## Rounding
rounding <- function(vars) {
  round(vars / 5) * 5
}

## Mid 6 rounding
roundmid_any <- function(x, to=6){
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}


# Factorise ----
fct_case_when <- function(...) {
  # uses dplyr::case_when but converts the output to a factor,
  # with factors ordered as they appear in the case_when's  ... argument
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}


## Calculate total COVID events by age (months) and index date 
outcome_by_age <- function(data, index_date){
  
  data1 <- read_csv(here::here("output", "cohort", data)) %>% 
    mutate(date = as.Date(date, format = "%Y-%m-%d"),
           
           # Calculate age on index date
           age_mos = (dob %--% as.Date(index_date)) %/% months(1),
           age_yrs = (dob %--% as.Date(index_date)) %/% years(1)) %>%
    
    # Exclude people who died prior to index date
    subset(is.na(dod) | dod >= as.Date(index_date)) %>%
    
    group_by(age_mos) %>%
    mutate(# Denominator by age in months
      total = data.table::uniqueN(patient_id),
      
      event = if_else(date > as.Date(index_date) + 28 |
                        date < as.Date(index_date),
                      1, 0, 0)) %>%
    group_by(patient_id, age_mos, total) %>%
    # Create flag for people with outcome within follow-up window
    summarise(event = max(event)) 
  
  data2 <- data1 %>%
    group_by(age_mos, total) %>%
    summarise(n_event = sum(event)) %>%
    mutate( n_event_red = case_when(n_event > 7 ~ n_event),
            n_event_red = round(n_event_red / 5) * 5,
            total_red = case_when(total > 7 ~ total),
            total_red = round(total / 5) * 5,
            rate_outcome = n_event / total * 100000,
            rate_outcome_red = n_event_red / total_red * 100000,
            index_date = index_date)
  
  return(data2)
  
}
