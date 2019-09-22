# Write a function to calculate the jail admission rates and arrest rates
cal_rates <- function(dat, use.pop.jail = TRUE, method = "direct") {
  
  dat <- arrange(dat, YEAR)
  
  if(use.pop.jail) {
    dat$pop <- dat$pop_jail
  } else {
    dat$pop <- dat$pop_arrest
  }
  
  if (method == "direct") {
    # Aggregate number of arrests and number of jail admission
    rates <- dat %>%
      group_by(YEAR) %>%
      summarise(## Creat indicators for whether all the data are missing for the year
        missing_arrest = mean(is.na(total_arrest)),
        missing_jail = mean(is.na(total_jail)),
        ## Aggregate the numbers
        total_arrest = sum(total_arrest, na.rm = T), 
        total_jail = sum(total_jail, na.rm = T),
        total_pop = sum(pop, na.rm = T))
    
    # If all the data are missing for the year, then convert the 0 to NA
    rates$total_arrest[rates$missing_arrest==1] <- NA
    rates$total_jail[rates$missing_jail==1] <- NA
    
    rates <- rates %>%
      mutate(rate_arrest = total_arrest/total_pop * 100000,
             rate_jail = total_jail/total_pop * 100000)
  }
  
  if (method == "weighted" ) {
    rates <- dat %>%
      mutate(rate_arrest = total_arrest / pop * 100000,
             rate_jail = total_jail / pop * 100000) %>%
      group_by(YEAR) %>%
      mutate(total_pop = sum(pop, na.rm = T),
             wt = pop / total_pop,
             rate_arrest_wt = rate_arrest * wt,
             rate_jail_wt = rate_jail * wt,
             missing_arrest = mean(is.na(total_arrest)),
             missing_jail = mean(is.na(total_jail)))
    
    rates <- rates %>%
      group_by(YEAR) %>%
      summarise(total_arrest = sum(total_arrest, na.rm = T),
                total_jail = sum(total_jail, na.rm = T),
                total_pop = sum(pop, na.rm = T),
                rate_arrest = sum(rate_arrest_wt, na.rm = T),
                rate_jail = sum(rate_jail_wt, na.rm = T),
                missing_arrest = mean(missing_arrest),
                missing_jail = mean(missing_jail))
    
    rates$total_arrest[rates$missing_arrest==1] <- NA
    rates$total_jail[rates$missing_jail==1] <- NA
    rates$rate_arrest[rates$missing_arrest==1] <- NA
    rates$rate_jail[rates$missing_jail==1] <- NA
  }
  
  if (method == "simple") {
    # Turn the 0's in the population variable into NA
    dat$pop[dat$pop==0] <- NA
    
    # Calcualte the rates
    rates <- dat %>%
      mutate(rate_arrest = total_arrest / pop * 100000,
             rate_jail = total_jail / pop * 100000)
    
    rates <- rates %>%
      group_by(YEAR) %>%
      summarise(missing_arrest = mean(is.na(total_arrest), na.rm = T),
                missing_jail = mean(is.na(total_jail), na.rm = T),
                total_arrest = sum(total_arrest, na.rm = T),
                total_jail = sum(total_jail, na.rm = T),
                total_pop = sum(pop, na.rm = T),
                rate_arrest = mean(rate_arrest, na.rm = T),
                rate_jail = mean(rate_jail, na.rm = T))
    
    
    rates$rate_arrest[rates$missing_arrest==1] <- NA
    rates$rate_jail[rates$missing_jail==1] <- NA
  }
  
  
  rates <- rates %>%
    select(YEAR, 
           total_arrest, total_jail, 
           rate_arrest, rate_jail, 
           total_pop, 
           missing_arrest, missing_jail)
  
  
  return(rates)
}

