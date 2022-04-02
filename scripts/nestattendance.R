##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff

##############################################################################
### Script for preparing nest attendance data for analysis

#reading in nest attendance data and cleans it
nest_att <- read.csv("data/AllNests.csv",
                     header = TRUE,
                     sep = ",") %>%
  filter(year > 2017) %>% #to take only 2018-2021
  drop_na(charges,  #removes any rows where values weren't specified
         mock_falls,
         rattles)

#censoring latencies at 420
nest_att$t_return[nest_att$t_return > 420] <- NA
nest_att$t_move[nest_att$t_move > 420] <- NA

#coding any NA latencies as "n"
nest_att[is.na(nest_att$t_return), "m_return"] <- "n"
nest_att[is.na(nest_att$t_move), "m_move"] <- "n"

#converts dates to julian dates
nest_att$date <- as.Date(nest_att$date,
                         "%m/%d/%y")
nest_att$julian_date <- yday(nest_att$date)

nest_att$birth_date <- as.Date(nest_att$birth_date,
                               "%m/%d/%y")
nest_att$julian_birth_date <- yday(nest_att$birth_date)

#gets ages of pups, as.numeric to handle 0 day old pups
nest_att$age <- (as.numeric(nest_att$julian_date) 
                 - as.numeric(nest_att$julian_birth_date))



