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
                     sep = ",") %>% #n = 824
  filter(!pups_bled == "y") # take out pups_bled bc of interaction with sampling
#n = 681 after removing nest 2 observations

# censors all >420 and NA latencies to 420
nest_att[nest_att$t_return > 420 | is.na(nest_att$t_return),
         "m_return"] <- "n"
nest_att[nest_att$t_move > 420 | is.na(nest_att$t_move),
         "m_move"] <- "n"
nest_att[nest_att$m_return == "n",
         "t_return"] <- 420
nest_att[nest_att$m_move == "n",
         "t_move"] <- 420

#converts dates to julian dates
nest_att$julian_date <- yday(as.Date(nest_att$date,
                         "%m/%d/%y"))
nest_att$julian_birth_date <- yday(as.Date(nest_att$birth_date,
                               "%m/%d/%y"))

#gets ages of pups, as.numeric to handle 0 day old pups
nest_att$age <- as.integer(nest_att$julian_date - nest_att$julian_birth_date)



