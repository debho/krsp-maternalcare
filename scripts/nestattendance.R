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
                     sep = ",",
                     na.strings = c("", " ", "NA")) %>% #n = 824
  filter(nest == "1") %>% #n = 451
  drop_na(m_return)
  

# censors all >420 and NA latencies to 420
nest_att[(nest_att$t_return > 421 | is.na(nest_att$t_return)),
         "m_return"] <- "n" #if i put 420 it makes all my 420s "n" for some reason
nest_att[nest_att$m_return == "n",
         "t_return"] <- 421 #if i put 420 it turns all my 420 + "y" to "n"
nest_att[(nest_att$t_move > 421 | is.na(nest_att$t_move)),
         "m_move"] <- "n" #if i put 420 it makes all my 420s "n" for some reason
nest_att[nest_att$m_move == "n",
         "t_move"] <- 421

#converts dates to julian dates
nest_att$julian_date <- yday(as.Date(nest_att$date,
                         "%m/%d/%y"))
nest_att$julian_birth_date <- yday(as.Date(nest_att$birth_date,
                               "%m/%d/%y"))

#gets ages of pups, as.numeric to handle 0 day old pups
nest_att$age <- as.integer(nest_att$julian_date - nest_att$julian_birth_date)



