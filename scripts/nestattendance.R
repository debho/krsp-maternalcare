##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for preparing nest attendance data for analysis

#reading in nest attendance data and cleans it
nest_att <- read.csv("data/AllNestsCENS.csv",
                     header = TRUE,
                     sep = ",",
                     na.strings = c("", " ", "NA")) %>% #n = 824
  filter(nest == 1) %>%
  drop_na(return_lat,
          move_lat) #removes the ambiguous cases where it was coded as y but no times

#converts dates to julian dates
nest_att$date <- as.Date(nest_att$date,
                         "%m/%d/%y")
nest_att$julian_date <- yday(nest_att$date)
nest_att$birth_date <- as.Date(nest_att$birth_date,
                               "%m/%d/%y")
nest_att$julian_birth_date <- yday(nest_att$birth_date)

#gets ages of pups, as.integer to handle 0 day old pups
nest_att$age <- as.integer(nest_att$julian_date - nest_att$julian_birth_date)

#makes returning and moving binary var
nest_att$m_return <- as.factor(as.integer(nest_att$m_return == "y"))
nest_att$m_move <- as.factor(as.integer(nest_att$m_move == "y"))


