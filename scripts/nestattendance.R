##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for preparing nest attendance data for analysis

#reading in nest attendance data and cleans it
nest_attALL <- read.csv("data/AllNestsCENS.csv",
                     header = TRUE,
                     sep = ",",
                     na.strings = c("", " ", "NA")) #n = 824

#converts dates to julian dates
nest_attALL$date <- as.Date(nest_attALL$date,
                           "%m/%d/%y")
nest_attALL$julian_date <- yday(nest_attALL$date)
nest_attALL$birth_date <- as.Date(nest_attALL$birth_date,
                                  "%m/%d/%y")
nest_attALL$julian_birth_date <- yday(nest_attALL$birth_date)

#gets ages of pups, as.integer to handle 0 day old pups
nest_attALL$age <- as.integer(nest_attALL$julian_date - nest_attALL$julian_birth_date)

#makes returning and moving binary var
nest_attALL$m_return <- as.numeric(nest_attALL$m_return == "y")
nest_attALL$m_move <- as.numeric(nest_attALL$m_move == "y")

nest_att <- nest_attALL %>%
  filter(nest == 1) %>%
  drop_na(return_lat,
          move_lat) #removes the ambiguous cases where it was coded as y but no times




