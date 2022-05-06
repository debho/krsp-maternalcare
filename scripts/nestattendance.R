
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# EXPLANATION ####
# this script was used to prepare nest attendance data for analysis. i read
# in data from AllNestsCENS.csv, which is the master file for nest attendance
# data but with columns added in to censor latencies for moms that did not
# return/move within 7 minutes to 420 seconds (the maximum duration)
# i also converted the the m_return and m_move variables to binary variables
# indicating whether a mom returned/moved pups in ≤ 420 seconds

# reading in nest attendance data and cleans it
nest_attALL <- read.csv("data/AllNestsCENS.csv",
                     header = TRUE,
                     sep = ",",
                     na.strings = c("", " ", "NA")) #n = 824

# converts dates to julian dates
nest_attALL$date <- as.Date(nest_attALL$date,
                           "%m/%d/%y")
nest_attALL$julian_date <- yday(nest_attALL$date)
nest_attALL$birth_date <- as.Date(nest_attALL$birth_date,
                                  "%m/%d/%y")
nest_attALL$julian_birth_date <- yday(nest_attALL$birth_date)

# gets ages of pups, as.integer to handle 0 day old pups
nest_attALL$age <- as.integer(nest_attALL$julian_date - nest_attALL$julian_birth_date)

# makes returning and moving binary var
nest_attALL$m_return <- as.numeric(nest_attALL$m_return == "y")
nest_attALL$m_move <- as.numeric(nest_attALL$m_move == "y")

nest_att <- nest_attALL %>%
  filter(nest == 1) %>% # takes only Nest 1
  drop_na(return_lat,
          move_lat)
# removes the ambiguous cases where it was coded as y but no times

nest1 <- nest_attALL %>%
  filter(nest == 1)
nest2 <- nest_attALL %>%
  filter(nest == 2)
