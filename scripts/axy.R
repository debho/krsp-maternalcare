
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# SCRIPT NOT IN USE FOR NOW 

sq_11256 <- read_csv("data/axy/11256_006_20150424.csv") %>%
  filter(nest == "Nest",
         asleep == 0)
# ok i definitely need a more efficient way to do this...
