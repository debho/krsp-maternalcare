
##############################################################################
############################### Ho et al., 2022 ##############################
##############################################################################

# EXPLANATION ####
# this script handles the data wrangling for analyses for the Ho et al., 2022
# manuscript.
# changes from the AMDP thesis data include:
# 1. personality master file used is the one updated by ARM (May 2022)
# 2. includes squirrels from AG grid

personality_updated <- read.csv("data/personality-master-updated.csv",
                                header = T)

