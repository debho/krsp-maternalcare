
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# EXPLANATION ####
# this script used data from the "flastall2" table in the KRSP database
# to identify whether an individual survived at least 60 days, and then 200 days
# this survival data was based on the date that the individual was first
# sighted ("dates") and the date that the individual was last sighted ("datee")

survival <- flastall %>%
  mutate(age_last = as.integer(difftime(datee, dates, units = "days")),
         survived_200d = age_last >= 200,
         survived_60d = age_last >= 60) %>%
  transmute(juv_id = squirrel_id,
            start_date = dates,
            end_date = datee,
            last_fate = f2,
            age_last,
            survived_200d,
            survived_60d,
            byear)
#if end date is earlier than late-sept/early oct of 2021 it means that squirrel
#is presumed to be dead


survival$survived_200d <- as.numeric(survival$survived_200d)
survival$survived_60d <- as.numeric(survival$survived_60d)

