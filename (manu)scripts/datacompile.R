
##############################################################################
############################### Ho et al., 2022 ##############################
##############################################################################

# EXPLANATION ####
# this script compiles all data for the analyses in Ho et al., 2022 manuscript.
# changes from the AMDP thesis data include:
# 1. personality master file used is the one updated by ARM (May 2022)
# 2. includes squirrels from AG grid
# 3. uses trial 1 of KL 2017-2018 instead of trial 2
# 4. removes the 60-80 days filter to look at age effects
# 5. does not use maternal care data
# the personality_updated dataframe, as well as oft1 and mis1 scores, were 
# generated in personality_updated.R

# adds bucket_access variable to indicate offspring of food-supp moms
personality_updated <- personality_updated %>%
  mutate(grid = as.factor(grid),
         year = as.factor(year),
         gridyear = as.factor(paste(grid, year)),
         bucket_access = as.factor(as.integer(grid == "AG")))
# we only have 8 AG juvs? recheck the personality master file

# need to get survival data once we have part dates
# add in spring grid density
# get survival data for 2021 cohort once may 15 2022 census data is in


juv_personality <- merge(personality_updated,
                         juv_litter, #taken from dataconsol.R
                         by = "juv_id",
                         all.x = TRUE) %>%
  transmute(litter_id,
            year,
            juv_id,
            sex = sex.x,
            grid = grid.x,
            birth_date = as.Date(fieldBDate, "%Y-%m-%d"),
            julian_birth_date = yday(birth_date),
            weight,
            tagWT,
            growth,
            n_days,
            growthrate,
            oft1,
            mis1,
            trialdate,
            julian_trialdate) %>%
  mutate(julian_birth_date = yday(birth_date),
         age_trial = as.numeric(difftime(trialdate, birth_date,
                                         units = "days")))

na_bdate <- juv_personality %>%
  filter(is.na(birth_date))

write_csv(na_bdate,
          "data/missing_bdates.csv")
