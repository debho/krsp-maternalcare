##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for consolidating data

# STEP 1 ####
## matching juveniles to litters
## should match on litter ID

# but first lets clean up some column names to avoid confusion
colnames(litters)[1] <- "litter_id"
colnames(litters)[25] <- "mom_id"
colnames(juveniles)[17] <- "juv_id"

juv_litter <- merge(juveniles,
                    litters,
                    by = "litter_id",
                    all.x = TRUE) %>%
  select(juv_id,
         litter_id,
         sex,
         mom_id,
         grid,
         yr)


# STEP 2 ####
## now we match the moms and attentiveness data

juv_care <- merge(juv_litter,
                  nest_att,
                  by = "litter_id",
                  all.x = TRUE) %>%
  select(litter_id,
         juv_id,
         birth_date,
         julian_birth_date,
         mom_id,
         date,
         julian_date,
         t_return,
         t_move,
         bark)

# STEP 3 ####
## now for personality

juv_personality <- merge(personality,
                         juv_care,
                         by = "juv_id",
                         all.x = TRUE)%>%
  select(litter_id,
         mom_id,
         t_return,
         t_move,
         year,
         juv_id,
         sex,
         grid,
         birth_date,
         julian_birth_date,
         oft1,
         mis1,
         trialdate,
         julian_trialdate) %>%
  drop_na(t_return,
          t_move)

juv_personality$age_trial <- (juv_personality$julian_trialdate -
                               juv_personality$julian_birth_date)

# STEP 4 ####
## add in survival and we have our master table!

master <- merge(juv_personality,
                survival,
                by = "juv_id",
                all.x = TRUE) %>%
  select(litter_id,
         mom_id,
         t_return,
         t_move,
         year,
         juv_id,
         sex,
         grid,
         birth_date,
         julian_birth_date,
         oft1,
         mis1,
         trialdate,
         julian_trialdate,
         age_trial,
         age_last,
         survived_200d)

# adding in treatments
master$treatment <- as.integer(master$grid == "JO")

# cleaning up anything that looks weird
master$age_last <- ifelse((master$age_last < master$age_trial),
                          master$age_trial,
                          master$age_last)