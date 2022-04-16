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
  transmute(juv_id,
            litter_id,
            sex,
            fieldBDate,
            mom_id,
            grid,
            yr,
            date1,
            date2 = tagDt,
            n_days = as.numeric(difftime(date2, date1,
                                         units = "days")),
            weight,
            tagWT,
            growth = (tagWT - weight)) %>%
  naniar::replace_with_na(replace = list(n_days = 0)) %>%
  mutate(growthrate = (growth / n_days))
            

# STEP 2 ####
## now we match the moms and attentiveness data

juv_care <- merge(juv_litter,
                  nest_att,
                  by = "litter_id",
                  all.x = TRUE) %>%
  transmute(litter_id,
            n_pups,
            juv_id,
            birth_date = fieldBDate,
            julian_birth_date,
            mom_id,
            date,
            julian_date,
            t_return,
            return_lat,
            t_move,
            move_lat,
            m_return,
            m_move,
            n_days,
            weight,
            tagWT,
            growth,
            growthrate)

# STEP 3 ####
## now for personality

juv_personality <- merge(personality,
                         juv_care,
                         by = "juv_id",
                         all.x = TRUE)%>%
  transmute(litter_id,
             n_pups,
             mom_id,
             t_return,
             return_lat,
             t_move,
             move_lat,
             m_return,
             m_move,
             year,
             juv_id,
             sex,
             grid,
             birth_date = as.Date(birth_date, "%Y-%m-%d"),
             julian_birth_date,
             n_days,
             weight,
             tagWT,
             growth,
             growthrate,
             oft1,
             mis1,
             trialdate,
             julian_trialdate) %>%
  mutate(julian_birth_date = yday(birth_date))

juv_personality$age_trial <- (juv_personality$julian_trialdate -
                              juv_personality$julian_birth_date)

# STEP 4 ####
## add in survival and we have our master table

master <- merge(juv_personality,
                survival,
                by = "juv_id",
                all.x = TRUE) %>%
  transmute(litter_id,
            mom_id,
            n_pups,
            t_return,
            return_lat,
            m_return,
            t_move,
            move_lat,
            m_move,
            year,
            juv_id,
            sex,
            grid,
            birth_date,
            julian_birth_date,
            n_days,
            weight,
            tagWT,
            growth,
            growthrate,
            oft1,
            mis1,
            trialdate,
            julian_trialdate,
            age_trial,
            age_last,
            last_fate,
            survived_200d)

# adding in treatments
master$treatment <- as.factor(as.integer((master$grid == "JO") |
                                           (master$grid == "RR")))
master$gridtreat <- factor(master$grid,
                           levels = c("JO", "RR", "BT", "KL", "SU", "SUX"),
                           labels = c(2, 2, 1, 0, 0, 0))

# cleaning up trial dates that occur before end dates
master$age_last <- ifelse((master$age_last < master$age_trial),
                          master$age_trial,
                          master$age_last)

#add in missing age_trials
master$birth_date <- as.Date(master$birth_date,
                             "%y/%m/%d")
master$julian_birth_date <- yday(master$birth_date)

master$mastyear <- as.factor(as.integer(master$year == 2005 |
                                          master$year == 2019))
master <- merge(master, grids_density,
                by = c("grid", "year"),
                all.x = TRUE) %>%
  distinct(juv_id,
           .keep_all = TRUE) %>% 
  mutate(grid = as.factor(grid),
         litter_id = as.factor(litter_id),
         mom_id = as.factor(mom_id),
         year = as.factor(year),
         juv_id = as.factor(juv_id),
         sex = as.factor(sex),
         survived_200d = as.factor(survived_200d))

recent4 <- master %>%
  filter(year %in% c(2018, 2019, 2020, 2021))


# squirrels with no  dates of birth:
# 24809 23788 24830 24680 24753 24729 24730 24745 23797 23889 23906 24594 23856
# 23931 13235 23256 23894 23903

# STEP 5 ####
## add in LSR
## group by litter_id, find no. of females in each litter
## then divide by n_pups to get proportion of females

LSR <- master %>%
  group_by(litter_id) %>%
  count(litter_id,sex) %>%
  filter(sex == "F")


