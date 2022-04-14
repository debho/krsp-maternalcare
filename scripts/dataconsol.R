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
            yr)

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
            t_move,
            m_return,
            m_move)

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
             t_move,
             m_return,
             m_move,
             year,
             juv_id,
             sex,
             grid,
             birth_date,
             julian_birth_date,
             oft1,
             mis1,
             trialdate) 


juv_personality$julian_birth_date <- yday(juv_personality$birth_date)
juv_personality$julian_trialdate <- yday(as.Date(juv_personality$trialdate,
                                            "%m/%d/%y"))
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
            t_return,
            t_move,
            m_return,
            m_move,
            year,
            juv_id,
            sex,
            grid,
            birth_date, #something went wrong with this and now it's also showing julian birth date
            julian_birth_date,
            n_pups,
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

master$mastyear <- as.integer(master$year == 2005 | master$year == 2019)
master <- merge(master, grids_density,
                by = "grid",
                all.x = TRUE) %>%
  rename(year = year.x)  %>%
  distinct(juv_id,
           .keep_all = TRUE)

master$yearF <- as.factor(master$year)
master$m_return <- as.integer(master$m_return == "y")
master$m_move <- as.integer(master$m_move == "y")

recent4 <- master %>%
  filter(year > 2017)

# STEP 5 ####
## add in LSR
## group by litter_id, find no. of females in each litter
## then divide by n_pups to get proportion of females

LSR <- master %>%
  group_by(litter_id) %>%
  count(litter_id,sex) %>%
  filter(sex == "F")