
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# EXPLANATION ####
# this script was used to consolidate data from the KRSP database with nest
# attendance data and personality data. 
# see dataimport.R for script where I pulled in tables from the KRSP database.
# doing this allowed me to match juveniles to their mothers through their
# litter IDs
# in this script, i also prepare the data for analysis by standardizing
# continuous variables by group and converting categorical variables into
# factors. see below for more details on what was done to each variable.
# grid density data is the density per grid during the May census. see
# density.R for details.

# first lets clean up some column names to avoid confusion
colnames(litters)[1] <- "litter_id"
colnames(litters)[25] <- "mom_id"
colnames(juveniles)[17] <- "juv_id"

# STEP 1 ####
## matching juveniles to litters via litter_id
# KRSP tables used: "juvenile" and "litter"

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
  mutate(growthrate = (growth / n_days)) # adds in growth rate info
            

# STEP 2 ####
## adding in nest attendance data to the table, matching on litter_id
# see nestattendance.R for script on preparing nest attendance data for analysis

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
## adding in cleaned personality data to the table, matching by juv_id
# see personality.R for script on cleaning personality data

juv_personality <- merge(personality,
                         juv_care,
                         by = "juv_id",
                         all = TRUE)%>%
  filter(!grid == "AG") %>% #removes food-supplemented grid
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
  mutate(julian_birth_date = yday(birth_date),
         age_trial = as.numeric(difftime(trialdate, birth_date,
                                         units = "days")))

# STEP 4 ####
## adding in survival data
# see survival.R to see how this data was prepared from the data in the KRSP
# flastall2 table

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
            end_date,
            survived_200d,
            survived_60d) %>%
  filter(age_trial >= 60,
           age_trial <= 80) # takes in only juvs that were 60-80days old 

# cleaning up trial dates that occur before end dates
master$age_last <- ifelse((master$age_last < master$age_trial),
                           master$age_trial,
                           master$age_last)

master <- master %>% 
  mutate(aug_census = make_date(year, 08, 15),
  age_census = as.integer(difftime(aug_census, birth_date, units = "days")),
  # were they alive on Aug 15 of their birth year?
  alive_aug = as.integer((end_date >= aug_census) & 
                           (age_census >= 60 | #were they at least 60 days then? 
                              age_last >= 60 | survived_60d == 1))) # if not, are they known to have lived more than 60 days?
# NA for those with end date after census but age unknown (ie. can't tell if
# they're >= 60 days old on census day)

## FACTORING CATEGORICAL VARIABLES ####
## adding in new categorical variables
# this variable looks at the specific playback
master$gridtreat <- factor(master$grid,
                           levels = c("JO", "RR", "BT", "KL", "SU", "SUX"),
                           labels = c("rattle", "rattle", "chickadee",
                                      "control", "control", "control"))

# this variable considers chickadee as part of the controls
master$treatment <- factor(master$grid,
                           levels = c("JO", "RR", "BT", "KL", "SU", "SUX"),
                           labels = c("rattle", "rattle", "control",
                                      "control", "control", "control"))

# this variable indicates whether a juvenile was born during a mast year
master$mastyear <- as.factor(as.integer(master$year == 2005 |
                                          master$year == 2019))

## this factors all other categorical variables already in the master table
master <- merge(master, grids_density,
                by = c("grid", "year"),
                all.x = TRUE) %>%
  mutate(grid = as.factor(grid),
         litter_id = as.factor(litter_id),
         mom_id = as.factor(mom_id),
         year = as.factor(year),
         juv_id = as.factor(juv_id),
         sex = as.factor(sex),
         m_return = as.factor(m_return),
         m_move = as.factor(m_move),
         alive_aug = as.factor(alive_aug),
         mastyear = as.factor(mastyear),
         gridyear = as.factor(paste(grid, year))) %>%
  distinct(juv_id,
           .keep_all = TRUE)

# filters out 2018-2021 for analyses involving playback treatments
recent4 <- master %>%
  filter(year %in% c(2018, 2019, 2020, 2021))

## STANDARDIZES CONTINUOUS VARIABLES ####
## the scale at which each variable was standardized by is next to its line of 
## code
# standardized to mean = 0, sd = 1 within each scale of standardizing 
master <- master %>%
  mutate(oft1 = scale_by(oft1 ~ gridyear), #grid-year
         mis1 = scale_by(mis1 ~ gridyear), #grid-year
         growthrate = scale_by(growthrate ~ gridyear), #grid-year
         spr_density = scale(spr_density), #globally
         return_lat = scale(return_lat), #globally
         move_lat = scale(move_lat), #globally
         n_pups = scale_by(n_pups ~ gridyear), #grid-year
         age_trial = scale(age_trial)) #globally

recent4 <- recent4 %>%
  mutate(oft1 = scale_by(oft1 ~ gridyear), #grid-year
         mis1 = scale_by(mis1 ~ gridyear), #grid-year
         growthrate = scale_by(growthrate ~ gridyear), #grid-year
         spr_density = scale(spr_density), #globally
         return_lat = scale(return_lat), #globally
         move_lat = scale(move_lat), #globally
         n_pups = scale_by(n_pups ~ gridyear), #grid-year
         age_trial = scale(age_trial)) #globally
