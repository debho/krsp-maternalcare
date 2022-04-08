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

colnames(litters)[1] <- "litter_id" #so that there's a common column
colnames(litters)[25] <- "mom_id" #avoid confusion with squirrel_id
colnames(juveniles)[17] <- "juv_id" #avoid confusion with squirrel_id

juv_to_litter <- merge(juveniles,
                       litters,
                       by = "litter_id",
                       all.x = TRUE)

# STEP 2 ####
## we need to find out who the moms are
## match nest attendance to that table on moms

colnames(nest_att)[4] <- "mom_id"

juvlitter_mom <- merge(juv_to_litter,
                       nest_att,
                       by = "mom_id",
                       all.x = TRUE)

# STEP 3 ####
## match these to personality
## match on juv_id

colnames(personality)[1] <- "juv_id"

master <- merge(juvlitter_mom,
                personality,
                by = "juv_id", 
                all.x = TRUE) %>% #now we clean
  filter(yr > 2017,
         grid.x %in% c("BT", "JO", "KL", "SU")) %>% #gets only 2018-2021 data
  drop_na(oft1, #only those with personality data
          mis1,
          bark) %>% #only those with nest attendance data
  select(juv_id,
         litter_id.x,
         mom_id,
         sex.x,
         grid.x,
         age.x,
         t_return,
         t_move,
         oft1,
         mis1,
         yr,
         n_pups) #consolidates table

# STEP 4 ####
## clean up column names

colnames(master)[2] <- "litter_id"
colnames(master)[4] <- "sex"
colnames(master)[5] <- "grid"
colnames(master)[6] <- "age"

# STEP 5 ####
## combine all except for JO into control
master$treatment[master$grid == "JO"] <- 1
master$treatment[master$grid %in% c("BT", "KL", "SU")] <- 0

# STEP 6 ####
## obtain LSR, using F:M
## group by litter
## no. of females/n_pups

nests <- master %>%
  group_by(litter_id,
           sex) %>%
  summarise(n_females = n()) #hmmm i think i need durther wrangling before i can do this


