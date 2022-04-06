##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for consolidating data

#what do i need to do to combine maternal care data with juves?
#1. take juvs and litters table and join on litter_id
#2. join that on squirrel_id but use the one in juvs because the one in litter is mom
#3. join it with nest_att on litter_id
colnames(litters)[1] <- "litter_id"

juvs_litters <- merge(juveniles,
                      litters,
                      by = "litter_id",
                      all = TRUE) %>%
  select(litter_id,
         squirrel_id.x,
         sex,
         squirrel_id.y,
         grid,
         yr,
         fieldBDate) %>%
  drop_na("squirrel_id.x")


colnames(juvs_litters)[2] <- "squirrel_id"
colnames(juvs_litters)[4] <- "mom_id"
colnames(personality)[1] <- "squirrel_id"

juvs_personality <- merge(juvs_litters,
                          personality,
                          by = "squirrel_id",
                          all = TRUE) %>%
  select(litter_id,
         squirrel_id,
         fieldBDate,
         taglft,
         tagrt,
         colours,
         sex.x,
         mom_id,
         grid.x,
         yr,
         oft1,
         mis1) %>%
  drop_na(oft1,
          mis1)

colnames(nest_att)[4] <- "mom_id"

master_table <- merge(juvs_personality,
                      nest_att,
                      by = "litter_id",
                      all = TRUE) %>%
  filter(yr > 2017) %>%
  drop_na(bark) #gives all squirrels that have both personality and nest att data


