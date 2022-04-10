##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for survival data

survival <- flastall %>%
  filter(byear > 2017,
         bcert == "Y") %>%
  mutate(age_last = as.integer(difftime(datee, dates, units = "days")),
         survived_200d = age_last >= 200) %>%
  select(juv_id = squirrel_id,
         start_date = dates,
         end_date = datee,
         last_fate = f2,
         age_last,
         survived_200d,
         byear)

survival[!survival$last_fate %in% c(4, 5, 10, 11, 12) & survival$byear == 2021,
         "survived_200d"] <- 1

survival$survived_200d <- as.integer(survival$survived_200d)

