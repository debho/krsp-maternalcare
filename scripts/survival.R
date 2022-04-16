##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for survival data

survival <- flastall %>%
  filter(byear < 2021) %>%
  mutate(age_last = as.integer(difftime(datee, dates, units = "days")),
         survived_200d = age_last >= 200) %>%
  transmute(juv_id = squirrel_id,
            start_date = dates,
            end_date = datee,
            last_fate = f2,
            age_last,
            survived_200d,
            byear)

#if end date is earlier than late-sept/early oct of 2021 it means that squirrel
#is presumed to be dead

survival$survived_200d <- as.numeric(survival$survived_200d)

