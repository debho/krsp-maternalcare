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
         dates,
         datee,
         age_last,
         survived_200d) %>%
  mutate(survived_200d = as.integer(survived_200d))


flastall_juv <- tbl(con, "flastall2") %>% 
  select(squirrel_id, date_end = datee, fate_end = f2) %>% 
  collect()
