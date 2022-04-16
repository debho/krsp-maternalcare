##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for handling growth rate

nest1 <- nest_attALL %>%
  filter(nest == 1) %>%
  mutate(date1 = date,
         julian_date1 = julian_date)

nest2 <- nest_attALL %>%
  filter(nest == 2) %>%
  mutate(date2 = date,
         julian_date2 = julian_date)

nest12 <- merge(nest1,
                nest2,
                by = "litter_id",
                all = TRUE)

juvs <- juveniles %>%
  filter(juv_id %in% master$juv_id |
         litter_id %in% master$litter_id)

growthrate <- merge(nest12,
                    juvs,
                    by = "litter_id",
                    all = TRUE) %>%
  drop_na(date1,
          date2,
          weight,
          tagWT)

growthrate <- growthrate %>%
  mutate(date1 )
