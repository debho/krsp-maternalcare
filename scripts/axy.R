##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for handling axy data

sq_11256 <- read_csv("data/axy/11256_006_20150424.csv") %>%
  filter(nest == "Nest",
         asleep == 0)
# ok i definitely need a more efficient way to do this...
