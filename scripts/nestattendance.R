##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff

##############################################################################
### Script for preparing nest attendance data for analysis

#reading in nest attendance data and cleans it
nest_att <- read.csv("data/AllNests.csv",
                     header = TRUE,
                     sep = ",") %>%
  filter(year > 2017) %>% #to take only 2018-2021
  drop_na(charges,
          mock_falls,
          rattles) #removes any rows where values weren't specified

