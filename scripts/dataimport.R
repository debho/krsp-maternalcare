##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff

##############################################################################
### Script for importing data

#reading in personality data
personality_raw <- read.csv('data/personality-master.csv',
                            header = T,
                            na.strings = c("", " ", "NA"))

#reading in nest attendance data
                            