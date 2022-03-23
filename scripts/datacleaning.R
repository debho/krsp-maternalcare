##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff

##############################################################################
### Script for cleaning all data

##PERSONALITY
#new df with only 2018-2021
personality <- filter(personality_all,
                      cohort > 2017) 
personality_clean <- filter(personality,
                            Exclude_unless_video_reanalyzed == 'N')
describe(personality_clean) #errors in latencies, gotta check data

                      