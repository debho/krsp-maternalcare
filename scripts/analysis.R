##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for running analyses

library(lme4) #for linear models

##STEP 1: survival ~ personality * density


##STEP 2: personality ~ grid * year

personality_analysis <- lmer(oft1 ~ sex.x + grid.x + age + (1|litter_id),
                             data = master_table)
#personality <- lmer(oft1 ~ sex + grid + age 
#                     + (1/litter_id)) #random effect

##STEP 3: personality ~ maternal care


##STEP 4: aggression ~ LSR

