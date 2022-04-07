##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for running analyses

library(lmerTest) #for linear models
library(psycho)
##STEP 1: survival ~ personality * density


##STEP 2: personality ~ grid * year

oft1_analysis <- lmer(oft1 ~ sex.x + grid.x + age + (1|litter_id),
                      data = master_table)
mis1_analysis <- lmer(mis1 ~ sex.x + grid.x + age + (1|litter_id),
                      data = master_table)

#personality <- lmer(oft1 ~ sex + grid + age 
#                     + (1/litter_id)) #random effect

##STEP 3: personality ~ maternal care



##STEP 4: aggression ~ LSR

