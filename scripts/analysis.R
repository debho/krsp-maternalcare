##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for running analyses

library(lmerTest) #for linear models

##STEP 1: survival ~ personality * density


##STEP 2: personality ~ grid * year

oft1_analysis <- lmer(oft1 ~ sex * treatment * age * (1|litter_id),
                      data = master)
mis1_analysis <- lmer(mis1 ~ sex * treatment * age * (1|litter_id),
                      data = master)

summary(oft1_analysis)
summary(mis1_analysis)


##STEP 3: personality ~ maternal care

cor.test(master$oft1,
         master$t_return)
cor.test(master$oft1,
         master$t_move)
cor.test(master$oft1,
         master$treatment)
cor.test(master$mis1,
         master$t_return)
cor.test(master$mis1,
         master$t_move)
cor.test(master$mis1,
         master$treatment)

##STEP 4: aggression ~ LSR

