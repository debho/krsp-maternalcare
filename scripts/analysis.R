##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for running analyses

library(lmerTest) #for linear models
library(sjPlot) #for visualizing

##STEP 1: survival ~ personality * density


##STEP 2: personality ~ grid * year

pers_analysis <- lmer(oft1 + mis1 ~ sex + treatment + age + (1|litter_id),
                      data = master)
summary(pers_analysis)

##STEP 3: personality ~ maternal care

maternal_pers <- lmer(oft1 + mis1 ~ t_return + t_move * treatment + (1|litter_id),
                      data = master)
maternal_pers_int <- lmer(oft1 * mis1 ~ t_return + t_move * treatment + (1|litter_id),
                      data = master)
summary(maternal_pers)
summary(maternal_pers_int)
##STEP 4: aggression ~ LSR


##RUNNING RANDOM CORRELATION TESTS
cor.test(master$oft1 + master$mis1,
         master$t_return)
cor.test(master$oft1 + master$mis1,
         master$t_move)
cor.test(master$oft1 + master$mis1,
         master$treatment)
cor.test(master$oft1 * master$mis1,
         master$t_return)
cor.test(master$oft1 * master$mis1,
         master$t_move)
cor.test(master$oft1 * master$mis1,
         master$treatment)
cor.test(master$t_return + master$t_move,
         master$treatment)
cor.test(master$t_return * master$t_move,
         master$treatment)
cor.test(master$oft1 * master$mis1,
         master$t_return + master$t_move)

