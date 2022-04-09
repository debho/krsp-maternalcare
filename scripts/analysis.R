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

survival_pers <- lmer(survived_200d ~ (oft1 + mis1) + (1|litter_id),
                      data = master)
survival_oft1 <- lmer(survived_200d ~ oft1 + (1|litter_id),
                      data = master)
survival_mis1 <- lmer(survived_200d ~ mis1 + (1|litter_id),
                      data = master)

summary(survival_pers)
summary(survival_oft1)
summary(survival_mis1)

##STEP 2: personality ~ grid * year

pers_model <- lmer(oft1 + mis1 ~ sex + treatment + age + (1|litter_id),
                      data = master)
oft1_model <- lmer(oft1 ~ sex + treatment + age + (1|litter_id),
                   data = master)
mis1_model <- lmer(mis1~sex+treatment+age+(1|litter_id),
                   data = master)
summary(pers_model) #sig effect of sex
summary(oft1_model) #sig effect of sex, males less active
summary(mis1_model) #no sig effects

##STEP 3: personality ~ maternal care

maternal_pers <- lmer(oft1 + mis1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
maternal_oft1 <- lmer(oft1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
maternal_mis1 <- lmer(mis1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
summary(maternal_pers) #no sig effects
summary(maternal_oft1) #no sig effects
summary(maternal_mis1) #no sig effects

#i dont even know what these show rip
#maybe i should watch the MLM lectures  

##STEP 4: mis1 ~ LSR


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

