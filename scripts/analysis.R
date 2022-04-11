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

# STEP 1: survival ~ personality * density ####

survival_pers <- lmer(survived_200d ~ oft1 + mis1 + treatment + (1|litter_id),
                      data = master)
survival_oft1 <- lmer(survived_200d ~ oft1 + treatment + (1|litter_id),
                      data = master)
survival_mis1 <- lmer(survived_200d ~ mis1 + treatment + (1|litter_id),
                      data = master)

summary(survival_pers)
summary(survival_oft1)
summary(survival_mis1)

# STEP 2: personality ~ grid * year ####

# model to see if BT is any diff from the other controls
pers_controls <- lmer(oft1 ~ sex + age_trial + grid + (1 | litter_id),
                      data = master,
                      subset = !grid == "JO") 
summary(pers_controls)
pers_model <- lmer(oft1 + mis1 ~ sex + treatment + age_trial + (1|litter_id),
                      data = master)
oft1_model <- lmer(oft1 ~ sex + treatment + age_trial + (1|litter_id),
                   data = master)
mis1_model <- lmer(mis1 ~ sex + treatment + age_trial + (1|litter_id),
                   data = master)


summary(pers_model) 
summary(oft1_model)
summary(mis1_model)

# STEP 3: personality ~ maternal care ####

maternal_pers <- lmer(oft1 + mis1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
maternal_oft1 <- lmer(oft1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
maternal_mis1 <- lmer(mis1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
summary(maternal_pers)
summary(maternal_oft1) 
summary(maternal_mis1) 

##STEP 4: mis1 ~ LSR


