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

# STEP 1 ####
## is personality influenced by anything other than maternal care?

# model to see if BT is any diff from the other controls
oft1_controls <- lmer(oft1 ~ grid + (1 | litter_id),
                      data = master,
                      subset = !grid == "JO") 
summary(oft1_controls) #no effect


mis1_controls <- lmer(mis1 ~ grid + (1 | litter_id),
                      data = master,
                      subset = !grid == "JO")
summary(mis1_controls) #no effect

# ok, now to see what impacts personality
oft1_analysis <- lmer(oft1 ~ age_trial + n_pups + treatment + (1 | litter_id),
                      data = master)
summary(oft1_analysis) #no effects

mis1_analysis <- lmer(mis1 ~ sex + age_trial + n_pups + treatment + (1 | litter_id),
                      data = master)
summary(mis1_analysis) #no effects

## STEP 2 ####
## how does treatment influence maternal care?
return_analysis <- lmer(t_return ~ treatment + n_pups + (1 | grid),
                        data = master)
summary(return_analysis) #treatment has a sig effect on t_return

move_analysis <- lmer(t_move ~ treatment + n_pups + (1 | grid),
                      data = master)
summary(move_analysis) #treatment has a sig effect on t_move

## STEP 3 ####
## how does maternal care influence personality?

oft1_return <- lmer(oft1 ~ sex + age_trial + treatment + t_return + (1 | litter_id),
                    data = master)
summary(oft1_return)

mis1_return <- lmer(mis1 ~ sex + age_trial + treatment + t_return + (1 | litter_id),
                    data = master)
summary(mis1_return)

oft1_move<- lmer(oft1 ~ sex + age_trial + treatment + t_move + (1 | litter_id),
                 data = master)
summary(oft1_move)

mis1_move <- lmer(mis1 ~ sex + age_trial + treatment + t_move + (1 | litter_id),
                  data = master)
summary(mis1_move) #latency to move pups has a significant effect on aggression

## STEP 4 ####
## how about survival?

oft1_survival <- lmer(age_last ~ oft1 + sex + treatment + n_pups + (1 | litter_id),
                      data = master)
summary(oft1_survival)

mis1_survival <- lmer(age_last~ mis1 + sex + treatment + n_pups + (1 | litter_id),
                      data = master)
summary(mis1_survival)
