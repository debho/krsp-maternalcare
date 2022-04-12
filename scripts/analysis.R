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
oft1_controls <- lmer(oft1 ~ gridtreat + (1 | litter_id),
                      data = master,
                      subset = !grid == "JO") 
summary(oft1_controls) #no effect


mis1_controls <- lmer(mis1 ~ gridtreat + (1 | litter_id),
                      data = master,
                      subset = !grid == "JO")
summary(mis1_controls) #no effect

# ok, now to see what impacts personality
oft1_analysis <- lmer(oft1 ~ sex + age_trial + treatment + (1 | litter_id),
                      data = master)
summary(oft1_analysis) #no effects

mis1_analysis <- lmer(mis1 ~ sex + age_trial + treatment + (1 | litter_id),
                      data = master)
summary(mis1_analysis) #no effects

## STEP 2 ####
## how does treatment influence maternal care?

return_move <- lm(t_move ~ t_return,
                  data = master,
                  subset = (m_return == "y" & m_move == "y"))
summary(return_move) #time to return has a sig effect on time to move

return_analysis <- lm(t_return ~ treatment + n_pups,
                        data = master)
summary(return_analysis) #treatment has a sig effect on t_return

move_analysis <- lm(t_move ~ treatment + n_pups,
                    data = master)
summary(move_analysis) #treatment has a sig effect on t_move

## STEP 3 ####
## how does maternal care influence personality?

oft1_return <- lmer(oft1 ~ t_return + sex + age_trial + treatment + (1 | litter_id),
                    data = master)
summary(oft1_return)

mis1_return <- lmer(mis1 ~ t_return + sex + age_trial + treatment + (1 | litter_id),
                    data = master)
summary(mis1_return)

oft1_move <- lmer(oft1 ~ t_move + sex + age_trial + treatment + (1 | litter_id),
                 data = master)
summary(oft1_move)

mis1_move <- lmer(mis1 ~ t_move + sex + age_trial + treatment + (1 | litter_id),
                  data = master)
summary(mis1_move)

## STEP 4 ####
## survival ~ personality

oft1_survival <- lmer(survived_200d ~ oft1 + (1 | grid),
                      data = master,
                      subset = !year == 2021)
summary(oft1_survival)

mis1_survival <- lmer(survived_200d ~ mis1 + (1 | grid),
                      data = master,
                      subset = !year == 2021)
summary(mis1_survival)

#wait idk if this survival code is even right
#is my survival measure even right

## STEP 5 ####
## aggression ~ LSR

mis_LSR <- lmer(mis1 ~ LSR + t_return + t_move + (1 | litter_id),
                data = master,
                subset = !is.na(LSR))
summary(mis_LSR)        
