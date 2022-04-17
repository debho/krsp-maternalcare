##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for running analyses

library(lmerTest) #for linear mixed models
library(QuantPsyc)

## CONTROLS ####
# model to see if BT is any diff from the other controls
oft_controls <- lmer(oft1 ~ gridtreat + (1 | litter_id),
                      data = recent4,
                      subset = !gridtreat == "rattle") #n = 37
summary(oft_controls) #no effect


mis_controls <- lmer(mis1 ~ gridtreat + (1 | litter_id),
                      data = recent4,
                      subset = !gridtreat == "rattle") #n = 37
summary(mis_controls) #no effect

## ANALYSIS #1 ####
# SURVIVAL ~ PERSONALITY + DENSITY + (1 | LITTER_ID)
# only uses JO KL SU (no densities for BT RR SUX)
# all years except 2021
oft_survival <- glmer(alive_aug ~ (oft1 * spr_density) +
                        (1 | litter_id) + (1 | gridyear),
                      data = master,
                      family = "binomial")
vif(oft_survival)
summary(oft_survival) #n = 120, spr_density has an effect

mis_survival <- glmer(alive_aug ~ (mis1 * spr_density) +
                        (1 | litter_id) + (1 | gridyear),
                      data = master,
                      family = "binomial")
vif(mis_survival)
summary(mis_survival) #n = 120

personality_survival <- glmer(alive_aug ~ (oft1 + mis1) * spr_density +
                                (1 | litter_id) + (1 | gridyear),
                              data = master,
                              family = "binomial")
vif(personality_survival)
summary(personality_survival) #n = 120

## ANALYSIS #2 ####
# PERSONALITY ~ TREATMENT

oft_predictors <- lmer(oft1 ~ (treatment * sex) + growthrate + year + (1 | litter_id),
                       data = recent4)
vif(oft_predictors)
summary(oft_predictors) #n = 61

mis_predictors <- lmer(mis1 ~ (treatment * sex) + growthrate + year + (1 | litter_id),
                       data = recent4)
vif(mis_predictors)
summary(mis_predictors) #n = 61

personality_predictors <- lmer(oft1 * mis1 ~ (treatment * sex) + year + (1 | litter_id),
                               data = recent4)
vif(personality_predictors)
summary(personality_pred)

## ANALYSIS #3 ####
# PERSONALITY ~ MATERNALCARE + SEX + (1 | YEAR) + (1 + LITTER_ID)

oft_care <- lmer(oft1 ~ return_lat + move_lat + sex + (1 | year) + (1 | litter_id),
                 data = master)
vif(oft_care)
summary(oft_care) #n = 67

mis_care <- lmer(mis1 ~ return_lat + move_lat + sex + (1 | year) + (1 | litter_id),
                 data = master)
vif(mis_care)
summary(mis_care) #n = 67

personality_care <- lmer(oft1 + mis1 ~ return_lat + move_lat + sex + (1 | year) + (1 | litter_id),
                         data = master)
vif(personality_care)
summary(personality_care) #n = 67

## ANALYSIS #4 ####
# MATERNAL CARE ~ TREATMENT

#how soon after returning did moms move their pups?
return_move <- lm(move_lat ~ return_lat,
                  data = master)
summary(return_move) #n = 67

return_treatment <- lmer(return_lat ~ treatment + n_pups + (1 | year),
                         data = recent4)
summary(return_treatment) #n = 33

move_treatment <- lmer(move_lat ~ treatment  + n_pups + (1 | year),
                       data = recent4)
summary(move_treatment) #n = 33

care_treatment <- lmer(return_lat * move_lat ~ treatment  + n_pups + (1 | year),
                     data = recent4)
summary(care_treatment) #n = 33

# for each table include sample size for both trials and individuals
# always figure out where i'm losing trials

# make a table with year, no. trials/juv, grid
# DO SURVIVAL ANALYSIS FIRST

# APRIL 14 2022

#for personality ~ treatment use only the 4 years
#use all data for survival data
# step  1 (all data)
#survived_200d ~ (oft1 * spring grid density) + (mis1 * spring grid density) + mastyear(y/n) + (! | litter_id)

# step 2: grid effects/treatment effects focus on 2018-2021
#oft1 ~ (treatment * sex) + growth rate + year (factor) + (1 | litter_id)
#oft and mis in separate models and use just those 4 years

#step 3 (all years)
#of1/mis1 ~ latency to retrieve (or binary response) + sex + (1 | year) + (1 | litter_id)

#step 4 (all years)
#effects of treatment on latency to return/move
#should i add in spring density???
#argue that under high density, high growth rate is favored and attentiveness increases growth rate
#RR is rattle and SUX is control
#ignore age for this thesis but report that about 50 didnt have an age (couldnt be confirmed at this tiem) and its like 20% of all trials





