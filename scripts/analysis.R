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
                      subset = !gridtreat == 2) #n = 63
summary(oft_controls) #no effect


mis_controls <- lmer(mis1 ~ gridtreat + (1 | litter_id),
                      data = recent4,
                      subset = !gridtreat == 2) #n = 63
summary(mis_controls) #no effect

## ANALYSIS #1 ####
# SURVIVAL ~ PERSONALITY + DENSITY + (1 | LITTER_ID)
# only uses JO KL SU (no densities for BT RR SUX)
# all years except 2021
oft_survival <- glmer(survived_200d ~ (oft1 * spr_density) + (1 | litter_id),
                      data = master,
                      family = "binomial")
vif(oft_survival)
summary(oft_survival) #n = 206

mis_survival <- glmer(survived_200d ~ (mis1 * spr_density) + (1 | litter_id),
                      data = master,
                      family = "binomial")
vif(mis_survival)
summary(mis_survival) #n = 206

personality_survival <- glmer(survived_200d ~ (oft1 + mis1) * spr_density + (1 | litter_id),
                              data = master,
                              family = "binomial")
vif(personality_survival)
summary(personality_survival) #n = 206

## ANALYSIS #2 ####
# PERSONALITY ~ TREATMENT

oft_predictors <- lmer(oft1 ~ (treatment * sex) + growthrate + (1 | litter_id),
                       data = recent4)
vif(oft_predictors)
summary(oft_predictors) #n = 101

mis_predictors <- lmer(mis1 ~ (treatment * sex) + growthrate  + (1 | litter_id),
                       data = recent4)
vif(mis_predictors)
summary(mis_predictors) #n = 101

## ANALYSIS #3 ####
# PERSONALITY ~ MATERNALCARE + SEX + (1 | YEAR) + (1 + LITTER_ID)

oft_care <- lmer(oft1 ~ (m_return + m_move) + sex + (1 | year) + (1 | litter_id),
                 data = master)
vif(oft_care)
summary(oft_care) #n = 104

mis_care <- lmer(mis1 ~ (m_return + m_move) + sex + (1 | year) + (1 | litter_id),
                 data = master)
vif(mis_care)
summary(mis_care) #n = 104

personality_care <- lmer(oft1 + mis1 ~ (m_return + m_move) + sex + (1 | year) + (1 | litter_id),
                         data = master)
vif(personality_care)
summary(personality_care) #n = 104

## ANALYSIS #4 ####
# MATERNAL CARE ~ TREATMENT

#how soon after returning did moms move their pups?
return_move <- lm(move_lat ~ return_lat,
                  data = master)
summary(return_move)

return_treatment <- lmer(return_lat ~ treatment + n_pups + (1 | year),
                         data = recent4)
summary(return_treatment)

move_treatment <- lmer(move_lat ~ treatment + n_pups + (1 | year),
                       data = recent4)
summary(move_treatment)

care_treatment <- lmer(return_lat + move_lat ~ treatment + n_pups + (1 | year),
                     data = recent4)
summary(care_treatment)

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
#oft1 ~ (treatment * sex) + growthrate + year (factor) + (1 | litter_id)
#oft and mis in separate models and use just those 4 years

#step 3 (all years)
#of1/mis1 ~ latency to retireve (or binary response) + sex + (1 | year) + (1 | litter_id)

#step 4 (all years)
#effects of treatment on latency to return/move
#argue that under high density, high growth rate is favored and attentiveness increases growth rate
#RR is rattle and SUX is control
#ignore age for this thesis but report that about 50 didnt have an age (couldnt be confirmed at this tiem) and its like 20% of all trials





