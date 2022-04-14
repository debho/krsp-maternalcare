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
oft1_controls <- lmer(oft1 ~ gridtreat + (1 | litter_id),
                      data = master,
                      subset = !gridtreat == 2) #n = 191
summary(oft1_controls) #no effect


mis1_controls <- lmer(mis1 ~ gridtreat + (1 | litter_id),
                      data = master,
                      subset = !gridtreat == 2) #n = 191
summary(mis1_controls) #no effect

## ANALYSIS #1 ####
# SURVIVAL ~ PERSONALITY + OTHER EFFECTS
# still missing densities for BT, RR, SUX
oft_survival <- glmer(survived_200d ~ oft1 * spr_density + mastyear + (1 | litter_id),
                     data = master,
                     family = "binomial")
summary(oft_survival) #n = 207

mis_survival <- glmer(survived_200d ~ mis1 * spr_density + mastyear + (1 | litter_id),
                      data = master,
                      family = "binomial")
summary(mis_survival) #n = 207

personality_survival <- glmer(survived_200d ~ (oft1 + mis1) * spr_density + mastyear + (1 | litter_id),
                      data = master,
                      family = "binomial")
summary(personality_survival) #n = 207

## ANALYSIS #2 ####
# PERSONALITY ~ TREATMENT

oft_predictors <- lmer(oft1 ~ (treatment * sex) + (1 | yearF) + (1 | litter_id),
                       data = recent4)
summary(oft_predictors) #n = 113

mis_predictors <- lmer(mis1 ~ (treatment * sex) + (1 | yearF) + (1 | litter_id),
                       data = recent4)
summary(mis_predictors) #n = 113

## ANALYSIS #3 ####
# PERSONALITY ~ MATERNALCARE + SEX + (1 | YEAR) + (1 + LITTER_ID)

oft_care <- lmer(oft1 ~ (t_return + t_move) * treatment + (1 | yearF) + (1 | litter_id),
                 data = master)
summary(oft_care) #n = 104

mis_care <- lmer(mis1 ~ (t_return + t_move) * treatment + (1 | yearF) + (1 | litter_id),
                 data = master)
summary(mis_care) #n = 104

personality_care <- lmer(oft1 + mis1 ~ (t_return + t_move) * treatment + (1 | yearF) + (1 | litter_id),
                         data = master)
summary(personality_care) #n = 104

## ANALYSIS #4 ####
# MATERNAL CARE ~ TREATMENT


# for each table include sample size for both trials and individuals
# always figure out where i'm losing trials

# make a table with year, no. trials/juv, grid
# DO SURVIVAL ANALYSIS FIRST

# APRIL 14 2022

#for personality ~ treatment use only the 4 years
#use all data for survival data
# step  1 (all data)
#survived_200d ~ (oft1 * spring grid density) + (mis1 * spring grid density) + mastyear(y/n) + (! | litter_id)
#add mastyear column, no for all except 2005 and 2019
#add another column for grid density (look at andrew's code or ask lauren)

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





