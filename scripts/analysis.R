
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# EXPLANATION ####
# this script is where all (generalized) linear (mixed-effects) models were
# fitted
# see dataconsol.R to see how data was consolidated
# 'master' is used for any analyses that do NOT have rattle/chickadee/control
# playbacks as a fixed/random effect. this table is a consolidation of
# data from the KRSP database (juveniles, litter, and flastall2), personality
# data, and nest attendance data. n = 124 juveniles.
# 'recent4' is a subset of 'master' and this table is used for any analyses
# involving playback treatment as a fixed effect. this table contains data
# only for juveniles born in 2018, 2019, 2020, and 2021 as this
# form of density cue treatment only began in 2018. n = 65 juveniles.

## ANALYSIS #1 ####
# SURVIVAL ~ PERSONALITY + DENSITY + (1 | LITTER_ID)
# only uses JO KL SU (no densities for BT RR SUX)
# data: master
# n = 116

personality_survival <- glmer(alive_aug ~ (oft1 + mis1) * spr_density + 
                                (1 | litter_id), 
                              data = master,
                              family = "binomial")
vif(personality_survival) #all VIF < 2
summary(personality_survival) 
# significant effect of spring grid density, where increased density decreased
# probability of survival
# significant interaction between aggression and spring grid density, where 
# in high-density conditions, increased aggression was favored

                              
## ANALYSIS #2 ####
# PERSONALITY ~ TREATMENT
# data: recent4

## CONTROLS
# n = 37, BT: n = 6; KL/SU: n = 31
# is there an effect of chickadee playbacks on personality?
oft_controls <- lmer(oft1 ~ gridtreat +
                       (1 | litter_id),
                     data = recent4,
                     subset = !gridtreat == "rattle")
summary(oft_controls)
# no significant effect of chickadee playbacks on offspring activity

mis_controls <- lmer(mis1 ~ gridtreat +
                       (1 | litter_id),
                     data = recent4,
                     subset = !gridtreat == "rattle") #n = 37
summary(mis_controls)
# no significant effect of chickadee playbacks on offspring aggression

## TREATMENT
# n = 61 juveniles, no growth rate data for 4 juveniles 
oft_predictors <- lmer(oft1 ~ (treatment * sex) + growthrate + year +
                         (1 | litter_id) + (1 | gridyear),
                       data = recent4)
vif(oft_predictors) #all VIF < 3
summary(oft_predictors)
# no significant effect of any of the above on offspring activity

mis_predictors <- lmer(mis1 ~ (treatment * sex) + growthrate + year +
                         + (1 | litter_id) + (1 | gridyear),
                       data = recent4)
vif(mis_predictors) #all VIF <3 (same predictors as OFT analysis)
summary(mis_predictors)
# no significant effect of any of the above on offspring aggression

## ANALYSIS #3 ####
# PERSONALITY ~ MATERNALCARE + SEX + (1 | YEAR) + (1 + LITTER_ID)
# uses censored latencies, where non-returnees are set to 420 seconds
# data: master
# n = 67; only 67 juveniles had accompanying nest attendance data

oft_care <- lmer(oft1 ~ return_lat + move_lat +
                   (1 | year) + (1 | litter_id),
                 data = master)
vif(oft_care) #all VIF < 3
summary(oft_care)
# no significant effect of return/move latency on offspring activity

mis_care <- lmer(mis1 ~ return_lat + move_lat + 
                   (1 | year) + (1 | litter_id),
                 data = master)
vif(mis_care) #all VIF < 3
summary(mis_care)
# no significant effect of return/move latency on offspring aggression

## ANALYSIS #4 ####

# how soon after returning did moms move their pups?
# data: master
# n = 25
# uses uncensored latencies; NAs were removed but latencies > 420s were
# kept in analysis
return_move <- lm(t_move ~ t_return,
                  data = master)
summary(return_move)

# MATERNAL CARE ~ TREATMENT
# data: recent4
# uses censored latencies, where non-returnees are set to 420 seconds
# n = 33 observations from n = 21 moms

return_treatment <- lmer(return_lat ~ treatment + n_pups +
                           (1 | year),
                         data = recent4)
vif(return_treatment) #all VIF < 2
summary(return_treatment)
# significant effect of rattle playback on latency to return; decreased latency
# to return for rattle playback grid than control

move_treatment <- lmer(move_lat ~ treatment + n_pups +
                         (1 | year),
                       data = recent4)
vif(move_treatment) #all VIF < 2
summary(move_treatment)
# no significant effect of rattle playbacks on latency to move pups