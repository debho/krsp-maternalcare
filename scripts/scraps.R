##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Random script scraps


#PREV ANALYSIS STUFF
# ok, now to see what impacts personality
oft1_analysis <- lmer(mis1 ~ sex + treatment + (1 | litter_id) (1 | year),
                      data = recent4)
summary(oft1_analysis) #effect of treatment

mis1_analysis <- lmer(mis1 ~ sex + age_trial + treatment + (1 | litter_id),
                      data = master)
summary(mis1_analysis) #effect of age at trial time

## STEP 2 ####
## how does treatment influence maternal care?

return_move <- lm(t_move ~ t_return,
                  data = recent4,
                  subset = (m_return == "y" & m_move == "y"))
summary(return_move) #time to return has a sig effect on time to move

return_analysis <- lm(t_return ~ treatment + n_pups,
                      data = recent4)
summary(return_analysis) #treatment has a sig effect on t_return
#boxplot this

move_analysis <- lm(t_move ~ treatment + n_pups,
                    data = recent4)
summary(move_analysis) #treatment has a sig effect on t_move
#boxplot this


## STEP 3 ####
## how does maternal care influence personality?
# check out diagnostics for lmer packages (lmer model diagnostics)

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

oft1_care <- lmer(oft1 ~ t_return + t_move + sex + age_trial + treatment + (1 | litter_id),
                  data = master)
summary(oft1_care)

mis1_care <- lmer(mis1 ~ t_return + t_move + sex + age_trial + treatment + (1 | litter_id),
                  data = master)
summary(mis1_care)

## STEP 4 ####
## survival ~ personality

oft1_survival <- glmer(survived_200d ~ oft1 * year + sex + (1 | litter_id),
                       data = master,
                       family = binomial)
summary(oft1_survival) #n = 91

mis1_survival <- glm(survived_200d ~ mis1 + year,
                     data = recent4,
                     family = binomial)
summary(mis1_survival) #n = 91

personality_survival <- glm(survived_200d ~ oft1 + mis1 + year,
                            data = recent4,
                            family = binomial)
summary(personality_survival) #n = 86
# FOR THESE 3 MODELS, AIC WAS LOWEST WHEN I USED + INSTEAD OF INTERACTIONS AND WHEN I REMOVED SEX

oft1_survivalNOJO <- glm(survived_200d ~ oft1 * year,
                         data = recent4,
                         family = binomial,
                         subset = !grid == "JO")
summary(oft1_survivalNOJO) #n = 45

mis1_survivalNOJO <- glm(survived_200d ~ mis1 * year,
                         data = recent4,
                         subset = !grid == "JO")
summary(mis1_survivalNOJO) #n = 45

personality_survivalNOJO <- lmer(survived_200d ~ oft1 * mis1 * year * sex + (1 | litter_id),
                                 data = recent4 %>%
                                   filter(!grid == "JO"))
summary(personality_survivalNOJO) #n = 42


