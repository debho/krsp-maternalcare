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

#OLD PLOTS DATA



# plotting oft1 ~ mis1 correlation by grid
ggplot(master, aes(oft1, mis1, col = grid)) +
  geom_point() +
  scale_color_paletteer_d("vapeplot::sunset") + 
  labs(y = "Aggression", x = "Activity") +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", se = T, aes(group = 1))

ggplot(master %>%
         filter(!gridtreat == 2), aes(gridtreat, oft1)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::sunset") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of control on activity") + 
  labs(y = "Activity",
       x = "Type of control")

ggplot(master %>%
         filter(!gridtreat == 2), aes(gridtreat, mis1)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::jazzcup") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of control on aggression") + 
  labs(y = "Aggression",
       x = "Type of control")


ggplot(master, aes(treatment, mis1)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::jazzcup") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on aggression")

# boxplots treatment 0 vs 1
ggplot(master, aes(oft1, treatment, col = grid)) +
  geom_point() +
  scale_color_paletteer_d("vapeplot::sunset") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on activity")

ggplot(master, aes(treatment, mis1, col = treatment)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::jazzcup") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on aggression")

ggplot(master, aes(treatment, oft1 + mis1, col = treatment)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::seapunk") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on personality")


# return ~ move
ggplot(master %>%
         filter(m_return == "y"), aes(t_return, t_move, col = treatment)) +
  geom_point(size = 3) + 
  labs(y = "Latency to move pups", x = "Latency to return") +
  scale_color_paletteer_d("vapeplot::cool") +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", se = F, aes(group = 1))

#return ~ treatment
ggplot(recent4, aes(grid, t_return, col = treatment)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::seapunk") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on latency to return")

ggplot(recent4, aes(grid, t_move, col = treatment)) +
  geom_boxplot() +
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  scale_color_paletteer_d("vapeplot::seapunk") + 
  ggtitle("Effects of treatment on latency to move")


# survival ~ activity + year
ggplot(master %>%
         filter(!year == 2021,
                !is.na(survived_200d)),
       aes(oft1,
           survived_200d,
           col = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("vapeplot::vaporwave") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  labs(y = "Probability of surviving at least 200 days",
       x = "Activity")

# survival ~ aggression + year
ggplot(master %>%
         filter(!year == 2021),
       aes(mis1,
           survived_200d,
           col = spr_density)) + #replace year with density, plot residuals esp if results are sig
  geom_point(size = 3,
             alpha = 0.5) +
  scale_color_paletteer_c("viridis::viridis") + 
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  labs(y = "Probability of surviving at least 200 days",
       x = "Aggression")

# survival ~ activity + aggression + year
ggplot(recent4 %>%
         filter(!year == 2021),
       aes(oft1 + mis1,
           survived_200d,
           col = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_color_paletteer_d("vapeplot::vaporwave") + 
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  labs(y = "Probability of surviving at least 200 days",
       x = "Activity + Aggression")

#maternal care and personality


# for each table include sample size for both trials and individuals
# always figure out where i'm losing trials

# make a table with year, no. trials/juv, grid
# DO SURVIVAL ANALYSIS FIRST

# APRIL 14 2022

#for personality ~ treatment use only the 4 years
#use all data for survival data
# step 1 (all data)
#survived_200d ~ (oft1 * spring grid density) + (mis1 * spring grid density) + (1 | litter_id)

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
