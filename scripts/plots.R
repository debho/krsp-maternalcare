##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for plots

library(paletteer) #for colors
library(ggplot2) #for figures and graphs
library(ggprism)
library(ggResidpanel)
library(sjPlot)
library(sjmisc)
library(lattice)

## EFFECTS OF CHICKADEE PLAYBACKS ####
ggplot(recent4 %>%
         filter(!gridtreat == 2),
       aes(gridtreat, oft1, col = gridtreat)) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot(position = "dodge") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_colour_viridis_d()

## ANALYSIS #1 PLOTS ####
## SURVIVAL DATA
# plots of glmer models
plot(oft_survival)
plot(mis_survival)
plot(personality_survival)

survivalres <- master %>%
  filter(!is.na(survived_200d),
         !is.na(spr_density),
         !is.na(litter_id)) %>%
  mutate(oft_pred = predict(oft_survival),
         oft_resid = resid(oft_survival),
         mis_pred = predict(mis_survival),
         mis_resid = resid(mis_survival),
         personality_pred = predict(personality_survival),
         personality_resid = resid(personality_survival)) #all standardized residuals are here

ggplot(master %>%
         filter(!is.na(survived_200d),
                !is.na(spr_density)), aes(oft1, survived_200d, col = spr_density)) +
  geom_point(size = 3,
             alpha = 0.8) +
  stat_smooth(aes(oft1, survived_200d, color = spr_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) + 
  scale_color_viridis_c() +
  facet_wrap(~ spr_density)

ggplot(master %>%
         filter(!is.na(survived_200d),
                !is.na(spr_density)), aes(mis1, survived_200d, col = spr_density)) +
  geom_point(size = 3,
             alpha = 0.8) +
  stat_smooth(aes(mis1, survived_200d, color = spr_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) + 
  scale_color_viridis_c() +
  facet_wrap(~ spr_density)

