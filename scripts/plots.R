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
         filter(!gridtreat == "rattle"),
       aes(gridtreat, oft1, col = gridtreat)) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot(position = "dodge") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_colour_viridis_d() #put in p-val

ggplot(recent4 %>%
         filter(!gridtreat == "rattle"),
       aes(gridtreat, mis1, col = gridtreat)) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot(position = "dodge") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_colour_viridis_d() #put in p-val


## ANALYSIS #1 PLOTS ####
## SURVIVAL DATA
# plots of glmer models
plot(oft_survival)
plot(mis_survival)
plot(personality_survival)

survivalres <- masterSCALED %>%
  filter(!is.na(alive_aug),
         !is.na(spr_density),
         !is.na(litter_id)) %>%
  mutate(oft_pred = scale(predict(oft_survival)),
         oft_resid = scale(resid(oft_survival)),
         mis_pred = scale(predict(mis_survival)),
         mis_resid = scale(resid(mis_survival)),
         personality_pred = scale(predict(personality_survival)),
         personality_resid = scale(resid(personality_survival))) #all standardized residuals are here

ggplot(survivalres, aes(spr_density, alive_aug, col = spr_density)) + #replace year with density, plot residuals esp if results are sig
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  stat_smooth(aes(oft1, alive_aug, color = oft_resid),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) + 
  scale_color_viridis_c()


ggplot(survivalres, aes(spr_density, alive_aug, col = spr_density)) +
  geom_point(size = 3,
             alpha = 0.8) +
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) + 
  stat_smooth(aes(mis1, alive_aug, color = spr_density),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) + 
  scale_color_viridis_c()
