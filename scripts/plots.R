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
library(ggResidpanel)
library(sjPlot)
library(sjmisc)
library(lattice)

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
         personality_resid = resid(personality_survival))

ggplot(survivalres, aes(oft1 * spr_density, oft_resid)) +
  geom_point(aes(colour = mastyear)) + 
  geom_smooth(aes(color = mastyear),  method = "lm", se = F) +
  labs(x = "Interaction between activity and grid density",
       y = "Survival to 200 days",
       title = "Relationship between activity, grid density, and survival")

ggplot(survivalres, aes(mis1 * spr_density, mis_resid)) +
  geom_point(aes(colour = mastyear)) + 
  geom_smooth(aes(color = mastyear),  method = "lm", se = F) +
  labs(x = "Interaction between aggression and grid density",
       y = "Survival to 200 days",
       title = "Relationship between aggression, grid density, and survival")

ggplot(survivalres, aes((oft1 + mis1) * spr_density, personality_resid)) +
  geom_point(aes(colour = mastyear)) + 
  geom_smooth(aes(oft1 * spr_density, oft_resid, color = mastyear),  method = "lm", se = F) + 
  geom_smooth(aes(mis1 * spr_density, mis_resid, color = mastyear),  method = "lm", se = F) +
  labs(x = "Interaction between activity, aggression and grid density",
       y = "Survival to 200 days",
       title = "Relationship between personality, grid density, and survival")

# plots density by year
ggplot(grids_density %>%
         filter(year > 2004), aes(year, spr_density)) + 
  geom_point(aes(color = year)) + 
  geom_line() +
  facet_wrap(~ grid)
