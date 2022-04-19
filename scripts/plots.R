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

## EFFECTS OF PERSONALITY ON SURVIVAL ####
survivalres <- master %>%
  filter(!is.na(alive_aug),
         !is.na(spr_density),
         !is.na(litter_id)) %>%
  mutate(alive_aug = (as.numeric(alive_aug)-1),
         oft_pred = scale(predict(oft_survival)), #all standardized residuals are here
         oft_resid = scale(resid(oft_survival)),
         mis_pred = scale(predict(mis_survival)),
         mis_resid = scale(resid(mis_survival)),
         personality_pred = scale(predict(personality_survival)),
         personality_resid = scale(resid(personality_survival)),
         spr_density_binned = cut_interval(spr_density, n = 2)) %>%
  mutate(spr_density_binned = factor(spr_density_binned,
                                     labels = c("Low density", "High density")),
         spr_density_quant = gtools::quantcut(spr_density, q = 3))

ggplot(survivalres, aes(oft1, alive_aug, col = spr_density_binned)) + 
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = spr_density_binned),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "Activity",
       y = "Probability of survival to autumn",
       title = "Effects of offspring activity on survival to autumn",
       col = "Grid density (Spring)",
       tag = "(a)")

ggplot(survivalres, aes(mis1, alive_aug, col = spr_density_binned)) + 
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = spr_density_binned),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "Aggression",
       y = "Probability of survival to autumn",
       title = "Effects of offspring aggression on survival to autumn",
       col = "Grid density (Spring)",
       tag = "(b)")

## EFFECTS OF CHICKADEE PLAYBACKS ####
ggplot(recent4 %>%
         filter(!gridtreat == "rattle"),
       aes(gridtreat, oft1, col = gridtreat)) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() +
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_color_paletteer_d("colorblindr::OkabeIto") + #how do i add p-vals
  labs(x = "Control type",
       y = "Activity",
       title = "Effects of chickadee playbacks on offspring activity",
       col = "Control type",
       tag = "(a)")

ggplot(recent4 %>%
         filter(!gridtreat == "rattle"),
       aes(gridtreat, mis1, col = gridtreat)) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_color_paletteer_d("colorblindr::OkabeIto") + #how do i add p-vals
  labs(x = "Control type",
       y = "Aggression",
       title = "Effects of chickadee playbacks on offspring aggression",
       col = "Control type",
       tag = "(b)")

## EFFECTS OF TREATMENT ON OFFSPRING PERSONALITY
ggplot(recent4,
       aes(treatment, oft1, col = treatment)) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_color_paletteer_d("colorblindr::OkabeIto") + #how do i add p-vals
  labs(x = "Treatment",
       y = "Activity",
       title = "Effects of density cues on offspring activity",
       col = "Treatment",
       tag = "(a)")

ggplot(recent4,
       aes(treatment, mis1, col = treatment)) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_color_paletteer_d("colorblindr::OkabeIto") + #how do i add p-vals
  labs(x = "Treatment",
       y = "Aggression",
       title = "Effects of density cues on offspring aggression",
       col = "Treatment",
       tag = "(a)")

# activity x aggression corr
ggplot(survivalres, aes(oft1, mis1, col = treatment)) + 
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = treatment),
              method = "lm",
              se = F) +
  stat_smooth(method = "lm",
              se = F,
              aes(group = 1)) + 
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "Activity",
       y = "Aggression",
       title = "Relationship between activity and aggression",
       col = "Grid density (Spring)")

## EFFECTS OF MATERNAL BEHAVIOR ON OFFSPRING PERSOPNALITY
ggplot(master,
       aes(return_lat, oft1, col = treatment)) + 
  geom_point(size = 3,
             alpha = 0.5) + 
  geom_smooth(method = "lm",
              se = F) +
  scale_color_paletteer_d("colorblindr::OkabeIto") + #how do i add p-vals
  labs(x = "Maternal latency to return (Censored)",
       y = "Activity",
       title = "Effects of maternal attentiveness on offspring activity",
       col = "Treatment",
       tag = "(a)")

ggplot(master,
       aes(return_lat, mis1, col = treatment)) + 
  geom_point(size = 3,
             alpha = 0.5) + 
  geom_smooth(method = "lm",
              se = F) +
  scale_color_paletteer_d("colorblindr::OkabeIto") + #how do i add p-vals
  labs(x = "Maternal latency to return (Censored)",
       y = "Aggression",
       title = "Effects of maternal attentiveness on offspring aggression",
       col = "Treatment",
       tag = "(b)")

ggplot(master,
       aes(return_lat, oft1 * mis1, col = treatment)) + 
  geom_point(size = 3,
             alpha = 0.5) + 
  geom_smooth(method = "lm",
              se = F) +
  scale_color_paletteer_d("colorblindr::OkabeIto") + #how do i add p-vals
  labs(x = "Maternal latency to return (Censored)",
       y = "Aggression",
       title = "Effects of maternal attentiveness on offspring aggression",
       col = "Treatment",
       tag = "(b)")

## EFFECTS OF TREATMENT ON MATERNAL BEHAVIOR ####
ggplot(recent4 %>%
         filter(!is.na(m_return)),
       aes(treatment, return_lat, col = gridyear)) + 
  labs(col = "Grid by year") +
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) + 
  scale_colour_viridis_d() + #viridis is colorblind-friendly
  labs(x = "Treatment",
       y = "Standardized latency to return to pups (Censored)",
       title = "Effect of density cues on maternal latency to return to pups") 





ggplot(survivalres, aes(mis1, alive_aug, col = spr_density_binned)) + 
  geom_point(size = 3,
             alpha = 0.5) +
  stat_smooth(aes(color = spr_density_binned),
              method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "Aggression",
       y = "Probability of survival to autumn",
       title = "Effects of offspring aggression on survival to autumn",
       col = "Grid density (Spring)",
       tag = "(b)")
