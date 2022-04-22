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
library(gridExtra) #for putting plots onto a grid
library(ggpubr) #for putting p-values onto plots

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


# survivalres is the master table with added columns for standardized residuals for survival data
# and it has densities binned instead of just continuous

oft_survivalPLOT <- ggplot(survivalres, aes(oft1, alive_aug, col = spr_density_binned)) + 
  theme_classic() +
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

mis_survivalPLOT <- ggplot(survivalres, aes(mis1, alive_aug, col = spr_density_binned)) + 
  theme_classic() +
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
oft_controlsPLOT <- ggplot(recent4 %>%
                             filter(!gridtreat == "rattle"),
                           aes(gridtreat, oft1, col = gridtreat)) + 
  theme_classic() +
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() +
  geom_jitter(color = "black",
              size = 2,
              alpha = 0.5) + #how do i add p-vals
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "Control type",
       y = "Activity",
       title = "Effects of chickadee playbacks on offspring activity",
       col = "Control type",
       tag = "(a)") + 
  stat_compare_means(method = "t.test",
                     aes(label = "p.format"))

mis_controlsPLOT <- ggplot(recent4 %>%
                             filter(!gridtreat == "rattle"),
                           aes(gridtreat, mis1, col = gridtreat)) + 
  theme_classic() +
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 2,
              alpha = 0.5) + #how do i add p-vals
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "Control type",
       y = "Aggression",
       title = "Effects of chickadee playbacks on offspring aggression",
       col = "Control type",
       tag = "(b)") + 
  stat_compare_means(method = "t.test",
                     aes(label = "p.format"))

ggsave("output/personality~controls_boxplot.png",
       arrangeGrob(oft_controlsPLOT, mis_controlsPLOT,
                   ncol = 2))

## EFFECTS OF TREATMENT ON OFFSPRING PERSONALITY

oft_treatmentPLOT <- ggplot(recent4,
                            aes(treatment, oft1, col = treatment)) + 
  theme_classic() +
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 2,
              alpha = 0.5) + #how do i add p-vals
  scale_color_paletteer_d("colorblindr::OkabeIto") + 
  labs(x = "Treatment",
       y = "Activity",
       title = "Effects of density cues on offspring activity",
       col = "Treatment",
       tag = "(a)")

mis_treatmentPLOT <- ggplot(recent4,
                            aes(treatment, mis1, col = treatment)) + 
  theme_classic() +
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 2,
              alpha = 0.5) + #how do i add p-vals
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  labs(x = "Treatment",
       y = "Aggression",
       title = "Effects of density cues on offspring aggression",
       col = "Treatment",
       tag = "(b)")

ggsave("output/personality~treatment_boxplot.png",
       arrangeGrob(oft_treatmentPLOT, mis_treatmentPLOT,
                   ncol = 2))

## EFFECTS OF MATERNAL BEHAVIOR ON OFFSPRING PERSONALITY

oft_carePLOT <- ggplot(master %>%
                         filter(!is.na(m_return)), #takes out the years with no maternal behavior data
                       aes(return_lat, oft1, col = year)) + 
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.8) + 
  geom_smooth(method = "lm",
              se = F,
              aes(group = 1)) + #plots overall regression line
  scale_color_paletteer_d("colorBlindness::paletteMartin") + 
  labs(x = "Standardized maternal latency to return",
       y = "Activity",
       title = "Effects of maternal attentiveness on offspring activity",
       col = "Year",
       tag = "(a)")

mis_carePLOT <- ggplot(master %>%
                         filter(!is.na(m_return)), #takes out the years with no maternal behavior data
                       aes(return_lat, mis1, col = year)) + 
  theme_classic() +
  geom_point(size = 3,
             alpha = 0.8) + 
  geom_smooth(method = "lm",
              se = F,
              aes(group = 1)) + #plots overall regression line
  scale_color_paletteer_d("colorBlindness::paletteMartin") + 
  labs(x = "Standardized maternal latency to return",
       y = "Aggression",
       title = "Effects of maternal attentiveness on offspring aggression",
       col = "Year",
       tag = "(b)")

ggsave("output/personality~care_scatter.png",
       arrangeGrob(oft_carePLOT, mis_carePLOT,
                   ncol = 2))

## EFFECTS OF TREATMENT ON MATERNAL BEHAVIOR ####
return_treatmentPLOT <- ggplot(recent4 %>%
                                 filter(!is.na(m_return)),
                               aes(treatment, return_lat, col = gridyear)) + 
  theme_classic() +
  labs(col = "Grid by year") +
  stat_boxplot(geom = "errorbar", width = 0.6) +
  geom_boxplot() + 
  geom_jitter(color = "black",
              size = 2,
              alpha = 0.5) + #how do i add p-vals
  scale_color_paletteer_d("colorBlindness::paletteMartin") + 
  labs(x = "Treatment",
       y = "Standardized latency to return",
       title = "Effect of density cues on maternal latency to return to pups") 


ggsave("output/return~treatment_boxplot.png",
       plot = return_treatmentPLOT)
