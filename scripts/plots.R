##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for plots

library(sjPlot)
library(sjmisc)


# plotting oft1 ~ mis1 correlation by grid
personality_grid <- ggplot(personality, aes(oft1, mis1, col = grid)) +
  geom_point(size = 3) + 
  labs(y = "Aggression", x = "Activity") +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", se = T, aes(group = 1))
personality_grid

# boxplots treatment 0 vs 1
ggplot(master, aes(treatment, oft1, col = grid,)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::sunset") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on activity")

ggplot(master, aes(treatment, mis1, col = grid)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::jazzcup") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on aggression")

ggplot(master, aes(treatment, oft1 + mis1, col = grid)) +
  geom_boxplot() +
  scale_color_paletteer_d("vapeplot::seapunk") + 
  geom_jitter(color = "black",
              size = 0.4,
              alpha = 0.5) +
  ggtitle("Effects of treatment on personality")


# return ~ move
ggplot(master, aes(t_return, t_move, col = treatment)) +
  geom_point(size = 3) + 
  labs(y = "Latency to move pups", x = "Latency to return") +
  scale_color_paletteer_d("vapeplot::cool") +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", se = F, aes(group = 1))

#return ~ treatment
ggplot(master, aes(treatment, t_return, col = grid)) +
  geom_point() +
  scale_color_paletteer_d("vapeplot::seapunk") + 
  ggtitle("Effects of treatment on latency to return")


# survival ~ activity + year
ggplot(recent4 %>%
         filter(!year == 2021),
       aes(oft1,
           survived_200d,
           col = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_color_paletteer_d("vapeplot::macplus") + 
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  labs(y = "Probability of surviving at least 200 days",
       x = "Activity")

# survival ~ aggression + year
ggplot(recent4 %>%
         filter(!year == 2021),
       aes(mis1,
           survived_200d,
           col = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_color_paletteer_d("vapeplot::vaporwave") + 
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
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  labs(y = "Probability of surviving at least 200 days",
       x = "Activity + Aggression")
