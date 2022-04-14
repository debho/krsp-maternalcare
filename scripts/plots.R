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
library(lattice)

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
         filter(!year == 2021),
       aes(oft1,
           survived_200d,
           col = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
  scale_color_paletteer_d("vapeplot::vaporwave") + 
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
  scale_color_paletteer_d("vapeplot::vaporwave") + 
  stat_smooth(method = "glm",
              se = F,
              method.args = list(family = binomial)) +
  labs(y = "Probability of surviving at least 200 days",
       x = "Activity + Aggression")

#maternal care and personality

