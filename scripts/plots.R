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

# boxplot that looks at jo vs bt or treatment 0 vs 1

# plotting survival
plot(oft1_care)
plot(mis1_care)
plot(personality_survival)
plot(personality_survivalNOJO)

# survival ~ activity + year
ggplot(recent4 %>%
         filter(!year == 2021),
       aes(oft1,
           survived_200d,
           col = year)) +
  geom_point(size = 3,
             alpha = 0.5) +
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
