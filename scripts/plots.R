##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for plots

# plotting oft1 ~ mis1 correlation by grid
personality_grid <- ggplot(personality, aes(oft1, mis1, col = grid)) +
  geom_point(size = 3) + 
  labs(y = "Aggression", x = "Activity") +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "lm", se = T, aes(group = 1))
personality_grid

# boxplot that looks at jo vs bt or treatment 1 vs 2