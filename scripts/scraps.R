##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Random script scraps

##RUNNING RANDOM CORRELATION TESTS
cor.test(master$oft1 + master$mis1,
         master$t_return)
cor.test(master$oft1 + master$mis1,
         master$t_move)
cor.test(master$oft1 + master$mis1,
         master$treatment)
cor.test(master$t_return,
         master$treatment)
cor.test(master$t_return,
         master$treatment)
cor.test(master$oft1,
         master$treatment)
cor.test(master$mis1,
         master$treatment)
cor.test(master$survived_200d,
         master$oft1 + master$treatment)
cor.test(master$survived_200d,
         master$mis1 - master$treatment)



# STEP 1: survival ~ personality * density ####

survival_pers <- lmer(survived_200d ~ oft1 + mis1 + treatment + (1|litter_id),
                      data = master)
survival_oft1 <- lmer(survived_200d ~ oft1 + treatment + (1|litter_id),
                      data = master)
survival_mis1 <- lmer(survived_200d ~ mis1 + treatment + (1|litter_id),
                      data = master)

summary(survival_pers)
summary(survival_oft1)
summary(survival_mis1)


pers_model <- lmer(oft1 + mis1 ~ sex + treatment + age_trial + (1|litter_id),
                   data = master)
oft1_model <- lmer(oft1 ~ sex + treatment + age_trial + (1|litter_id),
                   data = master)
mis1_model <- lmer(mis1 ~ sex + treatment + age_trial + (1|litter_id),
                   data = master)

summary(pers_model) 
summary(oft1_model)
summary(mis1_model)


# STEP 3: personality ~ maternal care ####

maternal_pers <- lmer(oft1 + mis1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
maternal_oft1 <- lmer(oft1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
maternal_mis1 <- lmer(mis1 ~ t_return + t_move + treatment + (1|litter_id),
                      data = master)
summary(maternal_pers)
summary(maternal_oft1) 
summary(maternal_mis1) 

##STEP 4: mis1 ~ LSR


oft1_means <- master %>%
  group_by(litter_id, treatment) %>%
  summarize(litter_means = mean(oft1),
            litter_se = sd(oft1)/sqrt(n()))
overall_oft1means <- oft1_means %>%
  group_by(treatment) %>%
  summarize(mean = mean(litter_means),
            se = sd(litter_means)/sqrt(n()))

ggplot(oft1_means, aes(x=litter_id, fill=factor(treatment), y=litter_means)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(c(min(oft1_means - 1), max(oft1_means + 1))) +
  theme_bw(base_size=12) +
  ylab("Activity")

mis1_means <- master %>%
  group_by(litter_id, treatment) %>%
  summarize(litter_meansMIS = mean(mis1),
            litter_se = sd(mis1)/sqrt(n()))
overall_oft1means <- mis1_means %>%
  group_by(treatment) %>%
  summarize(mean = mean(litter_meansMIS),
            se = sd(litter_meansMIS)/sqrt(n()))

ggplot(mis1_means, aes(x=litter_id, fill=factor(treatment), y=litter_meansMIS)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(c(min(mis1_means - 1),max(mis1_means + 1))) +
  theme_bw(base_size=12) +
  ylab("Aggression")


