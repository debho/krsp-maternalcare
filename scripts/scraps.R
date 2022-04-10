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
