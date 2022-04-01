##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff

##############################################################################
### Script for setting personality data up for analysis

#importing data and cleaning
personality <- read.csv('data/personality-master.csv',
                            header = T,
                            na.strings = c("", " ", "NA")) %>%
  filter(cohort > 2017, #takes only 2018 to 2021, STILL NEED TO ADD IN 2019 DATA!
         ageclass == "J", #takes only juvs
         Exclude_unless_video_reanalyzed == "N") #eliminates any exclusions

behaviors <- mutate_if(personality,
                       is.character, as.numeric) #convert all to numbers

#extracts OFT behaviors
beh.oft <- transmute(behaviors,
                     walk_prop = (walk/oft_duration),
                     jump_prop = (jump/oft_duration),
                     hole_prop = (hole/oft_duration),
                     hang_prop = (hang/oft_duration),
                     still_prop = (still/oft_duration),
                     chew_prop = (chew/oft_duration),
                     groom_prop = (groom/oft_duration)
)

#extracts MIS behaviors
beh.mis <- transmute(behaviors,
                     front_prop = (front/mis_duration),
                     back_prop = (back/mis_duration),
                     approachlat_prop = (mis_duration/approachlatency), #reversed because of rev corr with aggression
                     attacklat_prop = (mis_duration/attacklatency), #reversed because of rev corr with aggression
                     attack_prop = (attack/mis_duration)
)

#PCA ####

library(ade4) #trying this out again lmao

#PCA loadings for OFT

pca.oft <- dudi.pca(beh.oft,
                    scale = TRUE,)


pca.oft$c1

pca.oft$eig
head(pca.oft$l1)

personality$oft1 <- (pca.oft$l1$RS1) * -1 #because loadings are reversed for some reason

# PCA loadings for MIS

pca.mis <- dudi.pca(beh.mis,
                    scale = TRUE,)

pca.mis$c1
pca.mis$eig
head(pca.mis$l1)

personality$mis1 <- pca.mis$l1$RS1

#looking at correlation between OFT and MIS scores
cor.test(personality$oft1,
         personality$mis1,
         alternative = "greater") #p < .05
