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
  filter(cohort > 2017, #takes only 2018 to 2021
         ageclass == "J", #takes only juvs
         grid %in% c("BT", "KL", "JO", "SU"), 
         Exclude_unless_video_reanalyzed == "N",
         Proceed_with_caution == "N") #eliminates any exclusions

# drop duplicate squirrels
personality <- personality[!duplicated(personality$sq_id),]

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

library(ade4) #for PCA loadings
library(factoextra) #to get variance and all that good stuff

#PCA loadings for OFT

pca.oft <- dudi.pca(beh.oft,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 7)


pca.oft$c1 <- (pca.oft$c1 * -1)
pca.oft$c1
pca.oft$eig


personality$oft1 <- (pca.oft$l1$RS1 * -1) #because loadings are reversed for some reason
get_eig(pca.oft)

# PCA loadings for MIS

pca.mis <- dudi.pca(beh.mis,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 5)

pca.mis$c1
pca.mis$eig
get_eig(pca.mis)
personality$mis1 <- pca.mis$l1$RS1
