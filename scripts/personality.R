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
  filter(ageclass == "J", #takes only juvs
         grid %in% c("JO", "BT", "KL", "SU", "RR", "SUX"), #takes only the grids in study
         Exclude_unless_video_reanalyzed == "N", #eliminates any exclusions
         Proceed_with_caution == "N",
         !(grid == "KL" & (year == 2018 | year == 2017) & trialnumber == 1), #those squirrels were too young
         !(sq_id == "25287" & trialnumber == 2), #eliminates known exclusions
         !(sq_id == "23686" & trialnumber == 2),
         !(sq_id == "19257" & trialnumber == 1),
         !is.na(sq_id),
         !observer == "SWK") #because of GC experiment

colnames(personality)[1] <- "juv_id" #distinguish from squirrel_id in other tables since these are all juvs

personality$trialdate <- as.Date(personality$trialdate,
                                 "%m/%d/%y")
personality$julian_trialdate <- yday(personality$trialdate)
personality$videodate <- as.Date(personality$videodate,
                                 "%m/%d/%y")
personality[is.na(personality$oft_duration),
          "oft_duration"] <- 450.000
personality[is.na(personality$mis_duration),
          "mis_duration"] <- 300.000

## year counts pre-removal of those missing either OFT/MIS
# '05: 44; '09: 2; '12: 50; '16: 1; '17: 54; '18: 40; '19: 18; '20: 37; '21: 28

personality <- personality %>%
  drop_na(walk, #eliminates no OFT 
          front) #eliminates no MIS

##n = 255
# '12: 31

behaviors <- transmute(personality,
                       juv_id,
                       walk,
                       jump,
                       hole,
                       hang,
                       still,
                       chew,
                       groom,
                       oft_duration,
                       front,
                       back,
                       attack,
                       attacklatency,
                       approachlatency,
                       mis_duration) %>%
  mutate_if(is.character,
            as.numeric)

#extracts for each trait
beh.oft <- transmute(behaviors,
                     walk_prop = (walk/oft_duration),
                     jump_prop = (jump/oft_duration),
                     hole_prop = (hole/oft_duration),
                     hang_prop = (hang/oft_duration),
                     still_prop = (still/oft_duration),
                     chew_prop = (chew/oft_duration),
                     groom_prop = (groom/oft_duration)) 

beh.mis <- transmute(behaviors,
                     front_prop = (front/mis_duration),
                     back_prop = (back/mis_duration),
                     approachlat_prop = (approachlatency/mis_duration),
                     attacklat_prop = (attacklatency/mis_duration),
                     attack_prop = (attack/mis_duration)) 

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
personality$oft1 <- (pca.oft$l1$RS1 * -1)
get_eig(pca.oft)

# PCA loadings for MIS

pca.mis <- dudi.pca(beh.mis,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 5)

pca.mis$c1
get_eig(pca.mis)
personality$mis1 <- pca.mis$l1$RS1
