
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# EXPLANATION ####
# this script prepared personality data from personality-master.csv for running
# PCA and obtaining representative activity and aggression scores
# only juveniles who were 60-80 days at the time they went through the
# behavioral assays were included in this trial
# explanation on how raw data was processed to be entered into
# personality-master.csv can be found in the BORIS protocol document (Nov 2021
# version and later). there is also a brief summary at the end of this script.

# importing data and cleaning
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
         !observer == "SWK", #because of GC experiment
         sq_id %in% weaned_juvs,
         !(sq_id == "22684" & trialnumber == 2), #too old on this trial
         !(sq_id == "22964" & trialnumber == 1), #too young
         !(sq_id == "22966" & trialnumber == 1), #too young
         !(sq_id == "22979" & trialnumber == 1), #too young
         !(tagrt == "M6964"), #too young
         !(sq_id == "22985" & trialnumber == 2), #trial 2 only 8 days after trial 1 
         !(sq_id == "23052" & trialnumber == 2)) #trial 2 only 13 days after trial 1
  
colnames(personality)[1] <- "juv_id" #distinguish from squirrel_id in other tables since these are all juvs

personality$trialdate <- as.Date(personality$trialdate,
                                 "%m/%d/%y")
personality$julian_trialdate <- yday(personality$trialdate) #to get juv age at trial

personality[is.na(personality$oft_duration),
          "oft_duration"] <- 450.000
personality[is.na(personality$mis_duration),
          "mis_duration"] <- 300.000

personality <- personality %>%
  drop_na(walk, #eliminates no OFT 
          front) #eliminates no MIS

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

# PCA ####

#OFT
pca.oft <- dudi.pca(beh.oft,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 7)


pca.oft$c1 <- (pca.oft$c1 * -1) #loadings were reversed
pca.oft$c1
personality$oft1 <- (pca.oft$l1$RS1 * -1) 
get_eig(pca.oft)

#MIS
pca.mis <- dudi.pca(beh.mis,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 5)

pca.mis$c1
get_eig(pca.mis)
personality$mis1 <- pca.mis$l1$RS1

# RAW PERSONALITY DATA PROCESSING ####
# after exporting .csv files from BORIS, OFT and MIS behaviors were cut off at
# the 450s and 300s marks respectively.
# all behaviors were summed (or counted for discrete behaviors)
# for Still, all occurrences ≥ 1.9 seconds were summed
# occurrences of Still that were less than 1.9 seconds AND took place on the
# ground (i.e. NOT in Hang) were added to Walk time
# approach and attack latencies were censored to the max time (duration of the
# MIS trial) in the event that the focal individual did not see itself in the 
# mirror, approach, and/or attack

