
##############################################################################
############################### Ho et al., 2022 ##############################
##############################################################################

# EXPLANATION ####
# this script reruns the PCA analyses on personality data for Ho et al., 2022
# manuscript.
# changes from the AMDP thesis data include:
# 1. personality master file used is the one updated by ARM (May 2022)
# 2. includes squirrels from AG grid
# 3. uses trial 1 of KL 2017-2018 instead of trial 2
# 4. removes the 60-80 days filter to look at age effects

# PERSONALITY DATA ####
personality_updated <- read.csv("data/personality-master-updated.csv",
                                header = T) %>%
  filter(ageclass == "J", #takes only juvs
         grid %in% c("JO", "BT", "KL", "SU", "RR", "SUX", "AG"), #takes only the grids in study
         Exclude_unless_video_reanalyzed == "N", #eliminates any exclusions
         Proceed_with_caution == "N",
         !(grid == "KL" & (year == 2018 | year == 2017) & trialnumber == 2), #only takes trial 1
         !(sq_id == "25287" & trialnumber == 2), #eliminates known exclusions
         !(sq_id == "23686" & trialnumber == 2),
         !(sq_id == "19257" & trialnumber == 1),
         !is.na(sq_id),
         !observer == "SWK", #because of GC experiment
         !(sq_id == "22985" & trialnumber == 2), #trial 2 only 8 days after trial 1 
         !(sq_id == "23052" & trialnumber == 2)) #trial 2 only 13 days after trial 1

colnames(personality_updated)[1] <- "juv_id" #distinguish from squirrel_id in other tables since these are all juvs

personality_updated$trialdate <- as.Date(personality_updated$trialdate,
                                 "%m/%d/%y")
personality_updated$julian_trialdate <- yday(personality_updated$trialdate) #to get juv age at trial

# sets all trial durations
personality_updated[is.na(personality_updated$oft_duration),
                    "oft_duration"] <- 450.000
personality_updated[is.na(personality_updated$mis_duration),
                    "mis_duration"] <- 300.000

behaviors <- transmute(personality_updated,
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
beh.oft <- beh.oft %>%
  drop_na(walk_prop)

beh.mis <- transmute(behaviors,
                     front_prop = (front/mis_duration),
                     back_prop = (back/mis_duration),
                     approachlat_prop = (approachlatency/mis_duration),
                     attacklat_prop = (attacklatency/mis_duration),
                     attack_prop = (attack/mis_duration))
beh.mis <- beh.mis %>%
  drop_na(front_prop)

# PCA ####
#OFT
pca.oft <- dudi.pca(beh.oft,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 7)

pca.oft$c1 <- (pca.oft$c1 * -1) #loadings were reversed
pca.oft$c1
personality_updated$oft1 <- (pca.oft$l1$RS1 * -1) 
get_eig(pca.oft)

#MIS
pca.mis <- dudi.pca(beh.mis,
                    scale = TRUE,
                    scannf = FALSE,
                    nf = 5)

pca.mis$c1
get_eig(pca.mis)
personality_updated$mis1 <- pca.mis$l1$RS1 
