##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff

##############################################################################
### Script for cleaning all data

##PERSONALITY
#new df with only 2018-2021
juv_personality <- filter(personality_raw,
                          Exclude_unless_video_reanalyzed == 'N',
                          ageclass == 'J',
                          cohort > 2017)

#convert all to factors and extract only the variables i need
behaviors <- mutate_if(juv_personality,
                       is.character, as.numeric)
behaviors_prop <- transmute(behaviors,
  walk_prop = (walk/oft_duration),
  jump_prop = (jump/oft_duration),
  hole_prop = (hole/oft_duration),
  hang_prop = (hang/oft_duration),
  still_prop = (still/oft_duration),
  chew_prop = (chew/oft_duration),
  groom_prop = (groom/oft_duration),
  front_prop = (front/mis_duration),
  back_prop = (back/mis_duration),
  approachlat_prop = (approachlatency/mis_duration),
  attacklat_prop = (attacklatency/mis_duration),
  attack_prop = (attack/mis_duration)
)

#PCA run in analysis.R

#attaches composites to  dataframe
juv_personality$oft1 <- pc$RS1
juv_personality$mis1 <- pc$RS2



