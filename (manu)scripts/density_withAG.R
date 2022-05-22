
##############################################################################
############################### Ho et al., 2022 ##############################
##############################################################################

## SCRIPT ADAPTED FROM DR. ANDREW MCADAM'S SCRIPT ON FINDING GRID DENSITY
# EXPLANATION ####
# this script was used to calculate the spring grid density per grid-year
# combination using data from the May census
# i forked this script from the KRSP Github, under krsp-functions

census_2 <- census %>%
  rename (locX = locx,
          locY = locy,
          grid = gr,
          date = census_date,
          Sex = sex)

census_all<-bind_rows(middens, census_2)%>% 
  mutate(grid = factor(grid),
         year = year(ymd(date)),
         month = month(ymd(date)),
         Sex = factor(Sex)) %>%
  filter(grid %in% c("JO","RR","BT","KL", "SU", "SUX", "AG")) %>%
  mutate(grid = as.factor(grid))

SUJOKL_core_may_census <- filter(census_all, month==5,
                                 grid %in% c("SU", "JO", "KL"),
                                 locX>=-0.2, locX<=20.8,
                                 locY>=-0.2, locY<=20.8) #39.69ha

AG_core_may_census <- filter(census_all, month==5, 
                           grid =="AG", 
                           locX>=-0.2, locX<=20.8, 
                           locY>=-0.2, locY<=23.8) #45.36ha

core_may_census_all <- bind_rows(SUJOKL_core_may_census,
                               AG_core_may_census)

grids_density <- group_by(core_may_census_all, year, grid) %>% 
  filter(!is.na(squirrel_id)) %>%
  summarise(spr_number = n_distinct (squirrel_id)) 

grid <- c("KL", "SU", "JO", "AG")
grid_area <- c(39.69, 39.69, 39.69, 45.36)
grid_area <- data.frame(grid, grid_area)

grids_density <- left_join(grids_density, grid_area, by = "grid") %>% 
  mutate(spr_density = spr_number/grid_area,
         gridyear = paste(grid, year)) #annual grid density for SU, JO, KL, and AG
