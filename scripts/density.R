##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for getting grid density data

census_2 <- census %>%
  rename (locX = locx,
          locY = locy,
          grid = gr,
          date = census_date,
          Sex = sex) %>%
  select(-sq_fate)

census_all<-bind_rows(middens, census_2)%>% 
  mutate(grid = factor(grid),
         year = year(ymd(date)),
         month = month(ymd(date)),
         Sex = factor(Sex)) %>%
  filter(grid %in% c("JO","RR","BT","KL", "SU", "SUX")) %>%
  mutate(grid = as.factor(grid))

SUJOKL_core_may_census <- filter(census_all, month==5,
                               grid %in% c("SU", "JO", "KL"),
                               locX>=-0.2, locX<=20.8,
                               locY>=-0.2, locY<=20.8) #39.69ha

core_may_census_all<-bind_rows(SUJOKL_core_may_census)

grids_density <- group_by(core_may_census_all, year, grid) %>% 
  filter(!is.na(squirrel_id)) %>%
  summarise(spr_number = n_distinct (squirrel_id)) %>% 
  select (year, grid, spr_number)

grid <- c("KL", "SU", "JO")
grid_area <- c(39.69, 39.69, 39.69)
grid_area <- data.frame(grid, grid_area)

grids_density <- left_join(grids_density, grid_area, by = "grid") %>% 
  mutate(spr_density = spr_number/grid_area) #annual grid density for SU, JO, and KL