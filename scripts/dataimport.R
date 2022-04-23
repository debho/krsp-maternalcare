
##############################################################################
############ AMDP Thesis - Deborah Ho, University of Michigan 2022 ###########
##############################################################################

# EXPLANATION ####
# this script is where I import tables from the KRSP database to consolidate
# data for all analyses

# library for krsp functions
library(krsp)

# connecting to database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname ="krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

# lists available tables
krsp_tables(con)

# pulling tables from database
squirrels <- tbl(con, "squirrel") %>%
  collect() 
  
litters <- tbl(con, "litter") %>%
  collect()

juveniles <- tbl(con, "juvenile") %>%
  collect()

traps <- tbl(con, "trapping") %>%
  collect() 

census <- tbl(con, "census") %>%
  collect() %>%
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) %>%
  filter(!sq_fate == 7)

flastall <- tbl(con, "flastall2") %>% 
  collect() 

middens <- tbl(con, "dbamidden") %>%
  collect() %>%
  mutate(locX = loc_to_numeric(locX),
         locY = loc_to_numeric(locY))

