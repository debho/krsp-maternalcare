##############################################################################
############### Maternal care effects on offspring personality ###############
##############################################################################
### How does maternal care style influence the development of offspring
### personality and what are the fitness effects on offspring?
### offspring personality ~ maternal care * density + other stuff
##############################################################################
### Script for importing data from krsp database

#library for krsp functions
library(krsp)

#connecting to database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname ="krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

#pulling tables from database
krsp_tables(con)

squirrels <- tbl(con, "squirrel") %>%
  collect() 
  
litters <- tbl(con, "litter") %>%
  collect() 

juveniles <- tbl(con, "juvenile") %>%
  collect() 

traps <- tbl(con, "trapping") %>%
  collect() 

changed_ids <- tbl(con, "historic_squirrel_ids") %>%
  collect()

census <- tbl(con, "census") %>%
  collect()

flastall <- tbl(con, "flastall2") %>% 
  collect()
