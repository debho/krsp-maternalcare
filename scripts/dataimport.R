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
  collect() %>%
  filter(trap_date > "2017-12-31")
  
litters <- tbl(con, "litter") %>%
  collect() 
 
juveniles <- tbl(con, "juvenile") %>%
  collect() 

ids <- tbl(con, "historic_squirrel_ids") %>%
  collect()

census <- tbl(con, "census") %>%
  collect() %>%
  filter(census_date > "2017-12-31")


