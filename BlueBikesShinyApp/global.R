# Helper libraries
library(DBI)
library(RSQLite)
library(pool)

####################
## Prepare database connection pool and global variables
####################
BOSTON_LON <- -71.0589
BOSTON_LAT <- 42.3601
DATABASE <- "bluebikes_Jul2018.sqlite3"
DATE_RANGE <- c(1,31)
HOUR_RANGE <- c(0,23)

pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = DATABASE
  )
onStop(function() {
  poolClose(pool)
})
