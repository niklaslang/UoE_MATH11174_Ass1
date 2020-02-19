library(data.table)

gfr.1 <- read.csv("assignment1_data/ltegfr1.csv", stringsAsFactors = TRUE)
gfr.2 <- read.csv("assignment1_data/ltegfr2.csv", stringsAsFactors = TRUE)

### (a) ###

# converting the files to data tables and merge in an appropriate way into a single data table

eGFR.dt <- merge( data.table(gfr.1),
                 data.table(gfr.2),
                 by.x = c("id","fu.years"), 
                 by.y = c("ID","fu.years"))

# ordering the observations according to subject identifier and follow-up time

setorderv(eGFR.dt, c("id","fu.years"), c(1,1))

# adding an assertion that the ordering is correct

stopifnot( all(eGFR.dt[, c("id", "fu.years")] == eGFR.dt[order(id, fu.years), fu.years, by = id] ))

           