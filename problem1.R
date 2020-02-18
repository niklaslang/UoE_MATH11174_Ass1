### problem 1 ###

# load library to work with data.tables

library(data.table)

# load data for problem 1

cohort <- read.csv("assignment1_data/cohort.csv", stringsAsFactors = TRUE)
lab <- read.csv("assignment1_data/lab1.csv")
linker <- read.csv("assignment1_data/linker.csv", stringsAsFactors = TRUE)

## (a) ##

# convert to data tables

cohort.dt <- data.table(cohort)
lab.dt <- data.table(lab)[,!"yob"] # ensure that only a single yob field remains
linker.dt <- data.table(linker)

# (a) Using all three files load and merge to create a single data table based dataset cohort.dt
# this will be used in your analysis

cohort.dt <- merge(cohort.dt,
                   linker.dt,
                   by.x = "id", by.y = "id")

cohort.dt <- merge(cohort.dt, 
                   lab.dt,
                   by.x = "LABID", by.y = "LABID")

# ensure that the albumin field is converted to a factor 
# and the ordering of the factor is 1=“normo”,2=“micro”,3=“macro”

levels(cohort.dt$albumin)
cohort.dt$albumin <- relevel(cohort.dt$albumin, "macro")
cohort.dt$albumin <- relevel(cohort.dt$albumin, "micro")
cohort.dt$albumin <- relevel(cohort.dt$albumin, "normo")
table(cohort.dt$albumin)

# perform assertion checks to ensure that all identifiers in cohort.csv have been accounted for 
# perform assertion checks to ensure that any validation fields are consistent between sets

# ensure that all identifiers in cohort.csv have been accounted for
stopifnot(all(cohort.dt$id %in% cohort$id))

# ensure that any validation fields are consistent between sets
setorder(cohort.dt, id)
setorder(linker.dt, id)

stopifnot(all(cohort.dt[,id, LABID] == linker.dt[,id, LABID]))
  
# stopifnot(all(for(orig.id in as.vector(cohort.dt$id)){isTRUE(linker.dt[id == orig.id][,2] == cohort.dt[id == orig.id][,2])}))

# after the assertions are complete, drop the identifier that originated from lab dataset LABID

cohort.dt$LABID <- NULL

## (b) ##

# evaluate if any missing age fields can be calculated
# ased on the age/year offset being consistent for the remainder of the cohort, update any missing age fields

# calculate the age offset

age.offset <- mean(cohort.dt$yob + cohort.dt$age, na.rm=TRUE)

# final code line

cohort.dt[, age := ifelse(is.na(age), age.offset-yob, age)]

## (c) ##

summary.cohort.dt <- data.table("Variable Names" = colnames(cohort.dt),
                                "Total N" = sapply(cohort.dt, length),
                                "Missing N (%)" = sapply(cohort.dt, function(z)sum(is.na(z))/length(z)*100))
