library(data.table)

gfr.1 <- read.csv("assignment1_data/ltegfr1.csv", stringsAsFactors = TRUE)
gfr.2 <- read.csv("assignment1_data/ltegfr2.csv", stringsAsFactors = TRUE)

### (a) ###

# converting the files to data tables and merge in an appropriate way into a single data table

eGFR.dt <- merge( data.table(gfr.1),
                 data.table(gfr.2),
                 by.x = c("id","fu.years"), 
                 by.y = c("ID","fu.years"),
                 all = TRUE )

# ordering the observations according to subject identifier and follow-up time

setorderv(eGFR.dt, c("id","fu.years"), c(1,1))

# adding an assertion that the ordering is correct

stopifnot( all(eGFR.dt[, c("id", "fu.years")] == eGFR.dt[order(id, fu.years), fu.years, by = id] ))

### (b) ###

# Computing the average eGFR and length of follow-up for each patient

avg.table <- NULL

# creating bins for eGFR ranges:
CKD.stage.1 <- 0
CKD.stage.2 <- 0
CKD.stage.3 <- 0
CKD.stage.4 <- 0
CKD.stage.5 <- 0

# creating bin for missing average eGFR
missing.avg.egfr <- 0

for (pat.id in 1:max(eGFR.dt$id)){
  avg.egfr <- mean(eGFR.dt[id == pat.id]$egfr) 
  avg.fu.years <- mean(eGFR.dt[id == pat.id]$fu.years)
  avg.table <- rbind(avg.table, c(pat.id, avg.egfr, avg.fu.years))
  if ( is.na(avg.egfr) ){
    missing.avg.egfr <- missing.avg.egfr + 1
  } else if (avg.egfr >= 0 && avg.egfr < 15){
    CKD.stage.1 <- CKD.stage.1 + 1
  } else if (avg.egfr >= 15 && avg.egfr < 30){
    CKD.stage.2 <- CKD.stage.2 + 1
  } else if (avg.egfr >= 30 && avg.egfr < 60){
    CKD.stage.3 <- CKD.stage.3 + 1
  } else if (avg.egfr >= 60 && avg.egfr < 90){
    CKD.stage.4 <- CKD.stage.4 + 1
  } else if (avg.egfr > 90){
    CKD.stage.5 <- CKD.stage.5 + 1
  }
}

colnames(avg.table) <- c("pat.id", "avg.gfr", "avg.fu.years")
avg.dt <- data.table(avg.table)

# tabulating the number of patients with average eGFR in the following ranges: 
# (0, 15], (15, 30], (30, 60], (60, 90], (90, ∞)
CKD.stages.dt <- data.table("CKD stage" = c(1,2,3,4,5), 
                            "N Patients" = c(CKD.stage.1, CKD.stage.2, CKD.stage.3, CKD.stage.4, CKD.stage.5))

# Counting and reporting the number of patients with missing average eGFR: 
stopifnot(sum(is.na(avg.dt$avg.gfr)) == missing.avg.egfr)
sum(is.na(avg.dt$avg.gfr))
missing.avg.egfr

# Ensuring that the table ordering is returned to id, and follow-up time

# WHICH TABLE?

# eGFR.dt
setorderv(eGFR.dt, c("id","fu.years"), c(1,1))
stopifnot( all(eGFR.dt[, c("id", "fu.years")] == eGFR.dt[order(id, fu.years), fu.years, by = id] ))

# avg.dt
setorderv(avg.dt, c("pat.id","avg.fu.years"), c(1,1))
stopifnot( all(avg.dt[, c("pat.id", "avg.fu.years")] == avg.dt[order(pat.id, avg.fu.years), avg.fu.years, by = pat.id] ))
