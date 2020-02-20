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

summary.egfr.table <- NULL

# creating bins for eGFR ranges:
CKD.stage.1 <- 0
CKD.stage.2 <- 0
CKD.stage.3 <- 0
CKD.stage.4 <- 0
CKD.stage.5 <- 0

# creating bin for missing average eGFR
missing.avg.egfr <- 0

for (pat.id in 1:max(eGFR.dt$id)){
  
  # calculate average eGFR for each patient
  avg.egfr <- mean(eGFR.dt[id == pat.id]$egfr)
  
  # calculate average follow-up for each patient
  avg.fu.years <- mean(eGFR.dt[id == pat.id]$fu.years)
  
  # create a summary table
  summary.egfr.table <- rbind(summary.egfr.table, c(pat.id, avg.egfr, avg.fu.years))
  
  # increment the respective bin counter by 1
  if ( is.na(avg.egfr) ){
    missing.avg.egfr <- missing.avg.egfr + 1
  } else if (avg.egfr > 0 && avg.egfr <= 15){
    CKD.stage.5 <- CKD.stage.5 + 1
  } else if (avg.egfr > 15 && avg.egfr <= 30){
    CKD.stage.4 <- CKD.stage.4 + 1
  } else if (avg.egfr > 30 && avg.egfr <= 60){
    CKD.stage.3 <- CKD.stage.3 + 1
  } else if (avg.egfr > 60 && avg.egfr <= 90){
    CKD.stage.2 <- CKD.stage.2 + 1
  } else if (avg.egfr > 90){
    CKD.stage.1 <- CKD.stage.1 + 1
  }
}

colnames(summary.egfr.table) <- c("pat.id", "avg.gfr", "avg.fu.years") # assign colnames to table
summary.egfr.dt <- data.table(summary.egfr.table) # create data.table
head(summary.egfr.dt)

# tabulating the number of patients with average eGFR in the following ranges: 
# (0, 15], (15, 30], (30, 60], (60, 90], (90, ∞)
CKD.stages.dt <- data.table("CKD stage" = c("I","II","III","IV","V"), 
                            "N Patients" = c(CKD.stage.1, CKD.stage.2, CKD.stage.3, CKD.stage.4, CKD.stage.5))
CKD.stages.dt

# Counting and reporting the number of patients with missing average eGFR: 
stopifnot(sum(is.na(summary.egfr.dt$avg.gfr)) == missing.avg.egfr)
sum(is.na(summary.egfr.dt$avg.gfr))
missing.avg.egfr

# Ensuring that the table ordering is returned to id, and follow-up time

# WHICH TABLE?

# eGFR.dt
setorderv(eGFR.dt, c("id","fu.years"), c(1,1))
stopifnot( all(eGFR.dt[, c("id", "fu.years")] == eGFR.dt[order(id, fu.years), fu.years, by = id] ))

# summary.egfr.dt
setorderv(summary.egfr.dt, c("pat.id","avg.fu.years"), c(1,1))
stopifnot( all(summary.egfr.dt[, c("pat.id", "avg.fu.years")] == summary.egfr.dt[order(pat.id, avg.fu.years), avg.fu.years, by = pat.id] ))

# Fitting a linear regression model for the eGFR measurements as a function of time
# for each patient with at least 3 eGFR readings,
# store it in the data table

lr.models <- vector(mode="list", length=max(eGFR.dt$id))

for (pat.id in 1:max(eGFR.dt$id)){
  
  if (sum(!is.na(eGFR.dt[id == pat.id]$egfr)) >= 3){
    regr.egfr <- lm(egfr ~  fu.years, data = eGFR.dt[id == pat.id], na.action = na.omit)
  } else {
    regr.egfr <- NA
  }
  
  lr.models[[pat.id]] <- regr.egfr
  
}

summary.egfr.dt <- cbind(summary.egfr.dt, lr.models)

# Counting how many patients have a slope < -3, [-3, 0), [0, 3], > 3

# Creating slope bins
slope.1 <- 0 # < -3
slope.2 <- 0 # [-3, 0)
slope.3 <- 0 # [0, 3]
slope.4 <- 0 # > 3

# Looping over pat.id 
for (id in 1:max(summary.egfr.dt$pat.id)){
  
  if (!is.na(summary.egfr.dt[pat.id == id ]$lr.models)){
    
    slope <- coef(summary.egfr.dt[pat.id == id ]$lr.models[[1]])[2]
    
    if (slope < -3){
      slope.1 <- slope.1 + 1
    } else if (slope >= -3 && slope < 0){
      slope.2 <- slope.2 + 1
    } else if (slope >= 0 && slope <= 3){
      slope.3 <- slope.3 + 1
    } else if (slope > 3){
      slope.4 <- slope.4 + 1
    }
  }
}

slopes.dt <- data.table("slope" = c("< -3","[-3, 0)","[0, 3]","> 3"), 
                        "N Patients" = c(slope.1, slope.2, slope.3, slope.4))
slopes.dt
