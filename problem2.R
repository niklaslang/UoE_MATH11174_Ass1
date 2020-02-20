library(data.table)

gfr.1 <- read.csv("assignment1_data/ltegfr1.csv", stringsAsFactors = TRUE)
gfr.2 <- read.csv("assignment1_data/ltegfr2.csv", stringsAsFactors = TRUE)

### (a) ###

# converting the files to data tables and merge in an appropriate way into a single data table

eGFR.dt <- merge(data.table(gfr.1),
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

for (pat.id in 1:max(eGFR.dt$id)){
  
  # calculate average eGFR for each patient
  avg.egfr <- mean(eGFR.dt[id == pat.id]$egfr, na.rm = TRUE)
  # calculate average follow-up for each patient
  avg.fu.years <- mean(eGFR.dt[id == pat.id]$fu.years, na.rm = TRUE)
  # create a summary table
  summary.egfr.table <- rbind(summary.egfr.table, c(pat.id, avg.egfr, avg.fu.years))
}

colnames(summary.egfr.table) <- c("pat.id", "avg.gfr", "avg.fu.years") # assign colnames to table
summary.egfr.dt <- data.table(summary.egfr.table) # create data.table
head(summary.egfr.dt)

# tabulating the number of patients with average eGFR in the following ranges: 
# (0, 15], (15, 30], (30, 60], (60, 90], (90, ∞)
egfr.bins <- c(0, 15, 30, 60, 90, Inf)
table(cut(summary.egfr.dt$avg.gfr, egfr.bins))

# Counting and reporting the number of patients with missing average eGFR: 
sum(is.na(summary.egfr.dt$avg.gfr))
missing.avg.egfr

# Ensuring that the table ordering is returned to id, and follow-up time

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

slope.bins <- c(-Inf,-3,0,3,Inf)
table(cut(coef(summary.egfr.dt$lr.models[[1]])[2], slope.bins))

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

### (c) ###

# For patients with average eGFR in the (0,15] range, 
# collect in a data table their identifier, sex, age at baseline, average eGFR, 
# time of last eGFR reading and number of eGFR measurements taken

eGFR.le.15 <- NULL

for (i in 1:max(summary.egfr.dt$pat.id)){
  avg.gfr <- summary.egfr.dt[pat.id == i]$avg.gfr

  if (!is.na(avg.gfr) && avg.gfr > 0 && avg.gfr <= 15){
    sex <- eGFR.dt[id == i]$sex[1]
    age <- eGFR.dt[id == i]$baseline.age[1]
    last.egfr <- last(eGFR.dt[id == i]$fu.years)
    egfr.measurements <- sum(!is.na(eGFR.dt[id == i]$egfr))
    
    eGFR.le.15 <- rbind(eGFR.le.15, c(i, sex, age, avg.gfr, last.egfr, egfr.measurements))

  }
  
  
}

eGFR.le.15.dt <- data.table(eGFR.le.15)
colnames(eGFR.le.15.dt) <- c('Patient ID', 'Sex', 'Age (baseline)', 'average eGFR', 'last eGFR measurments', 'eGFR measurements')

head(eGFR.le.15.dt)

