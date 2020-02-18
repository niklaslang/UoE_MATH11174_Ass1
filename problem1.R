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

## (c) AND (d) ##

# Creating a separate data table to provide a summary of the data in the `cohort.dt` table.
#It contains a row for each column in the `cohort.dt` table and the following columns:
# variable name
# Median (Interquartile Range) for continuous non-categorical data and N (%) for categorical
# Total N
# Missing N (%)

return.median.plus.iqr <- function(vector, impute.to.mean=FALSE){
  
  if(impute.to.mean==TRUE){
    # find missing values
    na.idx <- is.na(vector)
    # replace NAs with the median
    vector[na.idx] <- mean(vector, na.rm=TRUE)
  }
  
  v.mean <- round( median(vector, na.rm = TRUE),2)
  v.iqr <- round( quantile(vector, c(0.25, 0.75), na.rm = TRUE),2)
  output <- paste0(v.mean, " (", v.iqr[1],",",v.iqr[2],")")
  
  return(output)
}

# Creating a function that takes a vector of numeric or integer values 
# and returns a character string of the median and interquartile range. 
# The functions accepts a boolean parameter `impute.to.mean` to impute any missing values 
# to the mean value of the vector. 
# Its default value is set to `FALSE`:

summary.con.variables <- data.table("Variable" = colnames(cohort.dt[, .SD, .SDcols = sapply(cohort.dt, is.numeric)]),
                                    "Median (IQR) or N (%)" = sapply(cohort.dt[, .SD, .SDcols = sapply(cohort.dt, is.numeric)], return.median.plus.iqr),
                                    "Total N" = sapply(cohort.dt[, .SD, .SDcols = sapply(cohort.dt, is.numeric)], length),
                                    "Missing N (%)" = sapply(cohort.dt[, .SD, .SDcols = sapply(cohort.dt, is.numeric)], function(z)sum(is.na(z))/length(z)*100))

# Splitting categorical fields in the summary table into a row per category. 
# Creating a function that takes a vector of categorical values as an input 
# and returns the total number of non-missing rows of the respective factor 
# and the overall percentage of the respective factor:

return.summary.cat <- function(vector, use.str){
  
  # calculate total number of non-missing rows of the factor category
  cat.N <- length( vector[which(vector == use.str )])
  # calculate overall percentage of non-missing rows of the factor category based on the input vector
  cat.perc <- round( cat.N / length(vector) * 100, 2 )
  
  output <- paste0( cat.N, " (", cat.perc, ")")
  return(output)
}

# Using this function to compute the total number of non-missing rows of the `albumin` and `diabetes` categories 
# and the overall percentage of the `albumin` and `diabetes` categories:

per.cat.N <- c( return.summary.cat(cohort.dt$diabetes, 0),
                return.summary.cat(cohort.dt$diabetes, 1),
                return.summary.cat(cohort.dt$albumin, "normo"),
                return.summary.cat(cohort.dt$albumin, "micro"),
                return.summary.cat(cohort.dt$albumin, "macro"))

# Computing the remaining values for the summary table of the categorial variables by hand

cat.missing.N <- format(round(sapply(cohort.dt[, c("diabetes","albumin")], function(z)sum(is.na(z))/length(z)*100),2), nsmall = 2)
cat.total.N <- sapply(cohort.dt[, c("diabetes","albumin")], length)

# Creating the summary table for all non-continuous variables:

summary.cat.variables <- data.table("Variable" = c("diabetes (0)","diabetes(1)","albumin (normo)","albumin (micro)","albumin (macro)"),
                                    "Median (IQR) or N (%)" = per.cat.N,
                                    "Total N" = c(rep(cat.total.N[1],2),rep(cat.total.N[2],3)),
                                    "Missing N (%)" = c(rep(cat.missing.N[1],2),rep(cat.missing.N[2],3)))

summary.cohort.dt <- rbind(summary.con.variables, summary.cat.variables)

