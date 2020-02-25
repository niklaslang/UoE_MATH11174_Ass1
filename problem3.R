### (a) ###

# load R6 library
library(R6)
library(data.table)

# Writing an R6 class `Calcegfr` that accepts 4 vectors:
# serum creatinine (`scr`), age (`age`), sex (`sex`) and ethnicity (`ethnic`)
# The initialisation method of the class validates that vector lengths match
# and stores them for use by the methods. 
# The class then implements two separate public methods of the two eGFR formulas 
# storing the result as a vector of the same length as the input

Calcegfr <- R6Class("Calcegfr", public = list(
  scr = vector(),
  age = vector(),
  sex = factor(),
  ethnic = factor(),
  mdrd4.egfr = vector(),
  ckdepi.egfr = vector(),
  
  # initalisation method
  initialize = function(scr, age, sex, ethnic){
    
    # input data checks
    
    # check whether vector lengths match
    stopifnot(length(scr) == length(age) &&
              length(scr) == length(sex) &&
              length(scr) == length(ethnic))
    
    # check whether scr and age are numeric
    stopifnot(is.numeric(scr) && is.numeric(age))
    
    # check whether sex and ethnic are factors 
    stopifnot(is.factor(sex) && is.factor(ethnic))
    
    # initialise member variables
    
    self$scr <- scr
    self$age <- age
    self$sex <- sex
    self$ethnic <- ethnic
    
    self$mdrd4.egfr <- numeric(length = length(self$scr))
    self$ckdepi.egfr <- numeric(length = length(self$scr))
    
  }, 
  
  # estimating GFR with MDRD4 equation
  
  egfr.mdrd4 = function(){
    scr <- self$scr
    age <- self$age
    sex <- self$sex
    ethnic <- self$ethnic
    
    # converting levels of factor variables
    
    sex <- ifelse( sex == "Female", 0.742, 1)
    ethnic <- ifelse( ethnic == "Black", 1.212, 1)
    
    # main body
    
    self$mdrd4.egfr <- 175*(scr/88.42)^(-1.154)*age^(-0.203)*sex*ethnic
    
    # return
    return(self$mdrd4.egfr)
  },
  
  # estimating GFR with CKD-EPI equation
  
  egfr.ckdepi = function(){
    scr <- self$scr
    age <- self$age
    sex <- self$sex
    ethnic <- self$ethnic
    
    # calculating alpha and kappa
    
    alpha <- ifelse( sex == "Female", -0.329, -0.411)
    kappa <- ifelse( sex == "Female", 0.7, 0.9)
    
    # converting levels of factor variables

    sex <- ifelse( sex == "Female", 1.018, 1)
    ethnic <- ifelse( ethnic == "Black", 1.159, 1)
  
    # main body
    
    self$ckdepi.egfr <- 141*pmin((scr/88.42)/kappa, 1)^alpha*pmax((scr/88.42)/kappa, 1)^(-1.209)*0.993^(age)*sex*ethnic
    
    # return results
    return(self$ckdepi.egfr)
  }
    
))

### (b) ###

# load data set

scr2 <- read.csv("assignment1_data/scr2.csv", stringsAsFactors = TRUE)

# instantiate an Calcegfr object
scr2.calcegfr <- Calcegfr$new(scr2$scr, scr2$age, scr2$sex, scr2$ethnic)

# compute the eGFR according to the two equations
mdrd4.egfr <- scr2.calcegfr$egfr.mdrd4()
sum(is.na(mdrd4.egfr))
ckdepi.egfr <- scr2.calcegfr$egfr.ckdepi()
sum(is.na(ckdepi.egfr))

# mean and SD of MDRD4 results
round(mean(mdrd4.egfr, na.rm = TRUE),2)
round(sd(mdrd4.egfr, na.rm = TRUE),2)

# mean and SD of CKD-EPI results
round(mean(ckdepi.egfr, na.rm = TRUE),2)
round(sd(ckdepi.egfr, na.rm = TRUE),2)

# Pearson correlation coefficient between MDRD4 results and CKD-EPI results
cor(mdrd4.egfr, ckdepi.egfr, method = "pearson", use = "complete.obs")

# Reporting the same quantities according to strata of MDRD4 eGFR: 0-60, 60-90 and > 90:

# create eGFR bins
egfr.bins <- c(0, 60, 90, Inf)

# subset MDRD results according to bins
mdrd4.egfr.low <- mdrd4.egfr[which(cut(mdrd4.egfr, egfr.bins) == "(0,60]")]
mdrd4.egfr.med <- mdrd4.egfr[which(cut(mdrd4.egfr, egfr.bins) == "(60,90]")]
mdrd4.egfr.high <- mdrd4.egfr[which(cut(mdrd4.egfr, egfr.bins) == "(90,Inf]")]

# calculate mean and SD for all 3 bins
low.mean <- round(mean(mdrd4.egfr.low, na.rm = TRUE),2)
low.SD <- round(sd(mdrd4.egfr.low, na.rm = TRUE),2)

med.mean <- round(mean(mdrd4.egfr.med, na.rm = TRUE),2)
med.SD <- round(sd(mdrd4.egfr.med, na.rm = TRUE),2)

high.mean <- round(mean(mdrd4.egfr.high, na.rm = TRUE),2)
high.SD <- round(sd(mdrd4.egfr.high, na.rm = TRUE),2)

# create sumamry table of MDRD4 eGFR

mdrd4.summary.dt <- data.table("eGFR" = c("(0,60]","(60,90]", "(90,Inf]"),
                               "N" = c(length(mdrd4.egfr.low), length(mdrd4.egfr.med), length(mdrd4.egfr.high)),
                               "Mean" = c(low.mean, med.mean, high.mean),
                               "SD" = c(low.SD, med.SD, high.SD))

mdrd4.summary.dt

### (c) ###

# Scatterplot of the MDRD4 and CKD-EPI eGFR

plot(mdrd4.egfr, ckdepi.egfr,
     cex = .5, 
     col='orange',
     main = paste("Q-Q Plot eGFR equations", 
                  "\nPearson's r: ", round(cor(mdrd4.egfr, ckdepi.egfr, method = "pearson", use = "complete.obs"),2)),
     xlab = "MDRD4",
     ylab = "CKD-EPI")

# add vertical lines corresponding to median, first and third quartiles of MDRD4
abline(v = median(mdrd4.egfr, na.rm = TRUE))
abline(v = quantile(mdrd4.egfr, na.rm = TRUE)[2])
abline(v = quantile(mdrd4.egfr, na.rm = TRUE)[4])

# add horizontal lines corresponding to median, first and third quartiles of CKD-EPI
abline(h = median(ckdepi.egfr, na.rm = TRUE))
abline(h = quantile(ckdepi.egfr, na.rm = TRUE)[2])
abline(h = quantile(ckdepi.egfr, na.rm = TRUE)[4])

# two separate plots for or eGFR values greater and lower than 90mL/min/1.73m2 

par(mfrow=c(1,2))

plot(mdrd4.egfr[which(mdrd4.egfr < 90)], ckdepi.egfr[which(mdrd4.egfr < 90)],
     cex = .5, 
     col='orange',
     main = paste("Q-Q Plot eGFR equations", 
                  "\nPearson's r: ", round(cor(mdrd4.egfr[which(mdrd4.egfr < 90)], ckdepi.egfr[which(mdrd4.egfr < 90)], method = "pearson", use = "complete.obs"),2)),
     xlab = "MDRD4",
     ylab = "CKD-EPI")

plot(mdrd4.egfr[which(mdrd4.egfr > 90)], ckdepi.egfr[which(mdrd4.egfr > 90)],
     cex = .5, 
     col='orange',
     main = paste("Q-Q Plot eGFR equations", 
                  "\nPearson's r: ", round(cor(mdrd4.egfr[which(mdrd4.egfr > 90)], ckdepi.egfr[which(mdrd4.egfr > 90)], method = "pearson", use = "complete.obs"),2)),
     xlab = "MDRD4",
     ylab = "CKD-EPI")

#sliding.cor <- numeric(321)
#for (i in 1:321){
#  sliding.cor[i] <- cor(mdrd4.egfr)[i:(i+80)], ckdepi.egfr[i:(i+80)], method = "pearson", use = "complete.obs")
#}

#par(mfrow=c(1,1))
#plot(seq(1,321), sliding.cor,
#     type = "l",
#     col='orange',
#     main = "Pearson's r between MDRD4 and CKD-EPI eGFR\nin sliding windows of SCr values",
#     xlab = "SCr sliding window",
#     ylab = "Pearson's r")

