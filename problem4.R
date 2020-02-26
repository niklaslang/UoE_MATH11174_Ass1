### (a) ###

library(data.table)
infert.dt <- data.table(infert, stringsAsFactors = TRUE)

# Fit a logistic regression model (M1) to predict secondary infertility 
# using age and parity as predictors

M1 <- glm(case ~ age + parity, data = infert.dt, family="binomial")

# Use the deviance to judge the goodness of fit of the model 
# and report a p-value (3 significant digits).

# compute the deviance of M1
M1$deviance

# compare the difference of deviances of M1 and M0 and report a p-value
signif(pchisq(M1$null.deviance - M1$deviance, df=1, lower.tail=FALSE), 3)

### (b) ###

# Fit a second model (M2) by adding the number of spontaneous abortions 
# to the set of predictors used in model M1

M2 <- glm(case ~ age + parity + spontaneous, data = infert.dt, family="binomial")

# odds ratio and 95% CI for the spontaneous abortions variable
M2.abortions <- exp(coef(M2)[4])
CI.M2.abortions <- exp(confint(M2)[4, ])
round(c(M2.abortions, CI.M2.abortions),2)

# Reporting a p-value for the likelihood ratio test compare model M2 to model M1
pval <- pchisq(M1$deviance - M2$deviance, df=1, lower.tail=FALSE)
signif(pval, 2)

### (c) ###

# Implementing a function that computes the binomial log-likelihood

loglik.binom <- function(y.obs, y.pred){
  
  # check input data
  stopifnot(length(y.obs) == length(y.pred))
  
  # use y.obs to split y.pred into cases and controls
  cases <- y.pred[which(y.obs == 1)]
  controls <- y.pred[which(y.obs == 0)]
  
  # compute log likelihood
  loglik <- sum(log(cases)) + sum(log(1-controls))
  
  # return log likelihood
  return(loglik)
}

#  y.obs is a vector of observed outcomes 
# (with values either 0 or 1 to represent controls and cases, respectively)
obs <- infert.dt$case # extract observations from infert data set

# y.pred is a vector of fitted probabilities learnt from a logistic regression model
preds <- predict(M2, newdata = infert.dt, type = "response") # obtain predicted probabilities with predict() function

# Using function loglik.binom() to compute deviance and null deviance for model M2

# calculating deviance for M2
# deviance is minus two times the log likelihood of a model
M2.dev <- -2*loglik.binom(obs, preds)
M2.dev

# check result
round(M2.dev, 2) == round(M2$deviance, 2)

# calculating null deviance for M2
# use the same observations as before 
null.obs <- obs
# use the same predicted probabilities - corresponding to the proportion of cases - for all observations
null.preds <- rep(sum( obs == 1)/length(obs), length((obs)))

# deviance is minus two times the log likelihood of a model
M2.null.dev <- -2*loglik.binom(null.obs, null.preds)
M2.null.dev

# check result
round(M2.null.dev, 2) == round(M2$null.deviance, 2)

### (d) ###

# Using functions glm.cv() and predict.cv() from Lab 3,
# performing 10-folds cross- validation for model M2
# (set the random seed to 1 before creating the folds)

# glm.cv function
glm.cv <- function(formula, data, folds) {
  regr.cv <- NULL
  for (fold in 1:length(folds)){
    regr.cv[[fold]] <- glm(formula, data=data[-folds[[fold]], ], family="binomial")
  }
  return(regr.cv)
}

# predict.cv function
predict.cv <- function(regr.cv, data, outcome, folds){
  pred.cv <- NULL
  for (fold in 1:length(folds)) {
    test.idx <- folds[[fold]]
    pred.cv[[fold]] <- data.frame(obs = outcome[test.idx], 
                                  pred = predict(regr.cv[[fold]],newdata=data,type="response")[test.idx])
  }
  return(pred.cv)
}

# load library
library(caret)

# set random seed to 1
set.seed(1)

# creating 10 folds
k <- 10
folds <- createFolds(infert.dt$case, k)

# fitting a logistic regression model in each of the folds
M2.cv <- glm.cv(case ~ age + parity + spontaneous, infert.dt, folds)

# validating M2 across the 10 folds
M2.cv.pred <- predict.cv(M2.cv, infert.dt, infert.dt$case, folds)

# Evaluating the predictive performance of model M2, 
# using function loglik.binom() to compute the log-likelihood 
# of the predicted probabilities for each test fold

loglik <- numeric(k)

for (fold in 1:k){
  obs <- M2.cv.pred[[fold]]$obs
  pred <- M2.cv.pred[[fold]]$pred
  loglik[fold] <- loglik.binom(obs, pred)
  cat("Test fold ", fold, ": log-likelihood of the predicted probabilities: \t", loglik[fold],"\n")
}

# Reporting the sum of the test log-likelihoods over all folds

sum(loglik)
