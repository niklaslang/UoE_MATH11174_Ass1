### (a) ###

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
# Report odds ratio and 95% CI for the spontaneuous abortions variable
# Perform a likelihood ratio test to compare model M2 to model M1
# and report the p-value for the test.