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

M2 <- glm(case ~ age + parity + spontaneous, data = infert.dt, family="binomial")

# odds ratio and 95% CI for the spontaneous abortions variable
M2.abortions <- exp(coef(M2)[4])
CI.M2.abortions <- exp(confint(M2)[4, ])
round(c(M2.abortions, CI.M2.abortions),2)

# Reporting a p-value for the likelihood ratio test compare model M2 to model M1
pval <- pchisq(M1$deviance - M2$deviance, df=1, lower.tail=FALSE)
signif(pval, 2)
