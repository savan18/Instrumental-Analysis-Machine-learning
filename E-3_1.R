# Instrumental Variables example
# install.packages("sem")
library(sem)
wages<-read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
iv.results<-tsls(lwage ~ educ + age + married + black, ~ feduc + age + married + black, data=wages)
# Run an instrumental variables regression. Note that father's education is
# used for an instrument, and is not in the first set of variables. Age,
# marital, status, and race are assumed to be less related to underlying
# ability and so are controlled for in the "first stage" regression too.

ols.results<-lm(lwage ~ educ + age + married + black, data=wages)
# Run the analogous OLS regression without father's education

print(summary(ols.results))
print(summary(iv.results))
# Note that the point estimate for education has now gone up. This suggests
# that people who have higher education in a way that's correlated with
# their father having higher education, even holding fixed age, marital
# status, and race, receive ~9% higher wages for every year of education
# Also note that the standard error on education is higher for the iv than
# ols results. The reason for this is that in IV, we're using only some of
# the variation in education: only the part related to father's education,
# so it's like there's less variation overall

# Stepwise regression and tree example
install.packages(c('MASS','sem'))
library(MASS)
start.model<-lm(wage ~ hours + IQ + KWW + educ + exper + age + married + black + south + urban + sibs, data=wages)
# Give an initial model, which will be the most coefficients we'd want to ever use
# remove tenur from the linear regressing because 
summary(start.model)
stepwise.model<- step(start.model)
# The command "step" adds and subtracts coefficients to maximize a measure of
# goodness of fit, by default AIC
summary(stepwise.model)

# install.packages('tree')
library(tree)
wage.tree <- tree(wage ~ married + hours + IQ + KWW + educ + tenure + exper, method="anova", data=wages)
# fit a tree
summary(wage.tree)
# Print the results

plot(wage.tree)
text(wage.tree)
# Plot the tree, and add the text decisions

cross.validation <- cv.tree(wage.tree)
# perform cross-validation, 10-fold. 

cross.validation
# look for the size with the lowest "dev" or deviance
# Why might this be different than the fitted tree?

pruned.wage.tree<-prune.tree(wage.tree,best=5)
# Prune this tree down to 5 terminal nodes, just to show this is how you would
# do it. Here this makes it worse though
summary(pruned.wage.tree)

lmfit<- lm(wage ~ married + hours + IQ + KWW + educ + tenure + exper, data=wages)
# Compute a regression using those same variables:
anova(lmfit)
summary(wage.tree)
# Notice that the mean sum of squared residuals was 130895 using the linear
# model, compared to the residual mean deviance of 130000 using trees. So the
# tree method has less residual error, which means it's a better predictor.
# This is especially striking given that the tree only uses KWW, educ, IQ,
# While the regression needs substantial contributions from tenure, experience,
# and marital status too.

predict(wage.tree, newdata=data.frame(IQ=100, KWW=50, educ=16, married=1, hours=40, tenure=3, exper=2))
predict(lmfit, newdata=data.frame(IQ=100, KWW=50, educ=16, married=1, hours=40, tenure=3, exper=2))
# These predictors give different estimates for the expected wage of a
# particular individual, relatively young, married, well educated, with good
# knowledge of the world of work. Which would you trust and why?
