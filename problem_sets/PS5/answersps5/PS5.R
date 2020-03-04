#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
install.packages(car)
library(car)
library(boot)
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS5")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)
summary(model1)

# a. Residuals vs. Fitted Values
par(mfrow=c(2,2)); plot(model1)
plot(model1, which=1)
# after fitted value of y hat equals ~ 45, variance seems to increase/residuals deviate more from 0. variance does not appear to be constant.

# b. Q-Q plot of studentized residuals
plot(model1, which=2)
# the normality assumption appears to be met because almost all points are very tightly clustered along the QQ line.

# c. Plot hat values
plot(hatvalues(model1), pch=16, cex=1, ylab="Hat values", main="Hat value plot", ylim=c(0,0.4))
abline(h=(2*5)/47, lty=2)
abline(h=(3*5)/47,lty=2)
identify(1:47, hatvalues(model1), row.names(gamble))
# using the average hat value as a threshold (thresholds = 2 * hbar and 3 * hbar), there are 4 high leverage points in this dataset: observations 31, 33, 35, and 42.
# these four points have are potentially influential i.e. have the potential to greatly affect the regression model.

# d. Outlier test
outlierTest(model1)
# according to outlier test, observation 24 has an adjusted (Bonferroni) p-value ~ 0. therefore, observation 24 is an extreme residual in this model.

# e. Bubble plot
plot(hatvalues(model1), rstudent(model1), ylim=c(-3,7.5), type="n")
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1), cex=10*cook/max(cook))
abline(h=c(-2,0,2), lty=2)
abline(v=c(2,3)*5/47, lty=2)
identify(hatvalues(model1), rstudent(model1), row.names(gamble))
# observations 31, 33, 35, and 42 are influential points because all 4 are above at least one threshold for studentized residuals and hat values. This means all 4 points are both high leverage and are regression outliers, making them influential points (they greatly affect the regression model).Observation 24 was also identified because while it did not pass any of the hat value thresholds, it has an unusally large Cook's distance.