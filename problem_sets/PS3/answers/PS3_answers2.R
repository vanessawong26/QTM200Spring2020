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
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr"),  pkgTest)

# set working directory, import datasets
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS3")
incumbents <- read.csv("incumbents_subset.csv")

#####################
# Problem 1: 
#####################
# x = difflog (explanatory)
# y = voteshare (outcome)

# run a regression where the outcome variable is voteshare and the explanatory variable is difflog.
ymean <- mean(incumbents$voteshare)
xmean <- mean(incumbents$difflog)
ysum <- sum(incumbents$voteshare)
xsum <- sum(incumbents$difflog)
yy <- (incumbents$voteshare) - (ymean)
xx <- (incumbents$difflog) - (xmean)
yyxxsum <- sum(yy*xx)
xxsq <- (xx)^2
sumxxsq <- sum(xxsq)
betaincumbs <- yyxxsum/sumxxsq
betaincumbs
# beta = 0.0417
alphaincumbs <- ymean - (betaincumbs*xmean)
alphaincumbs
# alpha = 0.5790
# linear model: y = 0.5790 + 0.0417x
# check work
incumbreg <- lm(incumbents$voteshare ~ incumbents$difflog)
incumbreg

# make a scatterplot of the two variables and add the regression line
plot(incumbents$difflog, incumbents$voteshare,
     xlab="Difference between incumbent and challenger campaign spending", ylab="Incumbent vote share")
abline(a=0.5790, b=0.0417, col="red")

# save the residuals of the model in a separate object
residscheck <- residuals(incumbreg)
residscheck

# write the prediction equation


#####################
# Problem 2: 
#####################
# x = difflog (explanatory)
# y = presvote (outcome)

# run a regression where the outcome variable is presvote and the explanatory variable is difflog
y2mean <- mean(incumbents$presvote)
y2sum <- sum(incumbents$presvote)
yy2 <- (incumbents$presvote) - y2mean
yyxxsum2 <- sum(xx*yy2)
betaincumbs2 <- yyxxsum2/sumxxsq
betaincumbs2
# beta = 0.0238
alphaincumbs2 <- y2mean - (betaincumbs2*xmean)
alphaincumbs2
# alpha = 0.5076
# linear model: y = 0.5076 + 0.0238x
# check work
incumbreg2 <- lm(incumbents$presvote ~ incumbents$difflog)
incumbreg2

# make a scatterplot of the two variables and add the regression line
plot(incumbents$difflog, incumbents$presvote, 
     xlab="Difference between incumbent and challenger campaign spending", ylab="Vote share")
abline(a=0.5076, b=0.0238, col="red")

#save the residuals of the model in a separate object
resids2 <- residuals(incumbreg2)
resids2

# write the prediction equation

#####################
# Problem 3: 
#####################
# x = presvote (explanatory)
# y = voteshare (outcome)

# run a regression where the outcome variable is voteshare and the explanatory variable is presvote
xmean3 <- mean(incumbents$presvote)
ymean3 <- mean(incumbents$voteshare)
xsum3 <- sum(incumbents$presvote)
ysum3 <- sum(incumbents$voteshare)
yy3 <- (incumbents$voteshare) - (ymean3)
xx3 <- (incumbents$presvote) - (xmean3)
yyxxsum3 <- sum(yy3*xx3)
xxsq3 <- (xx3)^2
sumxxsq3 <- sum(xxsq3)
beta3 <- yyxxsum3/sumxxsq3
beta3
# beta = 0.3880
alpha3 <- ymean3 - (beta3*xmean3)
alpha3
# alpha = 0.4413
# linear model: y= 0.4413 + 0.3880x
# check work
reg3 <- lm(incumbents$voteshare~incumbents$presvote)
reg3

# make a scatterplot of the two variables and add the regression line
plot(incumbents$presvote, incumbents$voteshare,
     xlab="presvote", ylab="voteshare")
abline(a=0.4413, b=0.3880, col="red")

# write the prediction equation

#####################
# Problem 4: 
#####################
# x = resids2 (explanatory)
# y = residscheck (outcome)

# run a regression where the outcome variable is the residuals from Q1 and the explantory variable is the residuals from Q2
xmean4 <- mean(resids2)
ymean4 <- mean(residscheck)
xsum4 <- sum(resids2)
ysum4 <- sum(residscheck)
yy4 <- residscheck - (ymean4)
xx4 <- resids2 - (xmean4)
yyxxsum4 <- sum(yy4*xx4)
xxsq4 <- (xx4)^2
sumxxsq4 <- sum(xxsq4)
beta4 <- yyxxsum4/sumxxsq4
beta4
# beta = 0.2569
alpha4 <- ymean4 - (beta4*xmean4)
alpha4
# alpha = -4.939 x 10^(-18)
# linear model: y = -4.939 x 10^(-18) + 0.2569x
# check work
reg4 <- lm(residscheck ~ resids2)
reg4
summary(reg4)

# make a scatterplot of the two reisduals and add the regression line
plot(resids2, residscheck, 
     xlab="Q2 Residuals", ylab="Q1 Residuals")
abline(reg4, col="red")

# write the prediction equation

#####################
# Problem 5: 
#####################
# x = difflog, presvote (explanatory)
# y = voteshare (outcome)

# run a regression where the outcome variable is the incumbent's voteshare and the explanatory variables are difflog and presvote
lmbyhand <- function(inputDF, covariates, outcome){
  X2 <- as.matrix(cbind(rep(1, dim[inputDF][1]), inputDF[,covariates]))
  Y2 <- inputDF[,outcome]
  betas2 <- solve((t(X2)%*%X2)) %*% (t(X2)%*%Y2)
  rownames(betas2)[1] <- "Intercept"
  n <- dim(inputDF)[1]
  k <- ncol(X2)}
lmbyhand(incumbents, c("difflog", "presvote"), "voteshare")
reg_results
# check work 
lm(incumbents$voteshare ~ incumbents$difflog + incumbents$presvote)$coefficients
multireg2 <- lm(incumbents$voteshare ~ incumbents$difflog + incumbents$presvote)
summary(multireg2)
# linear model: y = 0.4486 + 0.0355xdifflog + 0.2569xpresvote

# write the prediction equation

# what is it in this output that is identical to the output in Q4? why do you think this is the case?
# the slope of the regression of voteshare on (presvote and difflog) is the same as the slope of of the regression of Q1 residuals on Q2 residuals.
# i think this is the case because the regression of Q1 residuals on Q2 residuals tells us how much variation in presvote 
# *and* voteshare is not explained by difflog. 

# the relationship between variation in presvote/voteshare that is not explained by difflog. 


