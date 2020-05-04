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
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS6")
chol <- read.csv("cholesterol.csv")
gdp <- read.csv("gdpChange.csv")

#####################
# Problem 1: 
#####################

reg_logit <- glm(cholCat ~ sex + fat, data=chol, family=binomial(link="logit"))
summary(reg_logit)
# prediction equation: y = -4.759 + 1.357sex + 0.066fat

rlnull <- glm(cholCat ~ 1, data=chol, family=binomial(link="logit"))
summary(rlnull)
anova(rlnull, reg_logit, test="Chisq")
# global null: beta_fat = beta_sex = 0 
# p < 2.2e-16 
# p = < 0.01 therefore at least one beta is not equal to 0 

###############################################################
# summary output:
# Deviance Residuals: 
# Min        1Q    Median        3Q       Max  
# -2.89662  -0.73093   0.07127   0.64186   2.23806  

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.759162   0.563834  -8.441   <2e-16 ***
#  sex          1.356750   0.552130   2.457    0.014 *  
#  fat          0.065729   0.007826   8.399   <2e-16 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 435.54  on 314  degrees of freedom
# Residual deviance: 279.58  on 312  degrees of freedom
# AIC: 285.58
# Number of Fisher Scoring iterations: 5
###############################################################

# a
# For women, increasing fat intake by one gram is associated with an increase in the 
# odds of being in the high cholesterol group by a multiplicative factor of 1.068 (e^0.0657)

# b
# For men, increasing fat intake by one gram is associated with an increase in the 
# odds of being in the high cholesterol group by a multiplicative factor of 1.068 (e^0.0657)

# c 
# B0 + B1X
(-4.759 + (0.066 * 100))
# = 1.841
1/(1+exp(-1.841))
# = 0.863
# The estimated probability of a woman with a fat intake of 100 grams per day being 
# in the high cholesterol group is 86.3%

# d
reg_logit <- glm(cholCat ~ sex + fat, data=chol, family=binomial(link="logit"))
reg_logit2 <- glm(cholCat ~ sex * fat, data=chol, family=binomial(link="logit"))
anova(reg_logit, reg_logit2, test="Chisq")
# p = 0.6454 > 0.05. There is no evidence that including an interactive term of sex and fat intake
# would be a significant predictor of the odds of being in the high cholesterol group. Therefore,
# the answers to 2a and 2b would likely not change if the interactive term were included.

#####################
# Problem 2: 
#####################
require(nnet)
# 1
gdp$GDPWdiff_new[gdp$GDPWdiff=="no change"] <- 0
gdp$GDPWdiff_new[gdp$GDPWdiff=="positive"] <- 1
gdp$GDPWdiff_new[gdp$GDPWdiff=="negative"] <- 2
gdp_reg <- multinom(GDPWdiff_new ~ REG + OIL, data=gdp)
summary(gdp_reg)
exp(coef(gdp_reg)[,c(1:3)])

# In a given country, there is an increase in baseline odds of a 
# positive GDP difference by 5.865 times when that country has a democratic 
# government

# In a given country, there is an increase in baseline odds of a 
# negative GDP difference by 3.972 times when that country has a democratic 
# government

# In a given country, there is an increase in baseline odds of a 
# positive GDP difference by 97.156 times when that country has a ratio
# of fuel to total exports greater than 50% in 1984-86

# In a given country, there is an increase in baseline odds of a 
# negative GDP difference by 119.577 times when that country has a ratio
# of fuel to total exports greater than 50% in 1984-86

###############################################################
# cutoff points, coefficients:
# multinom(formula = GDPWdiff_new ~ REG + OIL, data = gdp)

# Coefficients:
#       (Intercept)   REG      OIL
# 1 (+)   4.533759 1.769007 4.576321
# 2 (-)   3.805370 1.379282 4.783968

# Std. Errors:
#         (Intercept)  REG      OIL
# 1 (+)   0.2692006 0.7670366 6.885097
# 2 (-)  0.2706832 0.7686958 6.885366

# Residual Deviance: 4678.77 
# AIC: 4690.77 
###############################################################
#> exp(coef(gdp_reg)[,c(1:3)])
#    (Intercept)    REG       OIL
# 1 (+)   93.10789 5.865024  97.15632
# 2 (-)   44.94186 3.972047 119.57794
###############################################################



# 2 
ordered_logit <- polr(GDPWdiff ~ REG + OIL, data=gdp, Hess=T)
summary(ordered_logit)
ordered_logit
exp(0.3984834)
exp(-0.1987177)
# coefficients (exp)
# REG : 1.489564
# OIL : 0.8197

exp(-0.7311784)
exp(-0.7104851)
# intercepts (exp)
# negative|no change    no change|positive 
#     0.4813414             0.4914058 

# The odds of having a positive GDP change when switching from a non-democratic
# to a democratic government multiply by a factor of 1.489. 

# The odds of having a positive GDP change, if that country has an average ratio of 
# fuel:total exports that is greater than 50%, multiplies by a factor
# of 0.8197.