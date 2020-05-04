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
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS7")
mexico <- read.csv("MexicoMuniData.csv")
install.packages("lme4")
library(lme4)
sleepstudy <- sleepstudy


#####################
# Problem 1: 
#####################
# (a)
p_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data=mexico, family=poisson)
summary(p_model)
# competitive.district
# p = 0.161
# z = -1.402
# p > 0.05, therefore swing district status is not a statiscally reliable predictor of
# the number of visits the winning PAN candidate made in 2006. thus there is no evidence that 
# PAN presidential candidates' visits swing districts more.

# (b)
exp(coef(p_model))
# marginality coef. (exp) = 0.1226841
# marginality coef (non-exp) = -2.0981
# interpetation: holding all other variables constant, a one unit increase in poverty is associated with an 
# average decrease in district visits by a multiplicative factor of 0.1227.

# PAN.governor coef. (exp) = 0.8127638
# PAN.governor coef (non-exp) = -0.2073
# interpretation: holding all other variables constant, a district's having a PAN-affiliated governor 
# is associated with an average decrease in district visits by a multiplicative factor
# of 0.8128.

# (c)
lambda <- exp(-3.9304 - .4594 - .2073)
lambda
# = 0.01008
# the estimated mean number of visits from winning PAN presidential 
# candidates under these conditions is 0.01008 (~ no visits).

#####################
# Problem 2: 
#####################
# (1)
pooled <- lm(Reaction ~ Days, data=sleepstudy)
summary(pooled)
# model: y = 251.405 + 10.467x
par(mfrow=c(2,2)); plot(pooled)
# based on residuals vs. fitted values plot, residuals (variance) appears to be constant
# based on normal Q-Q plot, normality assumption appears to be met as all points are 
# clustered very tightly around the QQ line.

# (2)
unpooled <- lm(Reaction ~ Days + factor(Subject)-1, data=sleepstudy)
summary(unpooled)
##############################################################
#fitted values
#                       Estimate   Std. Error  t value  Pr(>|t|)

# factor(Subject) 308   295.0310    10.4471   28.24   <2e-16
# factor(Subject)309    168.1302    10.4471   16.09   <2e-16
# factor(Subject)310    183.8985    10.4471   17.60   <2e-16
# factor(Subject)330    256.1186    10.4471   24.52   <2e-16
# factor(Subject)331    262.3333    10.4471   25.11   <2e-16
# factor(Subject)332    260.1993    10.4471   24.91   <2e-16
# factor(Subject)333    269.0555    10.4471   25.75   <2e-16
# factor(Subject)334    248.1993    10.4471   23.76   <2e-16
# factor(Subject)335    202.9673    10.4471   19.43   <2e-16
# factor(Subject)337    328.6182    10.4471   31.45   <2e-16
# factor(Subject)349    228.7317    10.4471   21.89   <2e-16
# factor(Subject)350    266.4999    10.4471   25.51   <2e-16
# factor(Subject)351    242.9950    10.4471   23.26   <2e-16
# factor(Subject)352    290.3188    10.4471   27.79   <2e-16
# factor(Subject)369    258.9319    10.4471   24.79   <2e-16
# factor(Subject)370    244.5990    10.4471   23.41   <2e-16
# factor(Subject)371    247.8813    10.4471   23.73   <2e-16
# factor(Subject)372    270.7833    10.4471   25.92   <2e-16
##############################################################

# (3)

unpooled2 <- lm(Reaction ~ Days:factor(Subject)-1, data=sleepstudy)
summary(unpooled2)
##############################################################
#fitted values
# Days:factor(Subject)308   60.321      8.618   7.000
# Days:factor(Subject)309   34.639      8.618   4.019
# Days:factor(Subject)310   38.244      8.618   4.438
# Days:factor(Subject)330   48.748      8.618   5.657
# Days:factor(Subject)331   50.383      8.618   5.846
# Days:factor(Subject)332   51.291      8.618   5.952
# Days:factor(Subject)333   52.566      8.618   6.100
# Days:factor(Subject)334   50.174      8.618   5.822
# Days:factor(Subject)335   38.651      8.618   4.485
# Days:factor(Subject)337   64.832      8.618   7.523
# Days:factor(Subject)349   47.459      8.618   5.507
# Days:factor(Subject)350   55.162      8.618   6.401
# Days:factor(Subject)351   47.667      8.618   5.531
# Days:factor(Subject)352   57.204      8.618   6.638
# Days:factor(Subject)369   51.606      8.618   5.988
# Days:factor(Subject)370   51.285      8.618   5.951
# Days:factor(Subject)371   49.236      8.618   5.713
# Days:factor(Subject)372   53.463      8.618   6.204
##############################################################

# (4)
unpooled3 <- lm(Reaction ~ Days + factor(Subject)-1 + Days:factor(Subject)-1, data=sleepstudy)
summary(unpooled3)
##############################################################
#fitted values
# factor(Subject)308       244.193     15.042  16.234
# factor(Subject)309       205.055     15.042  13.632
# factor(Subject)310       203.484     15.042  13.528
# factor(Subject)330       289.685     15.042  19.259
# factor(Subject)331       285.739     15.042  18.996
# factor(Subject)332       264.252     15.042  17.568
# factor(Subject)333       275.019     15.042  18.284
# factor(Subject)334       240.163     15.042  15.966
# factor(Subject)335       263.035     15.042  17.487
# factor(Subject)337       290.104     15.042  19.287
# factor(Subject)349       215.112     15.042  14.301
# factor(Subject)350       225.835     15.042  15.014
# factor(Subject)351       261.147     15.042  17.362
# factor(Subject)352       276.372     15.042  18.374
# factor(Subject)369       254.968     15.042  16.951
# factor(Subject)370       210.449     15.042  13.991
# factor(Subject)371       253.636     15.042  16.862
# factor(Subject)372       267.045     15.042  17.754
# Days:factor(Subject)309  -19.503      3.985  -4.895
# Days:factor(Subject)310  -15.650      3.985  -3.928
# Days:factor(Subject)330  -18.757      3.985  -4.707
# Days:factor(Subject)331  -16.499      3.985  -4.141
# Days:factor(Subject)332  -12.198      3.985  -3.061
# Days:factor(Subject)333  -12.623      3.985  -3.168
# Days:factor(Subject)334   -9.512      3.985  -2.387
# Days:factor(Subject)335  -24.646      3.985  -6.185
# Days:factor(Subject)337   -2.739      3.985  -0.687
# Days:factor(Subject)349   -8.271      3.985  -2.076
# Days:factor(Subject)350   -2.261      3.985  -0.567
# Days:factor(Subject)351  -15.331      3.985  -3.848
# Days:factor(Subject)352   -8.198      3.985  -2.057
# Days:factor(Subject)369  -10.417      3.985  -2.614
# Days:factor(Subject)370   -3.709      3.985  -0.931
# Days:factor(Subject)371  -12.576      3.985  -3.156
# Days:factor(Subject)372  -10.467      3.985  -2.627
##############################################################

semipool <- lmer(Reaction ~ Days + (1 + Days|Subject), data=sleepstudy)
summary(semipool)
sleepstudy$pooled_new <- fitted(pooled) 
sleepstudy$unpooled_new <- fitted(unpooled)
sleepstudy$semipooled_new <- fitted(semipool)

plot(sleepstudy$Days, sleepstudy$pooled_new)
plot(sleepstudy$Days, sleepstudy$unpooled_new)
plot(sleepstudy$Days, sleepstudy$semipooled_new)
# No, because the semipooled multilevel model appears to be pretty similar to the pooled
# and unpooled models, especially the unpooled model.