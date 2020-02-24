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
install.packages(car)
library(car)
data(Prestige)
help(Prestige)


# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr"),  pkgTest)

# set working directory, import datasets
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS4")


#####################
# Problem 1: 
#####################
Prestige$type
Prestige$profession <- ifelse(profession == "prof", 1, 0)
Prestige$profession
#####################
# y = prestige (outcome variable)
# x = income, professional, interaction 
reg1b <- lm(prestige ~ income + profession + income:profession, data=Prestige)
summary(reg1b)
#####################
# prediction equation: y = 21.1422 + 0.00317xincome + 37.7813xprofession - 0.00233xincomexprofessionx
#####################
# For blue and white collar workers, a one unit increase in income (one dollar increase) is associated with a 0.00317 unit increase in prestige.
#####################
# On average, holding all other variables constant at their means, professionals have 37.813 more units of prestige than blue and white collar workers.
#####################
# Professional coded as 1
# expected prestige score = (21.1422 + 37.7813) + (0.00317 + (-0.00233))x
upper <- (21.1422 + 37.7813) + ((0.00317 + (-0.00233))*1000)
lower <- (21.1422 + 37.7813)
upper - lower
# expected change in prestige score for professional occupations as a result of a $1,000 increase in income is 0.84.
#####################
# Professional coded as 1
# Non-professional coded as 0
# prediction equation: y = 21.1422 + 0.00317xincome + 37.7813xprofession - 0.00233xincomexprofessionx
ynonprof <- 21.1422 + (0.00317*6000)
yprof <- 21.1422 + (0.00317*6000) + (37.7813*1) - (0.00233*6000*1)
yprof - ynonprof
# the expected change in prestige score for an individual whose income is $6,000 and is changing from a non-professional to a professional occupation is 23.8013.
#####################

#####################
# Problem 2: 
#####################
ts <- (0.042)/(0.016)
2*pt(ts, df=29, lower.tail=FALSE)
# p = 0.0137 < 0.05. Therefore, we reject the null hypothesis that having yard signs in a precinct does not affect vote share.
#####################
ts2 <- (0.042)/(0.013)
2*pt(ts, df=75, lower.tail=FALSE)
# p = 0.0105 < 0.05. Therefore, we reject the null hypothesis that being next to precincts with yard signs does not affect vote share.
#####################
#In precincts that are not assigned to have yard signs posted and are not adjacent to precincts with yard signs, Ken Cuccinelli's predicted vote share is 0.302.
#####################
# 9.4% of the variation in vote share is explained by yard signs in a precint. This is a relatively low percentage, indicating that the yard signs have little explanatory power relative to other factors that are not modeled -- 90.6% of the variation in Cucinelli's voteshare is explained by other factors.

