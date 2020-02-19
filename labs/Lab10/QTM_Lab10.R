#####################################################
## File: Lab10.R                                   ##
## Multiple Regression in R                        ##
#####################################################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
# set wd
setwd('~/Documents/GitHub/QTM200Spring2020/labs/Lab10')

# load libraries
pkgTest <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

lapply(c("faraway"), pkgTest)


#######################
# Multiple Regression
#######################


# load dataset from Faraway package
data(sat)
?sat

# Estimate the effect of "expend" on "takers" 
# using a 95% level of significance
sat1 <- lm(takers ~ expend, data=sat)
summary(sat1)

# What if we control for other factors?
sat2 <- lm(takers ~ expend + ratio + salary, data=sat)
summary(sat2)

# How do you interpret the coefficient for salary?


# What if we include all the variables in the data?
sat3 <- lm(takers ~ ., data=sat)
summary(sat3)
# total is NA because it is verbal + math
