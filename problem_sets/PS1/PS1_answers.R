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

lapply(c(),  pkgTest)

# set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
z90 <- qnorm((1-.90)/2, lower.tail = FALSE)
n <- length(y)
mean(y)
sample_mean <- mean(y)
sample_sd <- sd(y)
lower_90 <- sample_mean - (z90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (z90 * (sample_sd/sqrt(n)))
confint90  <- c(lower_90, upper_90)
# ANSWER: 90% confidence interval is (94.1, 102.7)
# INTERPRETATION: We are 90% confident that that true average IQ of students at her school lies between 94.1 and 102.7.

#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#null hypothesis: xbar = 100
#alternative hypothesis: xbar > 100
#calculate test statistic and p-value
mean(y)
#mean of sample is 98.44
sd(y)
#sample size 
n <- length(y)
#standard deviation of sample is 13.09
teststata <- (98.44-100)
teststatb <- 13.09/(sqrt(n))
teststatistic <- ((teststata)/(teststatb))
#test statistic = -0.596
# n=25 therefore df = 24
#test statistic = -0.119
pt(abs(-0.596), df=24)
# p-value = 0.7216
# ANSWER/INTERPRETATION: p-value = 0.7216 > 0.05 therefore sample mean is not significantly greater than 100

#####################
# Problem 3
#####################

y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)
length(y)
str(y)
highschool[y==1] <- "Freshman"
highschool[y==2] <- "Sophomore"
highschool[y==3] <- "Junior"
highschool[y==4] <- "Senior"
str(highschool)
highschool[1:10]
highschool
#Recode numerical vector y so that values are assigned to corresponding categories

#####

expenditure <- read.table("expenditure.txt", header=T)
dev.off()
dev.off()

# PLOT 1
plot(expenditure$X1, expenditure$Y, 
     xlab="Per capita personal income", ylab="Per capita expenditure on public ed.")
abline(lm(expenditure$Y ~ expenditure$X1))
#Moderate, linear, positive correlation

# PLOT 2
plot(expenditure$X2, expenditure$Y, 
     xlab="Number of residents per thousand under 18 years of age", ylab="Per capita expenditure on public ed.")
abline(lm(expenditure$Y ~ expenditure$X2))
#Weak negative correlation

# PLOT 3
plot(expenditure$X3, expenditure$Y, 
     xlab="Number of people per thousand residing in urban areas", ylab="Per capita expenditure on public ed.")
abline(lm(expenditure$Y ~ expenditure$X3))
#Weak positive correlation


# PLOT 4
expenditure$fourregions <- NA
expenditure$fourregions <- factor(NA, levels=c("Northeast", "North Central", "South", "West"))
expenditure$fourregions[expenditure$Region==1] <- "Northeast"
expenditure$fourregions[expenditure$Region==2] <- "North Central"
expenditure$fourregions[expenditure$Region==3] <- "South"
expenditure$fourregions[expenditure$Region==4] <- "West"
boxplot(expenditure$Y ~ expenditure$fourregions, 
        xlab= "Region", 
        ylab= "Per capita expenditure on public ed.")

# On average, the Western region has the highest per capita expenditure on public education

# PLOT 5
plot(expenditure$X1, expenditure$Y, 
     xlab="Per capita personal income", ylab="Per capita expenditure on public education")
abline(lm(expenditure$Y ~ expenditure$X1))

# PLOT 6
dev.off()
plot(expenditure$X1, expenditure$Y, 
             xlab="Per capita personal income", ylab="Per capita expenditure on public ed.", col=expenditure$Region, pch=expenditure$Region)
legend(x=1000, y=140, c("Northeast","North Central","South","West"), cex=0.6, col=c("black","red","green","blue"),pch=c(1:4))

     