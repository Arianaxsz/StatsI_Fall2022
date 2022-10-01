#Problem Set 1 - Ariana Antunes - R code 
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

install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
lapply(c(),  pkgTest)

#####################
# load libraries
library(tidyverse)
library(ggplot2)
# set wd
setwd("~/Documents/GitHub/StatsI_Fall2022/problemSets/PS01")

# clear global .envir


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# capture the number of observations
n <- length(y)

#probably there was no need for this > 
data.frame(y)

# Explore data
summary(y)
str(y)
head(y)

#1. Find a 90% confidence interval for the average student IQ in the school.
CI_lower <- qnorm(0.05, 
                  mean = mean(y), 
                  sd = (sd(y)/sqrt(length(y))) # the equation for the standard error of the mean
)
CI_upper <- qnorm(0.95,
                  mean = mean(y),
                  sd = (sd(y)/sqrt(length(y)))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))

mean(y)

#Average of IQ in the school is 98.44 


## part 2 of question 1
#the average student IQ in her school is higher than the average IQ score (100) among all the schools in the country. 
##Using the same sample, conduct the appropriate hypothesis test with α = 0.05. 1

country_schools_IQ <- c(100)

#t.test ~ hypothesis test 
t.test(y, 
       country_schools_IQ = country_schools_IQ,
       mu = 0, 
       var.equal = FALSE, 
       alternative = "two.sided", 
       conf.level = 0.05) 

#2. answer is 98.44 


#####################
# Problem 2
#####################

#adding the data 'expenditure'
expenditure <- read.table("~/Documents/GitHub/StatsI_Fall2022/datasets/expenditure.txt", header=T)
#viewing data 
summary(expenditure)
View(expenditure)
class(expenditure)

#Correlation between (expenditure$Y, expenditure$X1, expenditure$X2, expenditure$X3)
?pairs
#First part Y+X!
ggplot(data = expenditure, aes(x=expenditure$X1, y=expenditure$Y)) + 
  geom_point(aes(color = Y, X1)) 
png(filename = "scatterplot_Y&X1.png",
    width = 600,
    height = 350)

#Second part Y+X2
ggplot(data = expenditure, aes(x=expenditure$X2, y=expenditure$Y)) + 
  geom_point(aes(color = Y, X2)) 
#Third part Y+X3 
ggplot(data = expenditure, aes(x=expenditure$X3, y=expenditure$Y)) + 
  geom_point(aes(color = Y, X3)) 

#Trying pairs function for full visibility
pairs(~ Y + X1 + X2 + X3, data = expenditure, rowlattop = FALSE, bg = "blue",
      main = "Correlation between Y,X1,X2 & X3")
#saving 
png(filename = "correlation_expenditure_plot.png",
    width = 600,
    height = 350)


##### -> the correlation seems to be much similar in the variables comparison

#Plot 2 between Y and Region 

ggplot(data = expenditure, aes(x=Region, y=Y)) + 
  geom_col(aes(fill = Y)) + labs(title = "Correlation expenditure assistance & Region")

##  On average west region has the highest per capita expenditure on housing assistance. 


#Correlation between Y,X1 and Region 
ggplot(data = expenditure, aes(x=X1, y=Y)) + 
  geom_point(aes(color=Region,
                 shape=as.factor(Region)))
#Saving plot 
png(filename = "correlation_YX!Region.png",
    width = 600,
    height = 350)


#the end 
