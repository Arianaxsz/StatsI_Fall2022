#####################
# load libraries
library(tidyverse)

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
install.packages("ggplot2")
library(ggplot2)
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/StatsI_Fall2022/problemSets/PS01/AA_answersPS01"


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

##2. Next, the school counselor was curious whether the average student IQ in her school is higher than the average IQ score (100) among all the schools in the country. Using the same sample, conduct the appropriate hypothesis test with α = 0.05. 1

country_schools_IQ <- c(100)

#t.test
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

expenditure <- read.table("~/Documents/GitHub/StatsI_Fall2022/datasets/expenditure.txt", header=T)
data("expenditure")
View(expenditure)

#plot(expenditure$Y, expenditure$X1, expenditure$X2, expenditure$X3)
ggplot( data = expenditure, aes(x = , y = x1)) + 
  geom_point()

#Trying with the 4 variables
ggplot(data = expenditure, aes(x = "expenditure$X11", y = "expenditure$Y"))
  geom_point(aes(color = State))

  data.frame("expenditure")
            
  expenditure <- ggplot(expenditure, aes(x = X1, y = Y))
  
  state_plot + 
    geom_line(aes(color = State))  
It is not working! :(
