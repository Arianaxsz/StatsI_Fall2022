# PS03_AA

#####################
# load libraries
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("stargazer")
library(stargazer)
# set wd
setwd("~/Documents/GitHub/StatsI_Fall2022")

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


#####################
#Question 1
#####################

# read in incumbers_subset data from incumbets .csv
Incumbets_subset <- read.csv("~/Documents/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv")

#Explore the data 
str(Incumbets_subset)
# run regression model with voteshare and difflog to understand its challenges
regression_model_problem1 <- lm(voteshare ~ difflog, data=Incumbets_subset)
# get summary of model with coefficient estimates 
summary(regression_model_problem1)
#table for the summary with stargazer 
stargazer(regression_model_problem1, type = "html", title = "Summary table")

# Create scatter plot of vote share and difflog with a regression line #Y is a linear function of x 
pdf("problemSets/PS03/my_answer_AA/plot_v&d.pdf", width = 8)
ggplot(data = Incumbets_subset, aes(x = difflog, y = voteshare)) + 
  geom_point(aes(color = voteshare)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Vote share & Difflog")
dev.off()

#Residuals of the model 
deviance(regression_model_problem1)
Inc_residuals <- resid(regression_model_problem1) #Residuals of the model in a separate object

pdf("problemSets/PS03/my_answer_AA/plo_res_v&d.pdf", width = 8)
ggplot(data = NULL, aes(Incumbets_subset$difflog, Inc_residuals)) + 
  geom_point() + 
  geom_smooth()

#Prediction equation 


# - Linear function ; y = α + βx 
# Slope β (beta) and y-intercept α (alpha)
#As the scatterplot suggest that the model y = α + βx is realistic, we use the data to estimate this line. 
Ῠ = a + bx 

#y-intercept (a) estimates the y-intercept α of the model and the slope (b) estimates the slope β. 
# Substituting a particular x-value into a + bx provides a value, denoted by y-bar, that predicts y at that value of x.



#####################
#Question 2
#####################

