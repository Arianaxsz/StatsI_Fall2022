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
setwd("~/Documents/GitHub/StatsI_Fall2022/problemSets/Ps03/my_answer_AA/PS03_AA")

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
stargazer(regression_model_problem1, type = "latex", title = "Summary table for Difflog & Vote share")

# Create scatter plot of vote share and difflog with a regression line #Y is a linear function of x 
png(filename = "plot_1.png",width = 1000, height = 500)
ggplot(data = Incumbets_subset, aes(x = difflog, y = voteshare)) + 
  geom_point(aes(color = voteshare)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Vote share & Difflog")
dev.off()

#Residuals of the model 
deviance(regression_model_problem1)
Inc_residuals <- resid(regression_model_problem1) #Residuals of the model in a separate object

#Residuals plot 
png(filename = "plot_res_v&d.png", width = 1000, height = 500)
ggplot(data = NULL, aes(Incumbets_subset$difflog, Inc_residuals)) + 
  geom_point() + 
  geom_smooth()

#Prediction equation  ~ Linear function ; y = α + βx 
# Slope β (beta) and y-intercept α (alpha)
#As the scatterplot suggest that the model y = α + βx is realistic, we use the data to estimate this line. 
Ῠ = a + bx 

#y-intercept (a) estimates the y-intercept α of the model and the slope (b) estimates the slope β. 
# Substituting a particular x-value into a + bx provides a value, denoted by y-bar, that predicts y at that value of x.

# ŷ=0.579031+0.041666x 

#####################
#Question 2
#####################

# run regression model with presvote and difflog to understand its challenges
regression_model_problem2 <- lm(presvote ~ difflog, data=Incumbets_subset)
# get summary of model with coefficient estimates 
summary(regression_model_problem2)
#table for the summary with stargazer 
stargazer(regression_model_problem2, type = "latex", title = "Summary table for Difflog & Presvote")

# Create scatter plot of presvote and difflog 
png(filename = "plot_2.png",
    width = 600,
    height = 350)

ggplot(data = Incumbets_subset, aes(x = difflog, y = presvote)) + 
  geom_point(aes(color = presvote)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Presvote & Difflog")
dev.off()

#Residual
Inc_residuals2 <- resid(regression_model_problem2) #Residuals of the model in a separate object

#Prediction Equation on Latex 

#####################
#Question 3
#####################

#Regression x voteshare and y presvote 
regression_model_problem3 <- lm(voteshare ~ presvote, data=Incumbets_subset)
# get summary of model with coefficient estimates 
summary(regression_model_problem3)
#table for the summary with stargazer 
stargazer(regression_model_problem3, type = "latex", title = "Summary table for Voteshare and Presvote")

#Scatterplot of the two variables 
ggplot(data = Incumbets_subset, aes(x = presvote, y = voteshare)) + 
  geom_point(aes(color = voteshare)) + 
  geom_smooth(method = "lm", se = FALSE) 
dev.off()

png("plot_3.png", width = 600, height = 350)

#Residuals 
Inc_residuals3 <- resid(regression_model_problem3) #Residuals of the model in a separate object


#####################
#Question 4
#####################

#Run a regression where the outcome variable is the residuals from Question 1 
#and the explanatory variable is the residuals from Question 2.
#x is residuals 1 and y is residuals 2 

regression_model_problem4 <- lm(Inc_residuals ~ Inc_residuals2)
# get summary of model 
summary(regression_model_problem4)
#table for the summary with stargazer 
stargazer(regression_model_problem4, type = "latex", title = "Summary table for Residuals")

#Scatterplot 
png("plot_4.png", width = 600, height = 350)
ggplot(data = Incumbets_subset, aes(x = Inc_residuals2, y = Inc_residuals)) + 
  geom_point(aes(color = Inc_residuals2)) + 
  geom_smooth(method = "lm", se = FALSE) 
dev.off()

#Equation in latex

#####################
#Question 5
#####################

#. Run a regression where the 
#outcome variable is the incumbent’s voteshare X
#and the explanatory variables are difflog and presvote. Y

regression_model_problem5 <- lm(voteshare ~ difflog + presvote, data=Incumbets_subset)
summary(regression_model_problem5)
stargazer(regression_model_problem5, type = "latex", title = "Summary table for 
          voteshare, difflog and presvote")