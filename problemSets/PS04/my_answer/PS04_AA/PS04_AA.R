# PS04_AA

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
install.packages(car)
library(car)
data(Prestige)
help(Prestige)

# set wd
setwd("~/Documents/GitHub/StatsI_Fall2022/problemSets/PS04/my_answer")

# clear global .envir


#####################
# Question 1        # 
#####################

# Exercise A 

#View data set to explore the variables names 
View(Prestige)
head(Prestige$type)
#variable type so that professionals are coded as 1, and blue and white collar workers are coded as 0.

Prestige$professionals <- as.factor(ifelse(Prestige$type=="prof", 1, ifelse(Prestige$type=="bc" | Prestige$type=="wc", 0, NA)))

View(professionals)
head(professionals)
table(professionals)
table(Prestige$professionals)

# Exercise B 
# Run a linear model with prestige as an outcome and income, professional, and the interaction of the two as predictors 
#(Note: this is a continuous × dummy interaction.)
#liner model 
md1 <- lm(prestige ~ as.factor(income) + as.factor(professionals),
          data = Prestige,
          na.action = na.omit)
summary(md1)

md2 <- lm(prestige ~ income*professionals, data = Prestige, na.action = na.omit)
summary(md2)

stargazer::stargazer(md2, type = "latex", 
                     title = "Summary table for prestige as an outcome and income, professional, and the interaction of the two as predictors")

# Exercise C - prediction equation 

#Prediction equation  ~ Linear function ; y = α + βx 
# Slope β (beta) and y-intercept α (alpha)
#As the scatterplot suggest that the model y = α + βx is realistic, we use the data to estimate this line. 
# Ῠ = a + bx 
#y-intercept (a) estimates the y-intercept α of the model and the slope (b) estimates the slope β. 
# Substituting a particular x-value into a + bx provides a value, denoted by y-bar, that predicts y at that value of x.

# y = 21.14 + 37.78 - 0.002

# Exercise D - 0.003 increase in income for every professional 37.78 

# Exercise E - increase of 37.78 for professionals every 0.003 change in their income. (?) 

# Exercise F
E(y) = α + β1x1 + β2x2
             
# y = 21.14 + 37.78 x 1,000 - 0.002 x 1 


# Exercise G
E(y) = α + β1x1 + β2x2 +  β3x3
# y = 21.14 + 0.03 x 6000 + 37.78 x 0 - 0.002 x 6000 x 0 


#####################
# Question 2        # 
#####################
# Exercise A
#Hypothesis test for precinct assigned 
#H0 : B2 = 0.042 vs. Ha : B2  =! 0.042 

#Test statistics 
#t =  B2 / se 
0.042/0.016 = 2.625 

#p-value 
df = n - 3 
131 - 3 = 128 

p_values <- 2*pt(abs(2.625) , 128, lower.tail = F)
View(p_values)

# p-value = 0.0097, H0 is not reject since p < a = 0.05


# Exercise B 
#Hypothesis test for precinct adjacent   
#H0 :B2 = 0.042 vs. Ha :B2  =! 0.042 

#Test statistics 
#t =  B2 / se 
0.042/0.013 = 3.230 

#p-value 
df = n - 3 
131 - 3 = 128 

p_values2 <- 2*pt(abs(3.230) , 128, lower.tail = F)
View(p_values2)

# p-value = 0.0015, H0 is not reject since p < a = 0.05


# Exercise C
#The constant is 0.302 and states the proportion of the vote that 
# went to McAuliff's opponent Ken Cuccinelli, which could be interpreted
#as the impact of lawn signs on vote share for exposion to the signs. 

# Exercise D

# The model fit is R2 = 0.094, which seems to have a small value, 
# and high significant when analyzing the importance of yard signs compered with the model and not modeled. 

