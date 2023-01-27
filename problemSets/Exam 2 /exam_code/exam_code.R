# Exam 2 

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
setwd("~/Documents/GitHub/StatsI_Fall2022/problemSets/Exam2/exam_code")

#####################
# Question 1        # 
#####################

# a) It seems that the estimate is high correlated between GDP and democracy, due to the small difference. And, the variation in FDI when a country GDP is increasing in one unit (1$) the FDI investment is -3 increase in GDP and countries democracy -2.99. 
    # the different is very small, 0.1, but the 

# b) 95% Confidence interval, a = 5 = 0.025 and 0.025 
 # N = 1000
 # E GDP = -3
 # SE GDP =  0.00013 
 # CI - mean +- 1.96SE 
#Calculate the 95 % confidence interval for the student IQ 
# Step 1: get t score
# 95% C.I 1.96x0.00013 

n <- 1000
c.estimate <- -3
gdp.se <- 0.00013 
t <-  qt(0.025, n - 1, lower.tail=F)
mean.n <- mean(n)
cbind(CIlower = mean(n) - 1.96 * -0.00013/ 1000, CIupper = mean(n) + 1.96 * -0.00013  / 1000)


-3 + 1.96*(0.00013)
-3 - 1.96*(0.00013)
[-2.999745, -3.000255]

# c) Calculate the difference in predicted FDI between low and high values of Education 
#    for non-democratic countries holding GDP constant at its sample mean.
Sample.mean <- -3
gdp.mean <-  24860.42 
sd.education <- 10.96 ~ 13.02 

x <- rbinom(10.96, 13.02)

var(x)

#####################
# Question 2        # 
#####################


# a) Maybe because it could cause bias to different countries, cultures and  factors associated with inequalities, for example GDP and 
#    standards of living varying between what wealth is and how are those counted. 
#    We could access a bigger study with divided countries and it's estimated, and change the monetary value by GDP. 

#####################
# Question 3        # 
#####################

# a) Correlation between the estimated coefficients as intercept is 0.25(.090), 
#    relates to increase of every 0.73 in well depth, and -1.01 distance of 100k to the closest commercial factory. 


# b) Yes, it vary. Additive affects, since the same effect for each well depth. 
#    The appropriate test should be reporting the effect sizes and confidence intervals, since these would 
#    convey the relative importance and magnitude of the effect. 

# c) Compute the average difference
deep_well <- 1 
dist100 <- c(0.37, 2.11)

avg <- list(deep_well, dist100, deep_well * 2, dist100 * 3)
avg

cat("After finding the average of each element of the list", "\n")
sapply(avg, mean)
# > sapply(avg, mean)
# [1] 1.00 1.24 2.00 3.72 

#####################
# Question 4        # 
#####################

# a) write out the fitted model for a ram lamb using the estimate coefficient

E(y) = -18.1368 + 2.2980 - 4.0716	

# b) What is the predicted Fatness index of a ewe lamb that weights 10kg? 

# c) Write out the fitted model for a ram lamb using the estimated coefficients.

#####################
# Question 5        # 
#####################

# a) Partial F-test : Describes how statistic significant is the regression/model, and test if "variable" is useful in the model at all.
#    The number further from 1 is actually better, but we can check the p-value on the side to check it as well. 
#    Larger F values give stronger evidence against Ho. 

# b) Residuals: Also known as the prediction errors, in an observation, the difference between an observed valye and the 
#    predicted value of the response variable, y-y(bar), is called the residual. 

# c) Categorical data/dummy variables: Data that can take only a specific set of values representing a set of possible categories, 
#    and dummy variables are used to avoid multicollinearity when doing a regression model . 
#    For example we can re code factor data to be used in the regression  

# d)  


#####################
# Question 6        # 
#####################

# a) 2 
# b) True 
# c) 3
# d) 4 

