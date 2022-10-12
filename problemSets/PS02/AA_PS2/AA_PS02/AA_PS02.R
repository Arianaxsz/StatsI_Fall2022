####PS2 Stats 
library(ggplot2)
library(tidyverse)
install.packages("olsrr")
library(olsrr)
### Question 1 

##(a) x-saured value

#Df = 3 - 1 = 2
#critical value = 5.990

Upperclass <- c(14, 6, 7)
Lowerclass <- c(7, 7, 1)

sum((Upperclass - Lowerclass)**2/Upperclass)

#Manually chi.square value  
((7 - 14)^2)/14 + ((7 - 6)^2)/6 + ((1 - 7)^2)/7


# > [1] 8.809524

#p value 
8.809524 
n = 3 

03 < p < .01  


chisq.test(Lowerclass, p = Upperclass)

#X-squared  = 6.257143 

ci <- read_excel("~/Downloads/Ps02_Corruption and Inequality at the Crossroad.xlsx")

chisq.test(Ps02_Corruption_and_Inequality_at_the_Crossroad)

## (b)p-value from the test
#df = 2
#critical value 0.1 = 4.605
#square  6.257143 ; .05 < p < .03 
#we conclude that the p value is bigger than 0.1 so we can 
#reject the null hypothesis 

## (c) 

chi <- chisq.test(table(Upperclass, Lowerclass))
chi
residuals <- chi$residuals 
#Add the answer in latex 


View(residuals)


### Questions 2

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")


mean(women$reserved)
sd(women$reserved)

dataset <- table (women$reserved, women$water)
dataset

mean(dataset)
sd(dataset)

t.test(dataset, mu = 4)

tail(dataset)

##(B) bivariate regression 

ggplot(data = women, aes(x = reserved, y = water)) + 
         geom_point() + 
         geom_abline()

#br 
ols_regress(reserved ~ water, data = women)

#the value is significant ! 


##(c) Interpret the coefficient estimate for reservation policy.

this is really tiring! 
  
  

