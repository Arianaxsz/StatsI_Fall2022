################################
#Applied Stats/Quant Methods 1 #
#      Problem Set 2           #
################################


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
 

n <- length(Lowerclass + Upperclass)


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
#reject the null hypothesis as we are not confident that the upper class is different
#than lower class, it does not tell how much is the difference.  

?qt
pvalue <- qt(0.1, n-1, lower.tail = F)

#> p-value : 1.88

## (c) 

chi <- chisq.test(table(Upperclass, Lowerclass))
chi
ls(chi)
residuals <- chi$residuals 
chi$residuals

## (d)
plot(Lowerclass, Upperclass) 
plot(residuals)

lm <- lm(Lowerclass ~ Upperclass)
plot(lm, las = 1) 

ggsave("Residuals plot")


# When you compare the cells, the standardized residual makes it easy to see which cells are contributing the most to the value, 
#and which are contributing the least. 
#If the residual is less than -2, the cell’s observed frequency is less than the expected frequency.
#Greater than 2 and the observed frequency is greater than the expected frequency.



### Questions 2

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")


mean <- mean(women$reserved)
sd <- sd(women$reserved)

as.logical(women$reserved)
as.factor(women$reserved)
women$reserved <- as.factor(women$reserved) # Convert to factor
levels(women$reserved) <- c("City 1 ", "City 2") 

dataset <- table (women$reserved, women$water)


mean(dataset)
sd(dataset)

t.test(dataset, mu = 4)

t.test(reserved ~ water, 
       data = women,
       mu = 4, 
       var.equal = FALSE, 
       alternative = "two.sided",
       conf.level = .95)

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
  
  

