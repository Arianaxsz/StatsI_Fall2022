################################
#Applied Stats/Quant Methods 1 #
#      Problem Set 2           #
################################


library(ggplot2)
library(tidyverse)
install.packages("olsrr")
library(olsrr)
### Question 1 

#(a) x-saured value

#Df = 3 - 1 = 2
#critical value = 5.990

Upperclass <- c(14, 6, 7)
Lowerclass <- c(7, 7, 1)


sum((Upperclass - Lowerclass)**2/Upperclass)

#Manually chi.square value  
((7 - 14)^2)/14 + ((7 - 6)^2)/6 + ((1 - 7)^2)/7
# > [1] 8.809524

n <- length(Lowerclass + Upperclass)

pvalue <-03 < p < .01  

chisq.test(Lowerclass, p = Upperclass)

#X-squared  = 6.257143 

table_ps02 <- read_excel("~/Downloads/table ps02.xlsx")


((7 - 14)^2)/14 + ((7 - 6)^2)/6 + ((1 - 7)^2)/7

chisq.test(table_ps02)

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

# a) state a null hypothesis and alternative hypothesis
# b) bivariate regression

pbar = 124/5745 #sample proportion 
p0 = .5 #hypothesis value 
n = 26082 #sample size 

z = (pbar - p0)/sqrt(p0*(1 - p0)/n) #test statistic 
z # [1] -154.5276

#critcal valye at .05 significant level

alpha = .05 
z.half.alpha = qnorm(1 - alpha/2) 
c(- z.half.alpha, z.half.alpha) 

# [1] -1.959964  1.959964

t.test(women$reserved, women$water, mu = ,)

t.test(reserved ~ water, 
       data = women,
       mu = 2, 
       var.equal = FALSE, 
       alternative = "two.sided",
       conf.level = .95)

t.test(women$water, conf.level = 0.5, alternative = "two.sided")

boxplot(women$reserved, women$water)

# p value 
pval = 2* pnorm(-154.5, lower.tail=FALSE)  # upper tail 
pval  


rb <- lm(water ~ reserved, data = women)
summary(rb)

#(c) Interpret the coefficient estimate for reservation policy.

lm(women$female ~ women$water)
?coefficients

coef(rb)
# (Intercept)    reserved 
#  14.738318    9.252423 


