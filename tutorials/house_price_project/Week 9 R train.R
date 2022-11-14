library(tidyverse)
library(broom)
library(ggplot2)
install.packages("stargaze")
library(stargazer)

# download the dat 

dat <- readRDS("data/train.rds")

#Linear Models 

mod1 <- lm(SalePrice ~ AdjSalePrice, data = dat)

mod2 <- lm(AdjSalePrice ~ SqFtLot, data = dat)

mod3 <- lm(AdjSalePrice ~ SqFtTotLiving, data = dat)

mod4 <- lm(AdjSalePrice ~ SqFtFinBasement, data = dat)

m1 <- stargazer(mod1, type = "latex", title = "Summary")

m2 <- stargazer(mod3, type = "text", title = "Summary 2")

#Bedrooms and bathrooms 


#Visual SalePrice and AdjSalePrice
ggplot(data = dat, aes(x = SalePrice, y = AdjSalePrice, group = as.factor(PropertyType))) + 
  geom_point(aes(color = as.factor(PropertyType))) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sale price")

#Visual 2 

ggplot(data = dat, aes(x = SqFtTotLiving, y = AdjSalePrice, group = as.factor(PropertyType))) + 
  geom_point(aes(color = as.factor(PropertyType))) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sale price")

#Predict/Augment 
stargazer(mod1,mod3)
dat_add <- augment(mod1, mod3)


m4 <- stargazer(mod1, mod3, type = "html", title = "Summary for both variables")




