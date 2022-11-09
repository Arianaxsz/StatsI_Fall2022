library(tidyverse)
library(broom)
library(ggplot2)

# download the dat 

dat <- readRDS("data/train.rds")

#Linear Models 

mod1 <- lm( ~ Avg_Cont_Grants + Gender, data = salary)
