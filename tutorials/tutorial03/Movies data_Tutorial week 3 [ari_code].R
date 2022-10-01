#Practical example 

#Using control flow in data wrangling 

#reading our data from our online github 
dat <- read.csv("https://raw.githubusercontent.com/Arianaxsz/StatsI_Fall2022/main/datasets/movies.csv") 

#Check the structure 
library(tidyverse)
str(dat)
head(dat)
View(dat)


#Gather all unique values 

#1. Use control flow to find the number of unique observations in each colunm
unique_obs <- vector("double", length = ncol(dat))

for (i in 1:ncol(dat)) { 
  unique_obs[i] <- length(unique(dat[,i]))
}

unique_obs


#2. Use unique_obs to subset dat, returning unique observations for columns with fewer than 12 unique
#observations 

#how to subset -> the coma is important as it states to columns and not the rows 
dat[,unique_obs < 12]
names(dat[, unique_obs < 12])

#saving this vector 
unique_obs_12 <- dat[,unique_obs < 12] 
list(unique_obs_12)

#To initialize list we need to create a vector as it has a certain length !! 

unique_obs_12 <- vector("list", length = sum(unique_obs < 12))

#giving vectors name and attributes 
names(unique_obs_12) <- names(dat[,unique_obs < 12])

#3. populate list with unique obs of cols fewer than 12 
#unique obs 
for (i in 1: sum(unique_obs < 12)) {
  unique_obs_12[i] <- unique(dat[, unique_obs < 12][i])
  
}
unique_obs_12


dbl_vec[1]
