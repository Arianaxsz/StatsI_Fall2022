install.packages("tidyverse")
library(tidyverse)

dat <- read.csv("movies.csv") # You need to edit this for your system



## How to select Warner films
# There is more than one Warner Bros. studio
sort(unique(dat$studio))

# The grepl() function can be used to filter character strings based on
# a word within the string.
dat % > % 
  filter(grepl("Warner|WARNER", studio)) % > % 
  summarise(c_score = mean(critics_score))

# We can see that none of the Warner films have won best picture
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_win == "yes") %>%
  select(title)

# But they did get two nominations...
dat %>%
  filter(grepl("Warner|WARNER", studio) & best_pic_nom == "yes") %>%
  select(title, thtr_rel_year)

cor(dat$best_pic_nom, dat$critics_score)

cor(dat$best_pic_nom, dat$critics_score)

# Table for movies
table

x - runtime, imdb rating, imdb num votes, critic score, audience score

y - best pic nom, best pic win, best actor win, best actress win, best director win

x <- table(dat$runtime, dat$imdb_rating, dat$imdb_num_votes, dat$critics_score, dat$audience_score)
y <

#titles for best picture 
sum(dat$best_pic == "yes")
which(dat$best_pic == "yes")
dat$title[c(40, 55, 90, 353, 382, 491, 610)]
