###############
# Data Analysis
###############

# Packages
library(tidyverse) # load our packages here

# Import data
data <- read.csv("/Users/Ari/StatsI_Fall2022/tutorials/tutorial01/data/height.csv")

# Explore data
summary(data)
str(data)
head(data)

# Visualise
hist(data$height,
     #breaks = 12,
     main = "Histogram of Height",
     xlab = "Height (cm)"
     )

plot(density(data$height),
     main = "Pdf of Height",
     xlab = "Height (cm)"
     )

?hist
?density

# Use a QQ plot to determine if our height variable is
# normally distributed
qqnorm(data$height)
qqline(data$height,
       distribution = qnorm)

?qqnorm

## Confidence Intervals
# Calculate 90 percent confidence intervals using normal distribution
CI_lower <- qnorm(0.05, 
                  mean = mean(data$height), 
                  sd = (sd(data$height)/sqrt(length(data$height))) # the equation for the standard error of the mean
)

CI_upper <- qnorm(0.95,
                  mean = mean(data$height),
                  sd = (sd(data$height)/sqrt(length(data$height)))
)

matrix(c(CI_lower, CI_upper), ncol = 2,
       dimnames = list("",c("Lower", "Upper")))

# Calculate 90 percent confidence intervals using a t distribution
se <- sd(data$height)/sqrt(length(data$height)) # Create an object with our standard error
t_score <- qt(.05, df = length(data$height)-1, lower.tail = FALSE)
CI_lower_t <- mean(data$height) - (se * t_score)
CI_upper_t <- mean(data$height) + (se * t_score)

# Check our working
t.test(data$height, conf.level = 0.9, alternative = "two.sided")

## Hypothesis Testing
# Wrangling our data
class(data$sex) # What class of vector is our sex variable?

as.logical(data$sex) # We can use functions to change the class (here w/o assigning)
as.factor(data$sex)

?factor # Factors have *levels*: if we want to change sex to a factor, we need to also change the levels
data$sex_f <- as.factor(data$sex) # Convert to factor
levels(data$sex_f) <- c("M", "F") # Change the levels
head(data) # Check our result

#understanding the mean (Ari tutorial week 2)
mean(data$height)
#conditioning mean (Ari tutorial week 2) (fun means function) (~ mostly used for regression and means depends on)

aggregate(data$height, by = list(data$sex_f), FUN = mean) # Base R grouping
aggregate(height ~ sex_f, data = data, FUN = mean) # Does the same using a formula

data %>% # Tidyverse method for grouping (%>% "calculation", this function 
  # used mostly by tidyverse and seems to be seemed as easier?)
  group_by(sex_f) %>%
  summarise(mean = mean(height))

# Visualise
boxplot(data$height ~ data$sex_f, # here we use formula notation to group (boxplot a way of showing distribution of continuous variables)
        main = "Boxplot of Height by Sex",
        ylab = "cm",
        xlab = "")
?boxplot



# By looking at the boxplot, what might we conclude?
  ##comments on the visualization: no overlatp between the boxes, skewed M more to the left,  
  #and the medicam between them it is not overlapping so we can deduct there are two different distributions 

# Formulate our null hypothesis:
# What is our null hypothesis? 
    ##The null hypothesis is that there is no different, we can reject the null hypothesis. 

# Where do we set alpha? (alpha= how far accros do we need our mean to be? and setting the probability of error) 
  #(i.e. rejecting null hypothesis when we shouldn't; "means would be a type 1 error"
# note that we don't get to decide beta - i.e. the probability of incorrectly 
# accepting the null hypothesis)
# Is a one-tail or two-tail test more appropriate?

# Test our hypothesis
# First - are our variances equal? (Don't worry too much about this now...)
var.test(height ~ sex_f,
         data = data,
         ratio = 1,
         alternative = "two.sided",
         conf.level = 0.95)

# Now we run our t test
t.test(height ~ sex_f, # Use formula to group
       data = data,
       mu = 0, # The default (null) is zero {ari notes: this is the mean}
       var.equal = FALSE, # The default is FALSE; if data has the same varience we use TRUE, we can use "Var.test()" to check if the varience is the same
       #Ari notes: less varience between M and F and we can use the function to check the varience 
       alternative = "greater", # Try changing this to "two.sided"
       conf.level = .95) # Try changing this critical value

# Now we run our t test
t.test(height ~ sex_f, # Use formula to group
       data = data,
       mu = 0, # The default (null) is zero {ari notes: this is the mean}
       var.equal = FALSE, # The default is FALSE; if data has the same varience we use TRUE, we can use "Var.test()" to check if the varience is the same
       #Ari notes: less varience between M and F and we can use the function to check the varience 
       alternative = "two.sided", # Try changing this to "two.sided"
       conf.level = .95) # Try changing this critical value

#the two sided equals to 18.668169 other than the "Inf" for infinit. 
# How do we interpret the output? (Hint: looking back at the boxplot can help)
