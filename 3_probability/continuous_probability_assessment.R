
####### Question 1  ####### 
x <- seq(-4, 4, length.out = 100)
qplot(x, f, geom = "line", data = data.frame(x, f = dnorm(x)))


####### Questions 1 and 2: ACT scores, part 1  ####### 
# For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a 
# mean of 20.9 and standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore 
# this detail and use continuous values instead.)
# First we'll simulate an ACT test score dataset and answer some questions about it.
# Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 and 
# standard deviation of 5.7. Save these values as act_scores. You'll be using this dataset throughout these 
# four multi-part questions.

####### Question 1  ####### 
# What is the mean of act_scores?
m <- 20.9
sd <- 5.7
set.seed(16)
act_scores <- rnorm(10000,m,sd)
mean(act_scores)

# What is the standard deviation of act_scores?
sd(act_scores)
 
# A perfect score is 36 or greater (the maximum reported score is 36).
# In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

# In act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores > 30)

# In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores <= 10)

####### Question 2  ####### 
# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value 
# of the probability density function over x given a mean of 20.9 and standard 
# deviation of 5.7; save the result as f_x. Plot x against f_x.
# Which of the following plots is correct?
x <- seq(1,36,1)
f_x <- dnorm(x,m,sd)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

####### Questions 3 and 4: ACT scores, part 2  ####### 
# Convert act_scores to Z-scores. Recall from Data Visualization (the second course in 
# this series) that to standardize values (convert values into Z-scores, that is, values 
# distributed with a mean of 0 and standard deviation of 1), you must subtract the
# mean and then divide by the standard deviation. Use the mean and standard deviation
# of act_scores, not the original values used to generate random test scores.

####### Question 3  ####### 
# What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
z <- (act_scores - mean(act_scores)) / sd(act_scores)
mean(z > 2)

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
mean(act_scores) + 2*sd(act_scores)

# A Z-score of 2 corresponds roughly to the 97.5th percentile.
# Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and 
# standard deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?
qnorm(0.975,mean(act_scores),sd(act_scores))

####### Question 4  ####### 
# In this 4-part question, you will write a function to create a CDF for ACT scores.
# Write a function that takes a value and produces the probability of an ACT score less than or 
# equal to that value (the CDF). Apply this function to the range 1 to 36.
# What is the minimum integer score such that the probability of that score or lower is at least .95?
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x) # x from 1 to 36. cdf returns x where p(act_scores <= x) >= a certain p e.g. 0.95
})
min(which(cdf >= .95))

# Use qnorm() to determine the expected 95th percentile, the value for which the probability of 
# receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
# What is the expected 95th percentile of ACT scores?
qnorm(0.95,m,sd)

# As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles 
# from the data.
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th 
# percentiles of the act_scores data. Save these as sample_quantiles.
# In what percentile is a score of 26? Your answer should be an integer (i.e. 60), not a percent or 
# fraction. Note that a score between the 98th and 99th percentile should be considered the 98th 
# percentile, for example, and that quantile numbers are used as names for the vector sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

# Make a corresponding set of theoretical quantiles using qnorm() over the interval 
# p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. 
# Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
# Which of the following graphs is correct?
theoretical_quantiles <- qnorm(p,m,sd)
qplot(theoretical_quantiles,sample_quantiles)+
  geom_abline()