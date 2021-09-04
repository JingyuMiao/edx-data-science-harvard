
# There are 12 multi-part problems in this comprehensive assessment that review concepts
# from the entire course. The problems are split over 3 pages. Make sure you read the
# instructions carefully and run all pre-exercise code.

# Overview
# In June 2016, the United Kingdom (UK) held a referendum to determine whether the country
# would "Remain" in the European Union (EU) or "Leave" the EU. This referendum is commonly
# known as Brexit. Although the media and others interpreted poll results as forecasting
# "Remain" (p>0.5), the actual proportion that voted "Remain" was only 48.1% (p=0.481)
# and the UK thus voted to leave the EU. Pollsters in the UK were criticized for
# overestimating support for "Remain".

# Data Import
# Import the brexit_polls polling data from the dslabs package and set options for the analysis:

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

# Final Brexit parameters
# Define p = 0.481 as the actual percent voting "Remain" on the Brexit referendum and
# d = 2p-1 = -0.038 as the actual spread of the Brexit referendum with "Remain"
# defined as the positive outcome:
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

####### Question 1: Expected value and standard error of a poll #######

# The final proportion of voters choosing "Remain" was p = 0.481. Consider a poll with a sample of
# N = 1500 voters.
# What is the expected total number of voters in the sample choosing "Remain"?
N <- 1500
N * p

# What is the standard error of the total number of voters in the sample choosing "Remain"?
sqrt( p*(1-p) / N) * N

# What is the expected value of x_hat, the proportion of "Remain" voters?
p

# What is the standard error of x_hat, the proportion of "Remain" voters?
sqrt( p*(1-p) / N)

# What is the expected value of d, the spread between the proportion of "Remain" voters and "Leave" voters?
d

# What is the standard error of d, the spread between the proportion of "Remain" voters and "Leave" voters?
2 * sqrt( p*(1-p) / N)




####### Question 2: Actual Brexit poll estimates #######
# Load and inspect the brexit_polls dataset from dslabs, which contains actual polling data for the 6 months
# before the Brexit vote. Raw proportions of voters preferring "Remain", "Leave", and "Undecided" are
# available (remain, leave, undecided) The spread is also available (spread), which is the difference in
# the raw proportion of voters choosing "Remain" and the raw proportion choosing "Leave".

# Calculate x_hat for each poll, the estimate of the proportion of voters choosing "Remain" on the
# referendum day (P=0.481), given the observed spread and the relationship . Use mutate(d=2*x_hat-1) to add a variable
# x_hat to the brexit_polls object by filling in the skeleton code below:

head(brexit_polls)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

# What is the average of the observed spreads (spread)?
mean(brexit_polls$spread)

# What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)

# What is the average of x_hat, the estimates of the parameter ?
mean(brexit_polls$x_hat)

# What is the standard deviation of x_hat?
sd(brexit_polls$x_hat)

####### Question 3: Confidence interval of a Brexit poll #######
# Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
brexit_polls[1,]

# Use qnorm() to compute the 95% confidence interval for x_hat.
brexit_polls[1,]$x_hat + c(-1,1) * qnorm(0.975)*sqrt(brexit_polls[1,]$x_hat*(1-brexit_polls[1,]$x_hat)/brexit_polls[1,]$samplesize)


####### Question 4: Confidence intervals for polls in June #######
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

# Create the data frame june_polls containing only Brexit polls ending in June 2016 (enddate of "2016-06-01" and later).
# We will calculate confidence intervals for all polls and determine how many cover the true value of d.

# First, use mutate() to calculate a plug-in estimate se_x_hat for the standard error of the estimate for each poll
# given its sample size and value of x_hat. Second, use mutate() to calculate an estimate for the standard error
# of the spread for each poll given the value of se_x_hat. Then, use mutate() to calculate upper and lower bounds for
# 95% confidence intervals of the spread. Last, add a column hit that indicates whether the confidence interval for
# each poll covers the correct spread d=-0.038.
d <--0.038

library("dplyr")

june_polls <-brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat * (1-x_hat) / samplesize),
         se_spread = 2*se_x_hat,
         lower = spread - qnorm(0.975) * se_spread,
         upper = spread + qnorm(0.975) * se_spread,
         hit = if_else(-0.038 >= lower & -0.038 <= upper, TRUE, FALSE),
         hit0 = if_else(0 >= lower & 0 <= upper, TRUE, FALSE))

# How many polls are in june_polls?
nrow(june_polls)

# What proportion of polls have a confidence interval that covers the value 0?
mean(june_polls$hit0)

# What proportion of polls predict "Remain" (confidence interval entirely above 0)?
mean(june_polls$lower > 0)

# What proportion of polls have a confidence interval covering the true value of d?
mean(june_polls$hit)


####### Question 5: Hit rate by pollster #######
# Group and summarize the june_polls object by pollster to find the proportion of
# hits for each pollster and the number of polls per pollster. Use arrange() to
# sort by hit rate.
june_polls %>% group_by(pollster) %>%
  summarise(hit_rate = mean(hit), polls = n()) %>%
  arrange(hit_rate)

####### Question 6: Boxplot of Brexit polls by poll type #######
# Make a boxplot of the spread in june_polls by poll type.
june_polls %>%
  ggplot(aes(x=poll_type, y=spread)) +
  geom_boxplot()

####### Question 7: Combined spread across poll type #######
# Calculate the confidence intervals of the spread combined across all polls in june_polls,
# grouping by poll type. Recall that to determine the standard error of the spread, you will
# need to double the standard error of the estimate.

# Use this code (which determines the total sample size per poll type, gives each spread
# estimate a weight based on the poll's sample size, and adds an estimate of p from the
# combined spread) to begin your analysis:
combined_by_type <- june_polls %>%
        group_by(poll_type) %>%
        summarize(N = sum(samplesize),
                  spread = sum(spread*samplesize)/N,
                  p_hat = (spread + 1)/2)

# What is the lower bound of the 95% confidence interval for online voters?
combined_by_type %>%
  mutate(se_p_hat = sqrt(p_hat*(1-p_hat)/N),
         se_spread = 2*se_p_hat,
         lower = spread - qnorm(0.975)*se_spread,
         upper = spread + qnorm(0.975)*se_spread)


####### Question 8: Interpreting combined spread estimates across poll type #######
# Interpret the confidence intervals for the combined spreads for each poll type calculated in
# the previous problem.


####### Question 9: Chi-squared p-value #######

# Define brexit_hit, with the following code, which computes the confidence intervals for all
# Brexit polls in 2016 and then calculates whether the confidence interval covers the actual
# value of the spread d=-0.038:

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

# Use brexit_hit to make a two-by-two table of poll type and hit status. Then use the chisq.test()
# function to perform a chi-squared test to determine whether the difference in hit rate is
# significant.
two_by_two <- table(brexit_hit$hit, brexit_hit$poll_type)

# What is the p-value of the chi-squared test comparing the hit rate of online and telephone polls?
chisq.test(two_by_two)$p.value

# Determine which poll type has a higher probability of producing a confidence interval that covers
# the correct value of the spread. Also determine whether this difference is statistically significant
# at a p-value cutoff of 0.05. Which of the following is true?
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))

####### Question 10: Odds ratio of online and telephone poll hit rate #######
# Use the two-by-two table constructed in the previous exercise to calculate the odds ratio between the
# hit rate of online and telephone polls to determine the magnitude of the difference in performance
# between the poll types.

# Calculate the odds that an online poll generates a confidence interval that covers the actual value
# of the spread.
chisq_df <- as.data.frame(two_by_two)

online_true <- chisq_df$Freq[chisq_df$Var1 == "TRUE" & chisq_df$Var2 == "Online"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "FALSE" & chisq_df$Var2 == "Online"]
online_odds <- online_true/online_false
online_odds

# Calculate the odds that a telephone poll generates a confidence interval that covers the actual value of the spread.
phone_true <- chisq_df$Freq[chisq_df$Var1 == "TRUE" & chisq_df$Var2 == "Telephone"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "FALSE" & chisq_df$Var2 == "Telephone"]
phone_odds <- phone_true/phone_false
phone_odds

# Calculate the odds ratio to determine how many times larger the odds are for online polls to hit versus telephone polls.
odds_online/odds_telephone

####### Question 11: Plotting spread over time #######
# Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored by poll type (poll_type).
# Use geom_smooth() with method = "loess" to plot smooth curves with a span of 0.4. Include the individual
# data points colored by poll type. Add a horizontal line indicating the final value of d=-0.038.
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))

####### Question 12: Plotting raw percentages over time #######
# Use the following code to create the object brexit_long, which has a column vote containing the three possible
# votes on a Brexit poll ("remain", "leave", "undecided") and a column proportion containing the raw proportion
# choosing that vote option on the given poll:
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

# Make a graph of proportion over time colored by vote. Add a smooth trendline with geom_smooth() and method =
#   "loess" with a span of 0.3.
brexit_long %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3)
