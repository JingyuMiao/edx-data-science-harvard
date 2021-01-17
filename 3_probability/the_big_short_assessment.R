
# review and assess the following concepts:
# Expected value and standard error of a single draw of a random variable
# Expected value and standard error of the sum of draws of a random variable
# Monte Carlo simulation of the sum of draws of a random variable
# The Central Limit Theorem approximation of the sum of draws of a random variable
# Using z-scores to calculate values related to the normal distribution and normal random variables
# Calculating interest/premium rates to minimize chance of losing money
# Determining a number of loans/policies required to profit
# Simulating the effects of a change in event probability


# Expected values of a random variable: a*p + b*(1- p)
# Expected value of the sum of n draws of a random variable: n * (a*p + b*(1-p))
# Standard deviation of an urn with two values: abs(b - a) * sqrt(p*(1 - p))
# Standard error of the sum of n draws of a random variable: sqrt(n) * abs(b - a) * sqrt(p*(1 - p))

library(tidyverse)
library(dslabs)

data(death_prob)
head(death_prob)
options(digits = 3)


####### Questions 1 and 2: Insurance rates, part 1  ####### 

# An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within 
# one year. The premium (annual cost) for this policy for a 50 year old female is $1,150. Suppose that in the event 
# of a claim, the company forfeits the premium and loses a total of $150,000, and if there is no claim the company 
# gains the premium amount of $1,150. The company plans to sell 1,000 policies to this demographic.

# Use death_prob to determine the death probability of a 50 year old female, p.
p <- death_prob %>% filter(age == 50, sex == 'Female') %>%  pull(prob)
p

# The loss in the event of the policy holder's death is -$150,000 and the gain if the policy holder remains alive is 
# the premium $1,150.
# What is the expected value of the company's net profit on one policy for a 50 year old female?
a <- -150000
b <- 1150
n <- 1000
a*p + b*(1- p)

# Calculate the standard error of the profit on one policy for a 50 year old female.
se <- abs(b - a) * sqrt(p*(1 - p))

# What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
e_n <- n * (a*p + b*(1-p))

# What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
se_n <- sqrt(n) * abs(b - a) * sqrt(p*(1 - p))

# Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(0, mean = e_n, sd = se_n)


s <- sample(c(a,b),n,prob=c(p,1-p),replace=TRUE)

# Use death_prob to determine the probability of death within one year for a 50 year old male.
p <- death_prob %>% filter(age == 50, sex == 'Male') %>%  pull(prob)
p

# Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies 
# to be $700,000. Use the formula for expected value of the sum of draws with the following values and solve for 
# the premium 𝑏:
# E[𝑆]=𝜇𝑆=700000
# 𝑛=1000
# 𝑝=death probability of age 50 male
# 𝑎=150000
# 𝑏=premium

# E[S] = n * (ap + b(1-p))
# b = ((E[S]/n) - ap)/(1-p)
e <- 700000
n <- 1000
a <- -150000
b <- (E/n- a*p) / (1-p)
b

# Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
se_n <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))

# What is the probability of losing money on a series of 1,000 policies to 50 year old males? Use the Central Limit Theorem.
e_n <- n*(a*p+b*(1-p))
pnorm(0, mean = e_n, sd = se_n)


####### Questions 3 and 4: insurance rates, part 2  ####### 

# Life insurance rates are calculated using mortality statistics from the recent past. They are priced such 
# that companies are almost assured to profit as long as the probability of death remains similar. If an event
# occurs that changes the probability of death in a given age group, the company risks significant losses.

# In this 6-part question, we'll look at a scenario in which a lethal pandemic disease increases the probability
# of death within 1 year for a 50 year old to .015. Unable to predict the outbreak, the company has sold 1,000 
# $150,000 life insurance policies for $1,150.

# What is the expected value of the company's profits over 1,000 policies?
n <- 1000
p <- 0.015
a <- -150000
b <- 1150
e_n <- n*(a*p+b*(1-p))

# What is the standard error of the expected value of the company's profits over 1,000 policies?
se_n <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))

# What is the probability of the company losing money?
pnorm(0, mean = e_n, sd = se_n)

# Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go 
# out of business.
# What is the probability of losing more than $1 million?
pnorm(-1000000, mean = e_n, sd = se_n)

# Investigate death probabilities p <- seq(.01, .03, .001).
# What is the lowest death probability for which the chance of losing money exceeds 90%?
p <- seq(.01, .03, .001)
a <- -150000
b <- 1150
n <- 1000

p_lose <- sapply(p, function(p){
  e_n <- n*(a*p+b*(1-p))
  se_n <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, mean = e_n, sd = se_n)
})

data.frame(p, p_lose) %>% 
  filter(p_lose > 0.9) %>% 
  pull(p) %>% 
  min()

# Investigate death probabilities p <- seq(.01, .03, .0025).
# What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
p <- seq(.01, .03, .0025)
p_lose <- sapply(p, function(p){
  e_n <- n*(a*p+b*(1-p))
  se_n <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1000000, mean = e_n, sd = se_n)
})

data.frame(p, p_lose) %>%
  filter(p_lose > 0.9) %>% 
  pull(p) %>% 
  min()

# Define a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, 
# loss of -$150,000 on a claim, and profit of $1,150 when there is no claim. Set the seed to 25, then run the model once.
# What is the reported profit (or loss) in millions?
set.seed(25)
n <- 1000
a <- -150000
b <- 1150
p_loss <- .015
s <- sample(c(a,b),n,prob=c(p_loss,1-p_loss),replace=TRUE)
profit <- sum(s)/10^6
profit

# Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000 replicates to simulate the range 
# of profits/losses over 1,000 loans.
# What is the observed probability of losing $1 million or more?
set.seed(27)
B <- 10000
profit <- replicate(B,{
  n <- 1000
  a <- -150000
  b <- 1150
  p_loss <- .015
  s <- sample(c(a,b),n,prob=c(p_loss,1-p_loss),replace=TRUE)
  sum(s)
})
  mean(profit < -10^6)
  
  
####### Questions 5 and 6: Insurance rates, part 3  ####### 
  
# Suppose that there is a massive demand for life insurance due to the pandemic, and the company wants to find a premium cost 
# for which the probability of losing money is under 5%, assuming the death rate stays stable at  𝑝=0.015 .
# Calculate the premium required for a 5% chance of losing money given  𝑛=1000  loans, probability of death  𝑝=0.015 , and 
# loss per claim  𝑙=−150000 . Save this premium as x for use in further questions.

# prob(S < 0) = 0.05
n <- 1000
l <- -150000
p <- .015
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))

# What is the expected profit per policy at this rate?
e <- l*p + x*(1-p)

# What is the expected profit over 1,000 policies?
e_n <- n*e

# Run a Monte Carlo simulation with B=10000 to determine the probability of losing money on 1,000 policies given the new premium x, 
# loss on a claim of $150,000, and probability of claim  𝑝=.015 . Set the seed to 28 before running your simulation.
# What is the probability of losing money here?
set.seed(28)
B <- 10000
profit <- replicate(B, {
  n <- 1000
  a <- -150000
  b <- x
  p <- 0.015
  s <- sample(c(a,b), n, prob=c(p,1-p), replace=TRUE)
  sum(s)
})

mean(profit < 0)

# The company cannot predict whether the pandemic death rate will stay stable. Set the seed to 29, then write a Monte Carlo simulation 
# that for each of  𝐵=10000  iterations: 
# randomly changes  𝑝  by adding a value between -0.01 and 0.01 with sample(seq(-0.01, 0.01, length = 100), 1)
# uses the new random  𝑝  to generate a sample of  𝑛=1,000  policies with premium x and loss per claim  𝑙=−150000 
# returns the profit over  𝑛  policies (sum of random variable)

# What is the expected value over 1,000 policies?
p <- 0.015
set.seed(29)
B <- 10000

profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  s <- sample(c(l,x), n, prob=c(new_p,1-new_p), replace=TRUE)
  sum(s)
})

e_n <- mean(profit)

# What is the probability of losing money?
mean(profit < 0)

# What is the probability of losing more than $1 million?
mean(profit < -10^6)
