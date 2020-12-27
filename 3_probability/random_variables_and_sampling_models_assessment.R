
# Expected values of a random variable: ap + b(1- p)
# Expected value of the sum of n draws of a random variable: n * (ap + b(1-p))
# Standard deviation of an urn with two values: abs(b - a) * sqrt(p(1 - p))
# Standard error of the sum of n draws of a random variable: sqrt(n) * abs(b - a) * sqrt(p(1 - p))


####### Questions 1 and 2: SAT testing  ####### 
# An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer 
# and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice 
# questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions 
# on the test.

# What is the probability of guessing correctly for one question?
1/5

# What is the expected value of points for guessing on one question?
1/5 * 1 + -0.25* 4/5

# What is the expected score of guessing on all 44 questions?
mu <- 44 * (1/5 * 1 + -0.25* 4/5)

# What is the standard error of guessing on all 44 questions?
se <- sqrt(44) * abs(-0.25 - 1) * sqrt(1/5 * 4/5)

# Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points 
# or higher on the test.
1-pnorm(8,mu,se)

# Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
n <- 44
B <- 10000
guess <- function(n){
  X <- sample(c(-0.25,1), n, replace = TRUE, prob=c(4/5, 1/5))
  sum(X)
}
S <- replicate(B, guess(n))
eight_or_higher <- mean(S >= 8)
eight_or_higher


# The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to 
# eliminate the penalty for guessing.
# Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that 
# is, an incorrect question gives a score of 0.
# What is the expected value of the score when guessing on this new test?
mu <- 44 * (1/4 * 1 + 3/4 * 0)
mu

# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range 
# of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
# hint: 
# This question is asking you to apply a range of probabilities to answering each question correctly, 
# from 0.25 to 0.95. This represents the probabilities that different students will have of answering 
# a question correctly, i.e. 0.25 for students who are simply guessing, to 0.95 for a student that 
# is making very educated decisions on each selection. The vector with probabilities to apply is given 
# to you in the question as "p". You must then determine which of these probabilities is the smallest one 
# that will give a student an 80% chance of answering 35 of the 44 questions correctly. For a student just 
# guessing (0.25), the probability is obviously very low, probably much less than 80%. For a clever 
# student who has 95% chance of answering any individual question correctly the chance is probably quite 
# high, more than 80%. Somewhere in between is the point where the probability of scoring 35 out of 44 
# changes from less than 80% probability to more than 80% probability. At which probability does that happen?

prob <- function(p){
  # calculate the expected value at given p
  mu <- 44 * (1*p + 0*(1-p))
  # calculate the standard error at given p
  se <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, mu, se)
}

r <- sapply(p, FUN=prob)

min(p[which(r > 0.8)])

options(scipen=999)
x <- data.frame(p,r)

####### Question 3: Betting on Roulette ####### 

# A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) 
# out of 38 total pockets. The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a 
# successful bet yields $6. A gambler wants to know the chance of losing money if he places 500 
# bets on the roulette House Special.

# What is the expected value of the payout for one bet?
a <- 6
b <- -1
p <- 5/38
mu <- n * (a*p + b*(1-p)) 

# What is the standard error of the payout for one bet?
se <- abs(b - a) * sqrt(p*(1 - p)) 
se

# What is the expected value of the average payout over 500 bets?
# Remember there is a difference between expected value of the average and expected value of the sum.
n <- 500
a <- 6
b <- -1
p <- 5/38
mu

# What is the standard error of the average payout over 500 bets?
# Remember there is a difference between the standard error of the average and standard error of the sum.
se/sqrt(n)

# What is the expected value of the sum of 500 bets?
mu_sum <- n * mu
mu_sum

# What is the standard error of the sum of 500 bets?
se_sum <- sqrt(n) * se
se_sum

# Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability 
# of losing money over 500 bets,  Pr(𝑋≤0) .
pnorm(0,mu_sum,se_sum) 