

library(gtools)
library(tidyverse)

# Permutations: # of combinations when order matters
#  formula: P_(k,n) = n!/(n-k)!    when n = group size, k = subset size
# Combinations: # of combinations when order does not matter
#  formula: (n k) = P_(k,n)/k! = n!/k!(n-k)!    when n = group size, k = subset size



####### Question 1: Olympic running ####### 

# Question 1a
# In the 200m dash finals in the Olympics, 8 runners compete for 3 medals
#  (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica
#  and the other 5 were from different countries. The three medals were all
#  won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).
medals <- permutations(8,3)
nrow(medals)

# Question 1b
# How many different ways can the three medals be distributed among the
#  3 runners from Jamaica?
jamaica <- permutations(3,3)
nrow(jamaica)

# Question 1c
# What is the probability that all three medals are won by Jamaica?
nrow(jamaica)/nrow(medals)

# Question 1d
# Build runner countries and reset seed
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)

# Run Monte Carlo 10k
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)



###### Question 2: Restaurant management ###### 

# A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. 
# He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
# A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, 
# a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

# Question 2a
# How many meal combinations are possible with the current menu?
entree <- combinations(6,1)
sides <- combinations(6,2)
drink <- combinations(2,1)
nrow(entree) * nrow(sides) * nrow(drink)

# Question 2b
# The manager has one additional drink he could add to the special.
# How many combinations are possible if he expands his original special to 3 drink options?
drink_2 <- combinations(3,1)
nrow(entree) * nrow(sides) * nrow(drink_2)

# Question 2c
# The manager decides to add the third drink but needs to expand the number of options. The manager would prefer not to change his menu further 
# and wants to know if he can meet his goal by letting customers choose more sides.
# How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
sides_2 <- combinations(6,3)
nrow(entree) * nrow(sides_2) * nrow(drink_2)

# Question 2d
# The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, 
# but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
# - Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 
# 3 drink choices, and a selection of 2 sides from 6 options.
f <- function(entree_counts) {
  entree <- combinations(entree_counts,1)
  drink <- combinations(3,1)
  sides <- combinations(6,2)
  results <- nrow(entree) * nrow(sides) * nrow(drink)
  print(results)
}

# - Use sapply() to apply the function to entree option counts ranging from 1 to 12.
# What is the minimum number of entree options required in order to generate more than 365 combinations?
entree_counts <- 1:12
a <- sapply(entree_counts, f)

df <- data.frame(entrees = entree_counts, combos = a)
df %>% filter(combos > 365) %>% min(.$entrees)

# Question 2e
# The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. 
# He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
# - Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, 
# and a selection of 2 sides from the specified number of side choices.
f2 <- function(side_counts) {
  entree <- combinations(6,1)
  drink <- combinations(3,1)
  sides <- combinations(side_counts,2)
  results <- nrow(entree) * nrow(sides) * nrow(drink)
  print(results)
}

# - Use sapply() to apply the function to side counts ranging from 2 to 12.
# What is the minimum number of side options required in order to generate more than 365 combinations?
side_counts <- 2:12
a2 <- sapply(side_counts, f2)

df2 <- data.frame(sides = side_counts, combos = a2)
df2 %>% filter(combos > 365) %>% min(.$sides)



###### Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1 ###### 

# Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a 
# case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) 
# that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in 
# grams per day (tobgp) across cases and controls grouped by age range (agegp).

# The dataset is available in base R and can be called with the variable name esoph:
head(esoph)

# You will be using this dataset to answer the following four multi-part questions (Questions 3-6).
# You may wish to use the tidyverse package:
library(tidyverse)

# The following three parts have you explore some basic characteristics of the dataset.Each row contains one group of the experiment. Each group has a different 
# combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for
# each group.

# Question 3a
# How many groups are in the study?
nrow(esoph)

# Question 3b
# How many cases are there?  Save this value as all_cases for later problems.
all_cases <- sum(esoph$ncases)
all_cases

# Question 3c
# How many controls are there? Save this value as all_controls for later problems.
all_controls <- sum(esoph$ncontrols)
all_controls

# The following four parts ask you to explore some probabilities within this dataset related to alcohol and tobacco consumption.

# Question 4a
# What is the probability that a subject in the highest alcohol consumption group is a cancer case?
levels(esoph$alcgp)
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# Question 4b
# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# Question 4c
# Given that a person is a case, what is the probability that they smoke 10g or more a day?
levels(esoph$tobgp)
tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

# Question 4d
# Given that a person is a control, what is the probability that they smoke 10g or more a day?
tob_controls <- esoph %>% 
  filter(tobgp != "0-9g/day") %>% 
  pull(ncontrols) %>% 
  sum()

tob_controls/all_controls
