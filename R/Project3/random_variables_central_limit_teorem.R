# RANDOM VARIABLES

library(dplyr)
dat <- read.csv('femaleMiceWeights.csv') # 24 mices with different diets
control <- filter(dat, Diet=='chow') %>% select(Bodyweight) %>% unlist
treatment <- filter(dat, Diet=='hf') %>% select(Bodyweight) %>% unlist
mcont <- mean(control)
mtreat <- mean(treatment)
obs <- mtreat - mcont

population <- read.csv('femaleControlsPopulation.csv') # all data from the lab, 225 mices.
population <- unlist(population)
population
mean( sample(population, 12) ) # the mean is always different for a random selected sample of 12 mices from the 225.

# Null hypothesis (just illustrative, in reality we do not have the total population data.)
n <- 10000
nulls <- vector('numeric',n)
for(i in 1:n){
  
  control <- sample(population, 12)
  treatment <- sample(population, 12)
  nulls[i] <- mean(treatment) - mean(control)
}
hist(nulls) 
mean( abs(nulls) > obs) # p-value

#----------------------------------------------------------------------------------------------------------------------

# CENTRAL LIMIT TEOREM

install.packages('rafalib')
library(rafalib)
qqnorm(nulls)
qqline(nulls)


N <- length(treatment)
se <- sqrt( var(treatment)/N + var(control)/N)
t_stat <- obs / se
pvalue <- 2 * (1 - pnorm(t_stat))
pvalue
t_stat


ttest <- t.test(treatment,control)
ttest
