# Confidence Intervals

library(dplyr)
set.seed(1)

# this data is as if we had all population of the mices
chow_population <- read.csv('femaleControlsPopulation.csv')
chow_population <- unlist(chow_population)
mu_chow <- mean(chow_population)
print(mu_chow)

# random sample of mices
n <- 30
chow <- sample(chow_population, n)
print(mean(chow))

#standard error
se <- sd(chow)/sqrt(n)

obs <- mean(chow) - mean(chow_population) # this is what we want to know, the great result is 0
# normal 0,1 variable

obs2 <- obs / se
Q <- qnorm(1 - 0.05/2)  # we know that 95% of the time the obs2 will be between -Q and Q (2 standard deviations from the mean)

# in reality we do not know the mean of the population so we can use:
interval <- c(mean(chow) - Q*se, mean(chow) + Q*se)
interval
interval[1] < mu_chow & interval[2] > mu_chow

# repeat the code above sevaral times
b <- 250
mypar()
plot(mean(chow_population)+c(-7,7), c(1,1), type='n', xlab='weight', ylab='interval', ylim = c(1,b))
abline(v=mean(chow_population))
n = 30
for (i in 1:b){
  chow <- sample(chow_population, n)
  se <- sd(chow)/sqrt(n)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se)
  covered <-  mean(chow_population) >= interval[1] & mean(chow_population) <= interval[2]
  color <- ifelse(covered, 4, 2)
  lines(interval, c(i,i), col=color)
}

# repeat the code above for only 5 mice for check if the CLT still work
n = 5 # and repeat the for loop
plot(mean(chow_population)+c(-7,7), c(1,1), type='n', xlab='weight', ylab='interval', ylim = c(1,b))
abline(v=mean(chow_population))
for (i in 1:b){
  chow <- sample(chow_population, n)
  se <- sd(chow)/sqrt(n)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se)
  covered <-  mean(chow_population) >= interval[1] & mean(chow_population) <= interval[2]
  color <- ifelse(covered, 4, 2)
  lines(interval, c(i,i), col=color)
} 
# in this case we have a lot more red lines that indicates the sample do not represent the population in the 95% confidence interval