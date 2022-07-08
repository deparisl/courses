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
n <- 30
for (i in 1:b){
  chow <- sample(chow_population, n)
  se <- sd(chow)/sqrt(n)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se)
  covered <-  mean(chow_population) >= interval[1] & mean(chow_population) <= interval[2]
  color <- ifelse(covered, 4, 2)
  lines(interval, c(i,i), col=color)
}

# repeat the code above for only 5 mice for check if the CLT still work
n <- 5 # and repeat the for loop
Q <- qt(1-0.05/2, df=n-1)
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

#-------------------------------------------------------------------------------------------------------------------------

# Power Calculations
# Power is the probability of rejecting the null hypothesis when the alternative hypothesis is true.

# population data file
dat <- read.csv('mice_pheno.csv')
dat
chow_pop <- filter(dat, Sex=='F' & Diet=='chow') %>% select('Bodyweight') %>% unlist
hf_pop <- filter(dat, Sex=='F' & Diet=='hf') %>% select('Bodyweight') %>% unlist

# mean of the 2 population
mu_chow <- mean(chow_pop)
mu_hf <- mean(hf_pop)
# difference between the 2 mean 
diff <- mu_hf - mu_chow
diff
percent_increse <- diff/mu_chow * 100
percent_increse # 9% of increase

# t-test with a random sample
m <- 12
hf <- sample(hf_pop, m)
chow <- sample(chow_pop, m)
pvalue <- t.test(hf,chow)$p.value
pvalue

alpha = 0.05
r <- 2000

# function to verify if the p-value is lower than alpha, return TRUE or FALSE
reject <- function(m, alpha=0.05){
  hf <- sample(hf_pop, m)
  chow <- sample(chow_pop, m)
  pvalue <- t.test(hf,chow)$p.value
  pvalue < alpha
}

reject(12)
rejections <- replicate(r, reject(m))
mu_rejec <- mean(rejections)
mu_rejec # in this case 0.215 -> 21.5% probability to reject the null hypothesis when the alternative is true.

# simulation with several samples sizes to verify the behavior of different samples sizes to the power.
Ns <- seq(5,50,5)
power <- sapply(Ns, function(m){
    rejections <- replicate(r, reject(m))
    mean(rejections)} )
plot(Ns, power, type='b')
