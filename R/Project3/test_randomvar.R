install.packages('downloader')
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)

# weights for the entire population
x <- unlist( read.csv(filename) )
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# 1) What is the average of these weights?
mean(x)

# 2) Make a random sample of size 5. What is the absolute value of the difference between 
# the average of the sample and the average of all the values?

set.seed(1)
y <- mean( sample(x, 5) )
y
abs( y - mean(x))


# 3) After setting the seed at 5, set.seed(5), take a random sample of size 5.
# What is the absolute value of the difference between the average of the sample and the average of all the values?
set.seed(5)
y <- mean( sample(x, 5) )
abs( y - mean(x))

# Why are the answers from 2 and 3 different?
# Because the average of the sample is a random variable.


#-----------------------------------------------------------------------------------------------------------------------
# 1) Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages.
# What proportion of these 1,000 averages are more than 1 gram away from the average of x ?
set.seed(1)
n <- 1000
avg <- vector('numeric',n)
for(i in 1:n){
  
  control <- sample(population, 5)
  avg[i] <- mean(control)
}

hist(avg)
mean(x)
mean(avg > mean(x)+1)
mean(avg < mean(x)-1)
tot <- mean(avg > mean(x)+1) + mean(avg < mean(x)-1)
tot # Answer 0.503

# 2) We are now going to increase the number of times we redo the sample from 1,000 to 10,000. 
# Set the seed at 1, then using a for-loop take a random sample of 5 mice 10,000 times. Save these averages.
# What proportion of these 10,000 averages are more than 1 gram away from the average of x ?
set.seed(1)
n <- 10000
avg <- vector('numeric',n)
for(i in 1:n){
  
  control <- sample(population, 5)
  avg[i] <- mean(control)
}

hist(avg)
mean(x)
mean(avg > mean(x)+1)
mean(avg < mean(x)-1)
tot <- mean(avg > mean(x)+1) + mean(avg < mean(x)-1)
tot # Answer 0.5084


#------------------------------------------------------------------------------------------------------------------------
# Normal Distribution Exercises

# 1) Use a histogram to "look" at the distribution of averages we get with a sample size of 5 and a sample size of 50. 
# How would you say they differ? 
n <- 1000
avg5 <- vector('numeric',n)
for(i in 1:n){
  
  control <- sample(population, 5)
  avg5[i] <- mean(control)
}
n <- 1000
avg50 <- vector('numeric',n)
for(i in 1:n){
  
  control <- sample(population, 50)
  avg50[i] <- mean(control)
}
hist(avg5)
hist(avg50) # both look normal distributions, but with sample size of 50 the spread is smaller. 

# 2) For the last set of averages, the ones obtained from a sample size of 50, what proportion are between 23 and 25?

mean( avg50 > 23 & avg50 < 25 )

# 3) What is the proportion of observations between 23 and 25 in a normal distribution with average 23.9 
 # and standard deviation 0.43?

x <- (1 - pnorm(23,23.9,0.43)) - (1 - pnorm(25,23.9,0.43)) 
x

#-----------------------------------------------------------------------------------------------------------------------
# central limit teorem

library(dplyr)
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 

dat <- na.omit( dat ) # remove lines with missing values

dat <- filter(dat, Diet == 'chow') %>% select('Bodyweight') %>% unlist
mean(dat)

dat # different file from the class
# ----------------------------------------------------------------------------------------------------------------------


# CLT and t-distribution in Practice Exercises

# 1) e 2)
dat <- read.csv('femaleMiceWeights.csv')

n <- 30
p <- 0.5
var <-  p*(1-p)/n
set.seed(1)

y <- replicate(n, (mean(sample(1:6, n, replace=TRUE)==6) - p) / sqrt(p*(1-p)/n) , simplify = 'array' )

#mean(abs(y) > 2)
qqnorm(y)
qqline(y)

# 5) 
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

Xavg <- sum(X)/ length(X)
Xavg
mean(X)

# 6)
sd(X)

# 7)
prob <- 2 * ( 1 - pnorm(2/sd(X) * sqrt(12) ) )
prob

# 8)
se <- sqrt(var(X)/12 + var(Y)/12)
se


# 9)
obs <- abs(mean(X) - mean(Y))
t_stat <- obs / se
tstat <- t.test(X,Y)
t_stat
tstat

# 11)

pvalue <- 2 * (1-pnorm(t_stat))
pvalue
