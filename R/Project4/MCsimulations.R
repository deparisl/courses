library(dplyr)
set.seed(1)

# this data is as if we had all population of the mice
chow_population <- read.csv('femaleControlsPopulation.csv')
chow_population <- unlist(chow_population)

ttestgenerator <- function(n) {
  cases <- sample(chow_population, n)
  controls <- sample(chow_population, n)
  tstat <- (mean(cases) - mean(controls))/ sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}

ttests <- replicate(1000, ttestgenerator(10))
hist(ttests)
qqnorm(ttests)
abline(0,1)

ttests <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
abline(0,1)

ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps,df=4), ttests, xlim=c(-6,6), ylim=c(-6,6))
abline(0,1)

#-----------------------------------------------------------------------------------------------------------------------

# Monte Carlo tests

# 1)

set.seed(1)
sample <- rnorm(5)
t <- sqrt(5)*mean(sample)/sd(sample)
t
# 2)

set.seed(1)
tstats <- function(n){
  sample <- rnorm(n)
  t <- sqrt(5)*mean(sample)/sd(sample)
  t > 2
}

test <- replicate(1000, tstats(5))
mean(test == 'TRUE')
1-pt(2,df=4)

# 3)

library(rafalib)
mypar(3,2)

set.seed(1)
Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, {
    X <- rnorm(N)
    sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 



