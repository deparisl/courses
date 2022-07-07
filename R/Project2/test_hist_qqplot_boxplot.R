load("C:/Users/nathan2/Downloads/skew.RData")
dim(dat) # dimension of the matrix

par(mfrow = c(3,3)) # plot 3x3 grafics

for (i in 1:9) {
  x <- dat[,i]
  qqnorm(x) 
  qqline(x) }

# columns that are skewed for the identity line -> column 4 and column 9
par(mfrow = c(1,2))
qqnorm(dat[,4]) # positive skewed
qqline(dat[,4])
qqnorm(dat[,9]) # negative skewed
qqline(dat[,9])

# histograms
hist(dat[,4])
hist(dat[,9])

#-----------------------------------------------------------------------------------------------------------------------

# The InsectSprays data set measures the counts of insects in agricultural experimental units treated 
# with different insecticides. This dataset is included in R, and you can examine it by typing:
head(InsectSprays)

# Which spray seems the most effective (has the lowest median count)?
par(mfrow = c(1,1))
boxplot(InsectSprays[,1] ~ InsectSprays[,2], ylab = 'Count', xlab = 'Spray')
# Answer: Spray C

# Let's consider a random sample of finishers from the New York City Marathon in 2002. 
# This dataset can be found in the UsingR package. Load the library and then load the nym.2002 dataset.

install.packages('dplyr')
library(dplyr)
data(nym.2002, package="UsingR")
# Use boxplots and histograms to compare the finishing times of males and females. 

head(nym.2002)
hist(nym.2002$time, xlab='Time')
data <- nym.2002[c('gender', 'time')]

female <- filter(data, gender=='Female')
female
male <- filter(data, gender=='Male')
male

par(mfrow = c(1,2))
hist(female$time, xlab='Female Time')
hist(male$time, xlab='Male Time')

par(mfrow = c (1,2))
qqnorm(female$time)
qqline(female$time)
qqnorm(male$time)
qqline(male$time)

mean(female$time)
mean(male$time)

par(mfrow = c(1,1))
boxplot(nym.2002$time ~ nym.2002$gender, ylab='Time', xlab='Gender')

        