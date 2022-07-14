# exercises with high correlation but not relation between the data.

# calculate the correlation between margarine consumption per capita in the USA and the divorce rate of Maine.
install.packages('dslabs')
library(dslabs)
data("divorce_margarine")
plot(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
cor(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)

# Median, MAD, and Spearman Correlation Exercises

data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

# we will reshape the data so that each row is a chick
chick <- reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time", direction="wide")
head(chick)
# remove the NAs
chick <- na.omit(chick)

# 1) How much does the average of chick weights at day 4 increase if we add 
# an outlier measurement of 3000 grams?

c <- c(chick$weight.4)
day4 <- c(chick$weight.4, 3000)
day4
ratio <- mean(day4)/ mean(c)
print(ratio)

# 2) the same as exer. 1 but with median:

ratio <- median(day4)/median(c)
print(ratio)

# 3) the same ratio for standard deviation

day4std <- sd(day4)
cstd <- sd(c)
ratio <- day4std/cstd
print(ratio)

# 4) the same for the MAD

mad4 <- mad(day4)
madc <- mad(c)
ratio <- mad4/madc
print(ratio)

# 5) correlations

plot(chick$weight.21, chick$weight.4)
day21 <- c(chick$weight.21, 3000)
corr <- cor(day4,day21)
coor <- cor(c, chick$weight.21)
ratio <- corr/coor
print(ratio)
#--------------------------------------------------------------------------------------------------------

# Mann-Whitney-Wilcoxon Test Exercises

# 1) Perform a t-test of x and y, after adding a single chick of weight 200 grams to x (the diet 1 chicks). 
# What is the p-value from this test?

library(dplyr)
x <- filter(chick, Diet == 1)
x <- c(x$weight.4)
y <- filter(chick, Diet == 4)
y <- c(y$weight.4)
x <- c(x, 200)
ttest <- t.test(x,y)
ttest$p.value

# 2) the same for the MWW test

mww <- wilcox.test(x,y)
mww$p.value

# 3) What is the difference in t-test statistic (obtained by t.test(x,y)$statistic) 
# between adding 10 and adding 100 to all the values in the group y? 
# Take the the t-test statistic with x and y+10 and subtract the t-test statistic with x and y+100. 
# The value should be positive.

x <- filter(chick, Diet == 1)
x <- c(x$weight.4)
y <- filter(chick, Diet == 4)
y <- c(y$weight.4)

library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

ttest0 <- t.test(x,y+10)$statistic
ttest1 <- t.test(x,y+100)$statistic
difference <- ttest0 - ttest1
print(difference)
