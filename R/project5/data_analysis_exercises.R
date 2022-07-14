# Scatterplot 

library(dplyr)
data(nym.2002, package="UsingR")
males <- filter(nym.2002, gender == 'Male')
females <- filter(nym.2002, gender == 'Female')

# 1) For males, what is the Pearson correlation between age and time to finish?

corr = cor(males$age, males$time)
corr

# 2) For females, what is the Pearson correlation between age and time to finish?

corr = cor(females$age, females$time)
corr

# 3) If we interpret these correlations without visualizing the data, we would conclude that the older we get, 
# the slower we run marathons, regardless of gender. 
# Look at scatterplots and boxplots of times stratified by age groups (20-25, 25-30, etc..).
# After examining the data, what is a more reasonable conclusion?

plot(nym.2002$age, nym.2002$time, ylab = 'Time', xlab = 'Age')

library(rafalib)

mypar(2,2)
plot(females$age, females$time)
plot(males$age, males$time)

group <- floor(females$age/5) * 5
boxplot(females$time~group)

group <- floor(males$age/5) * 5
boxplot(males$time~group)


# ----------------------------------------------------------------------------------------------------------
# Symmetry of Log Ratios Exercises

time = sort(nym.2002$time)

# 1) What is the fastest time divided by the median time?
median <- median(time)
min <- min(time)
r <- min/median
print(r)

# 2) What is the slowest time divided by the median time?
r <- max(time)/median




