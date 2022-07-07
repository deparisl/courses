# EXPLORATORY DATA ANALYSIS (EDA)

install.packages("UsingR")
library(UsingR)
x = father.son$fheight
length(x)
# take a random height of the 1078 data available 
y = round(sample(x,20),1)

#---------------------------------------------------------------------------------------------------------------------
# HISTOGRAM

hist(x)
# or with specific bins and labels
bins <- seq(floor(min(x)),ceiling(max(x))) # select the lower and higher values to built the bins
hist(x,breaks=bins,xlab="Height",main="Adult men heights")

#----------------------------------------------------------------------------------------------------------------------
# EMPIRICAL CUMMULATIVE FUNCTION (CDF) 

myCDF <- ecdf(x) 
# evaluate the function at these values:
xs<-seq(floor(min(x)),ceiling(max(x)),0.1) 
# plot the values:
plot(xs,myCDF(xs),type="l",xlab="x=Height",ylab="F(x)")


#----------------------------------------------------------------------------------------------------------------------
# For a NORMAL DISTRIBUTION

# data's mean and standard deviation 
mean(x)
sd(x)
# proportion of individuals with height > 70
mean(x>70) # 20%
# normal approximation
1-pnorm(70,mean(x),sd(x)) #20% -> the normal distribution maybe is a good approximation of the data
# to make it systematic we can use the Q-Q plot

#-----------------------------------------------------------------------------------------------------------------------
# Q-Q PLOT

# the percentiles we want
ps <- seq(0.01,0.99,0.01)
# the quantiles we want
qs <- quantile(x,ps)
# quantiles for the normal distribution using the percentiles above
normalqs <- qnorm(ps, mean(x), sd(x))
# plot the normal distribution quantiles and the quantiles of the data to see if it is a good approximation of the data
plot(normalqs, qs, xlab='Normal percentiles', ylab='Height percentiles')
abline(0,1) # identity line
# the normal dist. is predicting very well the data

# plot the QQ plot automatically
qqnorm(x)
qqline(x)

#-----------------------------------------------------------------------------------------------------------------------
# BOXPLOT

# when normal distribution is not a good approximation (ex: salaries data from usingR library)
par(mfrow = c(1,1))
x <- exec.pay
hist(x)
qqnorm(x)
qqline(x)

# boxplot
boxplot(x, ylab='10000s of dollars', ylim=c(0,400))
# the grey box contain 50% of the data (from 25th percentile to 75th percentile)
# the bold line in the box represents the median of the data (not the mean)
# the lines represents the range of the data
# dots are outliers (the range times the interquartile range from the box)
?boxplot
