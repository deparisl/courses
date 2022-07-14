library(UsingR)
data('father.son')
x <- father.son$fheight
y <- father.son$sheight

plot(x,y,xlab = "Father's Height in inches", ylab = " Son's Height in inches")

# round father's height
r <- round(x)
# creating a group that have father height in common
group <- split(y, r)
boxplot(group)
avg <- mean(y[round(x) == 72])
print(avg)

# making the data in standard notation (mean 0 and SD 1)
x <- (x-mean(x))/sd(x)
y <- (y-mean(y))/sd(y)

# means of father and son separated in quadrants 
means <- tapply(y,round(x*4)/4,mean)
fatherheights <- as.numeric(names(means))
plot(fatherheights,means,ylab="average of strata of son heights", ylim = range(fatherheights))
abline(0,cor(x,y))
cor <- cor(x,y)
print(cor) # correlation = 0.50, this value is the slope of the line in the plot (regression line)  
