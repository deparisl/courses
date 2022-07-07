
# AVERAGE LIFE EXPECTANCY FOR COUNTRIES 

# dataset Gapminder 
# This data set contains the life expectancy, GDP per capita, and population by country, every five years, from 1952 to 2007.

install.packages("gapminder")
library(dplyr)
library(gapminder)
data(gapminder)
head(gapminder)

# select only data from lifeExp column from 1952 
x <- gapminder
x <- filter(x, year == '1952') %>% select('lifeExp') %>% unlist
hist(x, xlab='Life Expectancy')

# 1) What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?

mean( x <= 40 ) # Answer 0.2887324


# 2) sapply() on a custom function

prop = function(q) {
  mean(x <= q)
}
prop(40)
qs = seq(from=min(x), to=max(x), length=20)
qs
props = sapply(qs, prop)

# function in one line:
props = sapply(qs, function(q) mean(x <= q))
par(mfrow = c(1,2))
plot(qs, props)
plot(ecdf(x)) # same thing


x <- gapminder
write.csv(x,"gapminder.csv", row.names = FALSE)
