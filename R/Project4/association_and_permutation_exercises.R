# exercises of week 3: association test

# 1)

d <- read.csv('assoctest.csv')
head(d)
# Compute the Chi-square test for the association of genotype with case/control status 
# (using the table() function and the chisq.test() function). 
# Examine the table to see if it looks enriched for association by eye.
# What is the X-squared statistic?

x <- table(d) # give a frequency table
chi <- chisq.test(x) # perform the chi-square test 
print(chi)

# 2) compute the fisher test for the same table.

fisher <- fisher.test(x) # [erform the fisher exact test
print(fisher)

#---------------------------------------------------------------------------------------
# permutation test

library(downloader)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

# random sample of the population of babies
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)
obs

# We can create one permuted sample with the following code, repeating 1000 times.

nulls <- vector('numeric', 1000)
for (i in 1:1000){
  dat <- c(smokers,nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean <- mean(smokersstar)-mean(nonsmokersstar)
  nulls[i] <- mean }

# p-value
nulls
pval <- mean(abs(nulls) > abs(obs))
print(pval)
# 3) repeat the exercises above with obs <- median() - median():

set.seed(1)
obs <- median(smokers) - median(nonsmokers)
nulls <- vector('numeric', 1000)
for (i in 1:1000){
  dat <- c(smokers,nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean <- median(smokersstar)-median(nonsmokersstar)
  nulls[i] <- mean }
# p-value
nulls
pval <- mean(abs(nulls) >= abs(obs)) 
print(pval)