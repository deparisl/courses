# T-test
library(dplyr)
library(downloader)
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

# filter the babies weight for non smoker and smoker mom
bwt_nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt_smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

avg_nonsmoke <- mean(bwt_nonsmoke)
avg_smoke <- mean(bwt_smoke)
dif_mean <- avg_nonsmoke - avg_smoke
sd_nonsmoke <- popsd(bwt_nonsmoke)
sd_smoke <- popsd(bwt_smoke)

dif_mean

set.seed(1)
n = 25
# random samples from babies with smoke mothers and non-smoke mothers
dat_ns <- sample(bwt_nonsmoke, n)
dat_s <- sample(bwt_smoke, n)
#t-test
tval <- t.test(dat_ns, dat_s)
tval <- 1.6593



# two-sided test
# p-value for the dat_ns and dat_s using tval
pval <- 1 -(pnorm(abs(tval)) - pnorm(-abs(tval)))
pval
