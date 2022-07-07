# First list

data <- read.csv('femaleMiceWeights.csv')
data[12, c('Bodyweight')]
data2 <- data.frame(data$Bodyweight)
length(data[,1])
data_hf <- data[13:24, c('Diet', 'Bodyweight')]
data_hf
mean(data_hf[, c('Bodyweight')])
?sample
set.seed(1)
sample(13:24, size=1)
data[21, c('Bodyweight')]



# second list------ dplyr

install.packages('dplyr')
library(dplyr)
View(data)

controls <- filter(data, Diet == 'chow')
View(controls)
controls <- select(controls, Bodyweight)
unlist(controls)

controls <- filter(data, Diet == 'chow') %>% select(Bodyweight) %>% unlist 
mean(controls)

install.packages('downloader')
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

dat <- read.csv('msleep_ggplot2.csv.')
View(dat)
class(dat)
primates <- filter(dat, order=='Primates')
nrow(primates)
class(primates)

primates_sleep <- filter(dat, order=='Primates') %>% select(sleep_total) %>% unlist
View(primates_sleep)
class(primates_sleep)
mean(primates_sleep)
?summarise

primates_sleep <- filter(dat, order=='Primates') %>% summarise(mean = mean(sleep_total))
primates_sleep
