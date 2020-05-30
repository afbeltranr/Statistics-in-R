library(downloader) 
library(dplyr)
#-------------excercices 1-------------

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
n <- 1000 
averages5 <- vector("numeric", n)
for(i in 1:n){
  
  averages5[i]  <- sample(x,5) %>% mean
   
}

set.seed(1)
n <- 1000 
averages50 <- vector("numeric", n)
for(i in 1:n){
  
  averages50[i]  <- sample(x,50) %>% mean
  
}

par(mfrow=c(1,2))
hist(averages5)
hist(averages50)

 

  p <- pnorm(25,mean(averages50),sd(averages50)- pnorm(23,mean(averages50),sd(averages50)))

p2 <- pnorm(25,23.9,0.43)- pnorm(23,23.9,0.43)
#------------exercises 1 END --------

    library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")

control <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist  
treatment <- filter(dat, Diet== "hf") %>% select(Bodyweight) %>% unlist

ttest <- t.test(treatment, control)
ttest

N <- length(treatment)

obs <- mean(treatment) - mean(control)

se <- sqrt(var(treatment)/N + var(control)/N)
   

   
tstat <- obs/se 

2*(1 - pnorm(tstat))

population <- read.csv("femaleControlsPopulation.csv")

population <- unlist(population)

n <- 10000
nulls <- vector("numeric",n)
for (i in 1:n){
  
  control <- sample(population,3)
  treatment <- sample(population,3)
  se <- sqrt(var(treatment)/3+ var(control)/3)
  nulls[i] <- (mean(treatment) - mean(control)) / se
}  

mypar()

qqnorm(nulls)
qqline(nulls)
  

