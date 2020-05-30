library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)


library(dplyr)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

set.seed(1)
alpha <- 0.05
N <-5
dat.ns <- sample(bwt.nonsmoke, 5)
dat.s <- sample(bwt.smoke, 5)
pvalue  <-  t.test(dat.ns, dat.s)$p.value
pvalue < alpha



# ___ Exercise 2 ------------
set.seed(1) # reproducible random numbers generation


# first, we define a function to perform rejections.

reject <- function(N, alpha=0.05){
  dat.ns <- sample(bwt.nonsmoke, N)
  dat.s <- sample(bwt.smoke, N)
  pval <- t.test(dat.ns, dat.s)$p.value
  pval < alpha
}
reject(12) # w try our reject function for samplesize 12 
B <- 10000 # defining how many times we'll draw samples and determine whether or not the p-val is less than the significance level 0.05, and therefore reject the null hypothesis.
rejections <- replicate(B,reject(5))
power.babies <- mean(rejections)
power.babies

Ns <- c (5,30,60,90,120)


powers <- sapply(Ns, function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})
# we obtain a power of about 80% using a sample size of 60

#now we set alpha at 0.01
powers2 <- sapply(Ns, function(N, alpha){
  rejections <- replicate(B, reject(N, alpha=0.01))
  mean(rejections)
})
powers2

#now, using a significance level of 0.01 we reduce our change of rejecting in general, so our chance of successfully rejecting the null in this case, our power, reduces as well. We need to take a larger sample to improve our power. 