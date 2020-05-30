rm(list=ls())
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)


#--- exercises #1 --------------
set.seed(1)
n <- 100 # total number of dice we're rolling

x <- sample(1:6, n, replace=TRUE)

mean(x==6)

p1 <- 1/6 # mean of die value distribution
sdp <- (p1*(1-p1)/n)  
z <- replicate(10000,{
           x <- sample(1:6, n, replace=TRUE)
               (mean(x==6)-p1)/sqrt(p1*(1-p1)/n)
}
               )

qqnorm(z)
abline(0,1)
hist(z)
abline(v=c(2,-2) ,col=2)
mean(abs(z)>2)


# ---------------2 ------------------
set.seed(1)
p <- c(0.5, 0.5, 0.01, 0,01)
N <-c(5,30,30,100)
die.sides <- 6       

rep <- 10000
z <- replicate(10000,{
  x <- sample(1:6, n, replace=TRUE)
  (mean(x==6)-p1)/sqrt(p1*(1-p1)/n)
}
)

z2 <- sapply(N, function(n){
  
   replicate(10000, {
    
    x <- sample(1:die.sides, n, replace= TRUE)
    
(mean(x==6)- p[which(n %in% N)] )/   sqrt(p[which(n %in%N)] * (1-p[which(n %in% N)]) /n )
  
  })
  
  
})

library(rafalib)
mypar(2,2)
# seq(along=Ns) generates the sequence 1, 2, ..., length(along)
for (i in seq(along=N)) { 
  titleavg <- signif(mean(z2[,i]),3)
  titlesd <- signif(popsd(z2[,i]),3)
  title <- paste0("p = ", p[i], " and n = ", N[i])
  qqnorm(z2[,i],main=title)
  qqline(z2[,i],col=2)
}           
          

set.seed(1)

ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==6) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}


