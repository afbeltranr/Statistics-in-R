library(dplyr)
library(rafalib)

babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke ==0) %>%select(bwt) %>% unlist

bwt.smoke <- filter(babies ,smoke ==1)  %>% select(bwt) %>% unlist


mean(bwt.nonsmoke) - mean(bwt.smoke)

N <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N)
dat.s <- sample(bwt.smoke, N)

se <- sqrt(var(dat.ns)/N + var(dat.s)/N)
Q <- qt(1- 0.01/2, 2*N-2)
Q*se

#---------exercise 3 ---------------
    
set.seed(1)

dat.ns1 <- sample(bwt.nonsmoke, 5)
dat.s1 <- sample(bwt.smoke, 5)

t <- t.test(dat.ns1, dat.s1)
tstat <- 1.8554
pval <- pt(1-tstat, 2*5-2)
