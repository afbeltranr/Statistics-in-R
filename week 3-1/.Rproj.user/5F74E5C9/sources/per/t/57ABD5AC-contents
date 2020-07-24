

# To check your understanding of the t-test videos, please complete the following exercises.
# 
# For these exercises we will load the babies dataset from babies.txt. We will use this data to review the concepts behind the p-values and then test confidence interval concepts.
library(downloader)
library(dplyr)
library(rafalib)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
        babies <- read.table("babies.txt", header=TRUE)


bwt.nonsmoke <- filter(babies, smoke == 0) %>% select(bwt) %>% unlist

bwt.smoke <- filter(babies , smoke == 1) %>% select(bwt) %>% unlist

mean(bwt.nonsmoke) - mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

set.seed(1)

 dat.ns <- sample(bwt.nonsmoke, 25)
 dat.s <- sample(bwt.smoke, 25)
 meandiff <- mean(dat.ns) - mean(dat.s)
 tstat1 <- meandiff/sqrt(var(dat.ns)/25+var(dat.s)/25)
tstat1
 tstat <- t.test(dat.ns, dat.s)
tstat
 tval <- 1.6593

pval <- 1-(pnorm(abs(tval))- pnorm(-abs(tval)))
pval



#---------Confidence intervals ------------




