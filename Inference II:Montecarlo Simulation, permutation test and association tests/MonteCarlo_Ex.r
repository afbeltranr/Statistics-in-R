# First Excercise

# We will imagine we are William Sealy Gosset (Student) and we will use our computer to check the result of the distribution of the t-statistic when the sample comes from a normal distribution.

set.seed(1)


n <- 5
sample <- rnorm(n)

tstat <- sqrt(n)*mean(sample)/sd(sample)
tstat


# second exercise 


generator <- function(n=5){
  sample <- rnorm(5)
  tstat <- sqrt(n)*mean(sample)/sd(sample)
  return(tstat)
  }
B <- 1000
ttests <- replicate(B, generator())
mean(ttests>2)


# Exercise 3
1-pt(2,df=4)

b <- 100 

ps <- seq(1/(B+1), 1-1/(B+1), len=b)
qs <- qt(ps, df=4)
Ns <- seq(5,30,5)

quantiles <-sapply(Ns, function(n){
ttests <- replicate(100, generator(n))})

par(mfrow=c(3,2))
for (i in 1:6){
    qqplot(qs, 
           quantiles[,i],
           xlab = "Theoretical t-dist quantiles",
           ylab = " Monte Carlo t-dist. quantiles",
           main=expression(paste("sample size =", Ns[i])))
  abline(0,1)
  
}# We can conclude that the t dist approximation works better for small sample sizes.


# exercise 4

# We have to obtain t-statistics for two means, and then compare. We have to obtain them from standard normalyy distributed data. we have to use t.test() function
set.seed(1)
generator4 <- function(n, mean = 0, sd = 1){
  sample1 <- rnorm(n, mean, sd)
  sample2 <- rnorm(n, mean, sd)
  tsat <- t.test(sample1, sample2, var.equal = TRUE)$t.val
return(tstat)  
}

# First we generate the theoretical quantiles we will compare to. Starting from probabilities, or proportions expressed as percentiles:
  
Ns <- seq(5,30,5)

b <- 100
ps <- seq(1/(b+1), 1-1/(b+1), len=b)

# Now. the way we compute the quantiles depends on the degrees of freedom of each experiment. as we did before we're going to test 6 sample sizes from 5 to 30. so for each sample size we should compute quantiles taking into account sample size.

qs <- sapply(Ns, function(N){
  qt(ps, df=2*N-2)
})

# Now we have our theoretical quantiles based on t-dist for each probability and sample size.

quantiles <- sapply(Ns, function(N){
  replicate(100,generator4(N))
}) # Here we obtain our experimental quantiles, or t-values, for each sample size
par(mfrow= c(3,2))


  for(j in 1:6){
  qqplot(qs[,j], quantiles[,j])  
  }
  

generator4(10)

