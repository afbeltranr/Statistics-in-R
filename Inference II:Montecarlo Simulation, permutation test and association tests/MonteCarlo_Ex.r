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
  tsat <- t.test(sample1, sample2, var.equal = TRUE)$statistic
return(tsat)  
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
  qqplot(qs[,j], 
         quantiles[,j],
         xlab = "Theoretical t-dist quantiles",
         ylab = " Monte Carlo t-dist. quantiles")  
    abline(0,1)
  }

# Execise 5 

# We've been generating samples from a normal distribution, in a continuous scale.

# This exercise is about checking how a binary variable will be distributed, and if its distribution can be approximated by the t-distribution.


generator5 <- function(n){
  
  sample <- sample(c(-1,1),n,replace=TRUE)
  tstat <- sqrt(n)*mean(sample)/sd(sample)
  return(tstat)
}
# Here we write our function for t tests for one random variable, in this case a binary variable that varies in c(-1,1).

#Now we have to build our theoretical quantiles vector as before.
b <- 100
ps <- seq(1/(b+1), 1-1/(b+1), len=100 )#probabilities, percentiles.
qs <- qt(ps, df=14)# Theoretical t-dist quantiles with 14 degrees of freedom

#It is time to generate our Monte Carlo simulated quantiles for our binary data.

ttests <- replicate(100, generator5(15))
dev.off()
qqplot(qs, ttests)
abline(0,1)


## Exercise 6


# Now we will check if this approximation remains for big sample sizes, such as 1000
ps <- seq(1/(b+1), 1-1/(b+1), len=1000 )#probabilities, percentiles.
qs <- qt(ps, df=999)

quantiles <- replicate(1000, generator5(1000))

qqplot(qs,quantiles)
abline(0,1)


# Exercise 7 



set.seed(1)
Ns <- seq(5,45,5)
par(mfrow=c(3,3))
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}
  

