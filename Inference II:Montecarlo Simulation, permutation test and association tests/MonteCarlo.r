# In this script we will use a Monte Carlo simulation to compare the CLT to the t-distribution approximation for different sample sizes.

library(dplyr) # package with functions select and filter, and %>% operator.
dat <- read.csv("mice_pheno.csv") # we read the whole mice population and store it in a variable or object called "dat"

controlPopulation <- filter(dat, Sex == "F", Diet == "chow") %>% select(Bodyweight) %>% unlist # we create a new object within wich information about female control population will be stored. more specifically mice weights. 

# We will build a function that automatically generates a t-statistic under the null hypothesis for a sample size of n.


generator <- function(n){
  
  cases <- sample(controlPopulation,n)
  controls <- sample(controlPopulation,n)
tstat <- (mean(cases)-mean(controls)) / sqrt(var(cases)/n + var(controls)/n)  
  
return(tstat)
  
} # here we wrote a function which values will vary with n,  the parameter we set between ().
#afterwards, between {}, we wrote a expression that relate certain variables with the parameter n. In this case we wrote an algorith (sequence of steps) in order to draw samples form the control population data and then, we use the standard error to transform this variable in the new variable tstat, which distributes normally, in fact in a standard normal distribution.

ttests <- replicate(1000, generator(10)) # in order to check tstat distribution, and taking into account that each time we draw a sample we obtain a new random array of numbers, we can take advantage of this and repeat the experiment a large number of times, as our results are based in asymptotics: they hold when the sample size goes to infinity.

hist(ttests) # 1000 experiments (Monte Carlo Simulations) using our function generator() with a sample size of 10.

# Now we will check how well this distribution is approximated by the normal distribution:

qqnorm(ttests)
abline(0,1)
# this looks like a very good approximation. For this particular population, a sample size of 10 was large enough to use the CLT approximation.

#Now let us check with a sample size of 3.


ttest <- replicate(1000 , generator(3))
qqnorm(ttest)
abline(0,1)

# Now we see that the large quantiles, referred to as the "tails" are larger than expected. we've discussed that in these cases when CLT stops working because our sample size is too small, the t-distribution is a better approximation:

ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps, 2*3-2),
       ttest,
       xlim=c(-6,6),
       ylim=c(-6,6))
abline(0,1)


# we can see that the t-distribution is a much better approximation in this case, but it is still not perfect. This si due to the fact that the original data is not that wella pproximated by the normal distribution.

# let us check that we've just said.

qqnorm(controlPopulation)
qqline(controlPopulation)

# Again, even for the population data we can se deviations in the large quaintile zone.

# Now, in practice, we do not have access to the entire population. 
# When we want to use Monte Carlo simultions in practice, it is much more typical to assume a parametric distribution and generate a population from this, which is called a "parametric simulation".

# So, we take parameters estimated from the real data (mean and standard deviation), and plug these into a model. This is actually the most common forma of the Monte Carlo simulation.

# For the case of mice weights, we could use the parameters derived by: the average mouse weight, which is about 24 g, and the standard deviation which is about 3.5 g.

# Then we say that the distribution is approximately normal, and generate a new population data:

controls <- rnorm(5000, mean=24, sd=3.5)

# After we generate the data, we can repeat the experiment (Monte Carlo Simulation). We no longer have to use the sample() function since we can re-generate random numbers:

generator <- function(n, mean=24, sd=3.5){
  
  cases <- rnorm(n, mean, sd)
  controls <- rnorm(n, mean, sd)
  diff <- mean(cases) - mean(controls)
  se <- sqrt(var(cases) + var(controls))
  tstat <- diff/se
  
return(tstat)  
}

ttests.random <- replicate(1000, generator(10))

hist(ttests.random)


