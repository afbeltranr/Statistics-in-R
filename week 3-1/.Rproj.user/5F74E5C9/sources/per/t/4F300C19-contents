library(dplyr)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex== "F" & Diet== "chow") %>% select(Bodyweight) %>% unlist

hfPopulation <- filter(dat, Sex== "F" & Diet== "hf") %>% select(Bodyweight) %>% unlist

mu_hf<- mean(hfPopulation)
mu_control <- mean(controlPopulation)

print (mu_hf - mu_control)
print (mu_hf - mu_control) /mu_control *100 # difference in relative percent

set.seed(1)
N <- 5
hf <- sample(hfPopulation,N)
control <- sample(controlPopulation,N)
tstat <- t.test(hf,control)
 t.test(hf,control)$p.value

  
N <- 12
alpha <- 0.05 
B <- 2000
reject <- function(N, alpha = 0.05){
  hf <- sample(hfPopulation,N) 
  control <- sample(controlPopulation,N)
    pval <- t.test(hf,control)$p.value
  pval < alpha
}

reject(12)
rejections <- replicate(B,reject(N))
  mean(rejections)
  
  # varying sample size
  Ns <- seq(5, 50, 5)
  
      power <- sapply(Ns,function(N){
    rejections <- replicate(B, reject(N))
    mean(rejections)
  })
  
  plot(Ns, power, type="b")
  
  #varying rejection treshold
  
  N <- 30
  alphas <- c(0.1,0.05,0.01,0.001,0.0001)
  power <- sapply(alphas,function(alpha){
    rejections <- replicate(B,reject(N,alpha=alpha))
    mean(rejections)
  })
  plot(alphas, power, xlab="alpha", type="b", log="x")  

  
  
  
# Now we'll see the effect of both parameters in one plot.
  
    # first, we calculate power as a function of sample size.
  
  B <- 2000
  reject <- function(N, alpha=0.05){
    hf <- sample(hfPopulation,N) 
    control <- sample(controlPopulation,N)
    pval <- t.test(hf,control)$p.value
    pval < alpha
  }
  
  Ns <- seq(5,50,5)
  alphas <- c(0.1,0.05,0.01,0.001,0.0001)
  powers <- matrix(nrow=10, ncol=5)
  
  for(i in 1:5){
   powers[,i] <- sapply(Ns, function(N, alpha){
    rejections <-replicate(2000,reject(N, alpha=alphas[i]))
    mean(rejections)
    })
  }

colors <- seq(1,5,1)
par(new=FALSE)
for (i in 1:5) {
  
  plot(Ns, powers[,i],
       col=colors[i],
       xlab='Ns',
       ylab='power',
       type = 'b',
       ylim = c(0,1)
       )
  par(new=TRUE)
}
alphastext <- c(expression(paste(alpha, '=0.1')),
            expression(paste(alpha, '=0.05')),
            expression(paste(alpha, '=0.01')),
            expression(paste(alpha, '=0.001')),
            expression(paste(alpha, '=0.0001')))   
legend('topleft', alphastext , col=colors, lty=1, cex=0.8)


 set.seed(1)
N <- 12
hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
diff <- mean(hf) - mean(control)
diff / mean(control) * 100

t.test(hf, control)$conf.int / mean(control) * 100


  
