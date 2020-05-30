library(dplyr)
      
dat <- read.csv("femaleMiceWeights.csv")

control <- filter(dat, Diet=="chow")
control <- select(control,Bodyweight)
control <- unlist(control)

control <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter (dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

obsdiff <- mean(treatment) - mean(control)
print(obsdiff)


#-- Now the population

population <- read.csv("femaleControlsPopulation.csv")

population <- unlist(population)


means <- vector("numeric",length=20)

for(i in 1:20){
  
  means[i] <- sample(population,12) %>% mean
  
}


control2 <- sample(population,12)
treatment2 <- sample(population,12)

print(mean(treatment2)- mean(control2))

# now lets do it 10000 times

n <- 10000
null <- vector("numeric",n)
for (i in 1:n){
  
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}

 mean(null >= obsdiff)


 null >= obsdiff
 data(father.son,package="UsingR")
 x <- father.son$fheight
 
 View(x)
 round(sample(x,10),1)

 n <- 100
 library(rafalib)
 nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency")
 totals <- vector("numeric",11)
 for (i in 1:n) {
   control <- sample(population,12)
   treatment <- sample(population,12)
   nulldiff <- mean(treatment) - mean(control)
   j <- pmax(pmin(round(nulldiff)+6,11),1)
   totals[j] <- totals[j]+1
   text(j-6,totals[j],pch=15,round(nulldiff,1))
   if(i < 15) Sys.sleep(1) ##You can add this line to see values appear slowly
 }
   
 hist(null, freq=TRUE)
 abline(v=obsdiff, col="red", lwd=2)
 
 
 1 - pnorm(obsdiff,mean(null),sd(null)) 
 
  
