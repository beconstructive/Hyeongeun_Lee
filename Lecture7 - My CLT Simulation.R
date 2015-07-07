##########################
### '150707. lecture 7 ###
##########################

# role 1 die many many times
hist(sample(6,10000,replace=T), freq=F)


# throw the dice 5 times and take mean
# do it repeatedly
# dist. of means could be normal dist.

# create my own function for simuation.
myfun_clt_dice <- function(trials){
  
  # create obj. to save # of trials
  result = numeric(trials)
  
  # loop state
  for (i in 1:trials) {
    result[i] <- mean(sample(1:6, 5, replace=T))
  }
  
  # draw histogram
  hist(result, freq = F)
  
  # return # of results
  return(result)
  
}

# draw histograms in 1 window
par(mfrow=c(1,3))

trials_10     <- myfun_clt_dice(10)
trials_100    <- myfun_clt_dice(100)
trials_10000  <- myfun_clt_dice(10000)

# draw normal dist in this case.
par(mfrow=c(1,1))
hist(trials_10000, freq=F)
xv <- seq(1,6,0.1)
yv <- dnorm(xv,mean=mean(trials_10000),sd=sd(trials_10000))
lines(xv,yv)