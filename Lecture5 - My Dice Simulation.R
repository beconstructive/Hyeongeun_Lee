##########################
### '150706. lecture 5 ###
##########################

# 2개의 주사위를 던져서 눈을 합하라
# 10, 100, 10000 번의 시뮬레이션 draw plot
# 알 수 있는 점은?

# create user define function for simuation.
myfun <- function(trials){
  
  # create obj. to save # of trials
  result = numeric(trials)
  
  # loop state
  for (i in 1:trials) {
    dice01 <- sample(1:6,1)
    dice02 <- sample(1:6,1)
    result[i] <- sum(dice01, dice02)
  }
  
  # draw histogram
  hist(result, freq = F)
  
  # return # of results
  return(result)
  
}

# draw histograms in 1 window
par(mfrow=c(1,3))

trials_10     <- myfun(10)
trials_100    <- myfun(100)
trials_10000  <- myfun(10000)
