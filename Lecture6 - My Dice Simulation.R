##########################
### '150706. lecture 6 ###
##########################

# 2개의 주사위를 던져서 눈을 합하라
# 10, 100, 10000 번의 시뮬레이션 draw plot
# 알 수 있는 점은?

# create user define function for simuation.
myfun_2dices <- function(trials){
  
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

trials_10     <- myfun_2dices(10)
trials_100    <- myfun_2dices(100)
trials_10000  <- myfun_2dices(10000)

# draw cummulative dist. plot
par(mfrow=c(1,1))
plot.ecdf(trials_10000)





#############################################################





# draw normal dist. histogram
# 얻은 자료의 분포하고 비교하기 위해!
hist(rnorm(n=10000, mean=0, sd=1), freq=F)

# find percential of your height.
# pop. mean : 70 inch /// pop. sd : 5 inch
# my height : 67inch
pnorm(67, mean=70, sd=5)
