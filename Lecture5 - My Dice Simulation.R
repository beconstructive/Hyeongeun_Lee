##########################
### '150706. lecture 5 ###
##########################

# 2개의 주사위를 던져서 눈을 합하라
# 10, 100, 10000 번의 시뮬레이션 draw plot
# 알 수 있는 점은?

myfun <- function(trials){
  
  result = numeric(trials)
  
  for (i in 1:trials) {
    dice01 <- sample(1:6,1)
    dice02 <- sample(1:6,1)
    result[i] <- sum(dice01, dice02)
  }
  hist(result, probability = T)
  return(result)
}

par(mfrow=c(1,3))
myfun(10)
myfun(100)
myfun(10000)
