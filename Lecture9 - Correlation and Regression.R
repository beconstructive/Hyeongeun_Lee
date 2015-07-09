##########################
### '150709. lecture 9 ###
##########################


# Q1. Association b/w the amount of money spends on 1st visit
#         vs their 2nd visit.

first <- c(20,32,35,34,40,51,52,56,57,68)
second <- c(23,34,36,44,42,51,54,57,54,62)

# a. display the relationship
plot(first, second)

# b. discribe the pattern.
#     => Yes, they have string positive linear relationship. 

# c. calculate the corr.
cor(first,second)

# d. s.e. of corr.
# corr. 
sqrt( (1-cor(first,second)^2) / (length(first)-2) )

# e. 95% c.i. for corr.
cor.test(first,second)$conf.int






# Q2-a. adding $30 to each 2nd visits
second2 <- second + 30
cor(first,second2)
#   => Adding a constant does not affect corr.

# Q2-b. multiply by 100 to each 1st visits.
first2 <- first*100
cor(first2,second)
#   => Multiplying a constant does not affect corr. because they was standardzied.





# Q3
Q3_data <-
  matrix(
    c(-1.3,4,
    -0.5,22,
    -0.3,0,
    0.2,0,
    0.1,11,
    0.5,13,
    1.0,17,
    0.3,25,
    0.4,24,
    0.5,27,
    0.1,29,
    0.2,33,
    0.4,33,
    1.3,42,
    1.2,33,
    1.4,20,
    1.6,19,
    1.6,19,
    1.8,25,
    3.1,65)
    ,ncol = 2, byrow = T)
Q3_data <- data.frame('X'=Q3_data[,1],'Y'=Q3_data[,2])

# a. 
plot(Q3_data); cor(Q3_data)
#   => They have positive linear relationship.

# b.
myfit <- lm(Y~X, Q3_data)
myfit
abline(myfit)

# c.
summary(myfit)
#   -> Yes, Since the p-value for testing null hyphothesis, beta=0, is lower than our significance lev. alpha=0.05.

# d.
plot(Q3_data) # The 20th obs. looks like extreme.

myfit2 <- lm(Y~X, Q3_data[-20,])
summary(myfit2)

abline(myfit); abline(myfit2, col='red')
myfit2$coefficients[2] - myfit$coefficients[2] # Lower about -3.89

# +@ Do prediction.
points(Q3_data[-20,]$X, as.vector(predict(myfit2)), col='blue')