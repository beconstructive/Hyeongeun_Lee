##### SKKU ISS 2015, Lecture 4
##### Objective: plotting open government data

# Jevin West
# Date:  7/2/2015

#clean variables
rm(list=ls(all=TRUE))

# read in data
P <- read.csv(file.choose())



##### Questions

## (1) Compare the precipitation from 1999 to 2005. Are they different?

# extract 1999 and 2005 year.
P_1999 <- log10(P[P$Year==1999,]$Value)
P_2005 <- log10(P[P$Year==2005,]$Value)

# draw histograms.
par(mfrow=c(1,2))
hist(log10(P[P$Year==1999,]$Value), main='Histogram of 1999', xlab='Log-transformed Precipitation')
hist(log10(P[P$Year==2005,]$Value), main='Histogram of 2005', xlab='Log-transformed Precipitation')

# t-test for difference in means.
t.test(P[P$Year==1999,]$Value,P[P$Year==2005,]$Value)



## (2) Precipitation for all countries over time.

# calculate means by each Years.
means <- tapply(P$Value,P$Year,mean)

# draw plot and trend line.
plot(x=levels(factor(P$Year)), y=means, xlab='Year', ylab='Precipitation for all countries')
abline(lm(Value ~ Year, data=P))
