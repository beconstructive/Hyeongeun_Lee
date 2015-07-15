
#####################################
### Assignment #3 - Hyeongeun Lee ###
#####################################


# load dataset
mydata <- read.csv(file.choose(), header=T)

head(mydata)
attach(mydata)

ye.model <- lm(CRIME ~ AREA, data=mydata)
pred <- predict(ye.model, mydata)

plot(AREA, CRIME)
abline(ye.model, col='blue')
points(AREA, pred, col='blue', pch=16)

detach(mydata)