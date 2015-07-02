#####################
### Assignment 01 ###
#####################


# load original data file
mydata <- read.csv(file.choose())

# exclude reigon name
mydata2 <- mydata[,-1]

# attach my dataset object
attach(mydata2)

# draw histogram of crime rate
hist(CRIME, breaks=20, probability=T)

# draw scatterplot by matrices
pairs(mydata2)