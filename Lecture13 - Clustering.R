############
# Exercise #
############


######################################


ye.model <- lm(income~education, data=sls)
sls$pred <- predict(ye.model, sls)
points(sls$education, sls$pred, col='blue', pch=16)

newsls <- data.frame(education=seq(1,24,0.5))
newsls$pred <- predict(ye.model, newsls)
points(newsls$education, newsls$pred, col='green')
points(newsls$education, newsls$pred, col='green', pch=3)


######################################


# load library
library(klaR)
library(class)
library(e1017)
library(calibrate) #textxy


# dataset
head(mtcars)
attach(mtcars)


# kmeans clustering
bad <- kmeans(mtcars, centers=2)

plot(mpg, hp, col=bad$cluster, pch=16, cex=2, xlab='MPG', ylab='Horsepower')
textxy

# Hierarchical Clustering
cars_norm = scale(mtcars)
d <- dist(cars_norm, method='euclidean')

hc <- hclust(d, method='complete')

plot(hc)
cutree(hc,3)

detach(mtcars)


#################################


# iris dataset
head(iris)
summary(iris)

# load iris data
x = iris[,5]
y = iris$Species

# predict 5th col. with first four cols.
classifier = NaiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5])

# trail data
model = train(x,y, 'nb', trControl=trainControl(method='cv', number=10))

# predict
predict(model$finalModel, x)$class
table(predict(model$finalModel, x)$class, y)


#################################


# spam email dataset
library(ElemStatLearn)

sub = sample(nrow(spam), floor(nrow(spam)*0.9))
train=spam[sub,]
test=spam[-sub,]

xTrain=train[,-58]
yTrain=train$spam

xTest=test[,-58]
yTest=test$spam

model = train(xTrain, yTrain, 'nb', trControl=trainControl(method='cv', number=10))
prop.table(table(predict(model$finalModel, xTest)$class, yTest))