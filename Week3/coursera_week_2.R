setwd('C:\\Users\\db345c\\Desktop\\Practical Machine Learning\\week3')

### Predicting with trees
##########################################################

data(iris); library(ggplot2)
names(iris)
table(iris$Species)

library(caret)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

qplot(Petal.Width, Sepal.Width, color=Species, data=training)

modFit <- train(Species ~., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)

######################## pretty print for rpart
# library(rattle)
# fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata=testing)


# Bagging (Bootstrap Aggregating)
############################################################

library(ElemStatLearn)
data(ozone, package='ElemStatLearn')
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA, nrow=10, ncol=155)
for(i in 1:10) {
    ss <- sample(1:dim(ozone)[1], replace=T)
    ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
    ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for(i in 1:10) {
    lines(1:155, ll[i,], col="grey", lwd=2)
}
lines(1:155, apply(ll, 2, mean), col="red", lwd=2)

######
library(party)
predictors = data.frame(ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B=10,
               bagControl = bagControl(fit=ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
plot(ozone$ozone, temperature, col='lightgrey', pcg=19)
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch=19, col='red')
points(ozone$ozone, predict(treebag, predictors), pch=19, col='blue')


### Random forests
##############################################################
data(iris); library(ggplot2); library(caret)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

library(caret)
modFit <- train(Species~., data=training, method='rf', prox=TRUE)
modFit

getTree(modFit$finalModel, k=2)



### Boosting
##############################################################
library(ISLR); data(Wage); library(ggplot2); library(caret)
Wage <- subset(Wage, select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage ~., method="gbm", data=training, verbose=FALSE)
print(modFit)

qplot(predict(modFit, testing), wage, data=testing)



### Model Based Prediction
#############################################################
data(iris); library(ggplot2); library(caret)
names(iris)

table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

modlda = train(Species ~., data=training, method="lda")
modnb = train(Species ~., data=training, method="nb")
plda = predict(modlda, testing); pnb = predict(modnb, testing)
table(plda, pnb)

equalPrediction = (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPrediction, data=testing)
