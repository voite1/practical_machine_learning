setwd('C:\\Users\\db345c\\Desktop\\Practical Machine Learning\\week4')

### Regularized regression
##################################################
library(ElemStatLearn); data(prostate)
str(prostate)



### Combining predictors
##################################################
library(ISLR); data(Wage); library(ggplot2); library(caret)
Wage <- subset(Wage, select=-c(logwage))

# Create a building data set and validatioon set
inBuild <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]

inTrain <- createDataPartition(y=buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

dim(training)

dim(testing)

dim(validation)

mod1 <- train(wage ~., method="glm", data=training)
mod2 <- train(wage ~., method="rf", data=training,
              trControl = trainControl(method="cv"), number=3)
pred1 = predict(mod1, testing); pred2 <- predict(mod2, testing)
qplot(pred1, pred2, colour=wage, data=testing)


predDf <- data.frame(pred1, pred2, wage=testing$wage)
combModFit <- train(wage~., method="gam", data=predDf)
compPred <- predict(combModFit, predDf)

sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((compPred-testing$wage)^2))

pred1V <- predict(mod1, validation); pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1=pred1V, pred2=pred2V)
combPredV <- predict(combModFit, predVDF)

sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))


#### Forecasting (Time Series)
#################################################################
library(quantmod)
from.date <- as.Date("01/01/08", format="%m/%d/%y")
to.date <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.date, to = to.date)

head(GOOG)

mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency=12)
plot(ts1, xlab="Years+1", ylab="GOOG")


### Unsupervised prediction
##################################################################
data(iris); library(ggplot2)
names(iris)
table(iris$Species)

library(caret)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

kMeans1 <- kmeans(subset(training, select=-c(Species)), centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour=clusters, data=training)

table(kMeans1$cluster, training$Species)

modFit <- train(clusters ~., dat=subset(training, select=-c(Species)), method="rpart")
table(predict(modFit, training), training$Species)

testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)
