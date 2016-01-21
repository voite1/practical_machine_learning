setwd('C:\\Users\\db345c\\Desktop\\Practical Machine Learning\\Week2')


######################################################################################
# LECTUR1: Caret Package  ############################################################
######################################################################################
library(caret)
library(kernlab)

data(spam)

# partition data 0.75 for training, 0.25 for testing, y is the variable to split on
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

# create training and testing subsets
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# Display training
dim(training)


# Building a model
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")

# Display the fit
modelFit

# Look at the final model of the fit
modelFit$finalModel

# Generate Predictions
predictions <- predict(modelFit, newdata=testing)

# Display predictions
predictions

# Build Confusion matrix
confusionMatrix(predictions, testing$type)

######################################################################################
# Data Slicing/cross validation ######################################################
######################################################################################
library(caret)
library(kernlab)

data(spam)

######################################################################################
# partition data 0.75 for training, 0.25 for testing, y is the variable to split on ##
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

# create training and testing subsets
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# Display training
dim(training)

######################################################################################
set.seed(32323)
# K-fold with 10-folds
# This can return return training set (returnTrain = TRUE) ###########################
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = TRUE)

# check the length of each folds
sapply(folds, length)

# Look at the first 10 elements of the first fold
folds[[1]][1:10]

######################################################################################
set.seed(32323)
# K-fold with 10 folds
# This can return return training set (returnTrain = FALSE)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = FALSE)

# Check the length of the testing set
sapply(folds, length)

######################################################################################
set.seed(32323)
# Resampling or Bootstrapping
# resample with replacement with 10-folds
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)

folds[[1]][1:10]

######################################################################################
set.seed(32323)
# time slicing
tme <- 1:1000

# continious value in time with 20 sample windows (predict next 10 samples out)
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon=10)

# check the names in the folds list
names(folds)

# Look at the first train slice
folds$train[[1]]

# Look at the first test slice
folds$test[[1]]
