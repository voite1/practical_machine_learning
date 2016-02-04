# load required libraries
library(caret)
library(rpart)
library(randomForest)
# Set appropriate working directory
# setwd('C:\\Users\\db345c\\Desktop\\Practical Machine Learning\\Final_Project')
setwd('C:\\Users\\Aleksey\\Documents\\School\\coursera\\Practical_Machine_Learning\\Final_Project')

# Check if the files exist locally, if not - download
training.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists("pml-training.csv")) {
    download.file(training.url, "pml-training.csv")
}

if(!file.exists("pml-testing.csv")){
    download.file(testing.url, "pml-testing.csv")
}

######################## LOAD DATA ###########################################

# Load training and testing data sets
training <- read.csv("pml-training.csv", na.strings = c("NA","", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings = c("NA","", "#DIV/0!"))

# cleanup unused variables
rm(training.url, testing.url)

######################## CLEANUP DATA ########################################

# identifying sparsely populated columns using nearZeroVar function
# from testing and training data sets
# Identical changes need to be made to testing and training data sets
to_skip <- nearZeroVar(training)
training <- training[, -to_skip]
testing <- testing[, -to_skip]

# cleanup unused variables
rm(to_skip)

# removing columns containing NA's only
# Identical changes need to be made to testing and training data sets
col_lst <- colSums(is.na(training)) == 0
training <- training[, col_lst]
testing <- testing[, col_lst]

# adjusting data - remove index columns from training and testing data set
testing <- testing[, -c(59)]
# creating simple dummy data with 5 levels identical to those in training data set
v = c('A', 'B', 'C', 'D', 'E', 'A', 'B', 'C', 'D', 'E', 'A', 'B', 'C', 'D', 'E', 'A', 'B', 'C', 'D', 'E')
testing$classe <- as.factor(v)
testing <- testing[, -c(1:6)]
training <- training[, -c(1:6)]

# cleanup unused variables
rm(col_lst, v)

# coerse training and testing data to the same data types and fix factors in testing
for (i in 1:length(testing)) {
  for(j in 1:length(training)) {
    if( length(grep(names(training[i]), names(testing)[j])) == 1)  {
      class(testing[j]) <- class(training[i])
    }      
  }      
}

# fix coersion that failed in previous step
for (i in 1:length(testing)) {
  if (class(training[, i]) != class(testing[, i])) {
    ### try fixing coersion, so testing and training data frames match
    class(training[, i]) <- class(testing[, i])
  }
}

# Partition data into two two sets - training and testing
# Identical changes need to be made to testing and training data sets
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
my_training <- training[inTrain,]
my_testing <- training[-inTrain,]

# cleanup unused variables
rm(inTrain, i, j)

################ CHECK if COLUMN CLASSES ARE OF THE SAME TYPE #################

# confirm tata types are the same between testing and my_testing
for (i in 1:length(testing)) {
  if (class(testing[, i]) != class(my_testing[, i])) {
    print(i)
    print(class(my_testing[, i]))
    print(class(testing[, i]))
  }
}

# confirm tata types are the same between testing and my_training
for (i in 1:length(testing)) {
  if (class(testing[, i]) != class(my_training[, i])) {
    print(i)
    print(class(my_training[, i]))
    print(class(testing[, i]))
  }
}

# cleanup unused vairables
rm(i)

######################## BUILD MODELS #########################################

# Build decision tree model
set.seed(212121)
treeFit <- rpart(classe ~., data=my_training, method="class")
treePredicted <- predict(treeFit, my_testing, type="class")
confusionMatrix(treePredicted, my_testing$classe)

# Build random forest model
set.seed(212121)
rfFit <- randomForest(classe ~., data=my_training)
rfPredicted <- predict(rfFit, my_testing, type="class")
confusionMatrix(rfPredicted, my_testing$classe)

finalAnswer <- predict(rfFit, testing, type="class")

############### PREDICTION ON THE TEST DATA SUPPLIED BY TEACHER ################

# printing final answer
print(finalAnswer)
