# set working directory
setwd("/Users/mac/Dropbox/coursera/practical machine learning/project")

library(doMC)
registerDoMC(2)
library(caret)
library(randomForest)

# read data
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

# check training data
head(training, n = 3)
dim(training)
str(training)
summary(training)

# check testing data
head(testing, n = 3)
dim(testing)
str(testing)
summary(testing)

# check missing values
colSums(is.na(training))
colSums(is.na(testing))

# remove columns with less than 80% complete data in testing samples
testing_complete <- testing[colSums(is.na(testing)) < 0.2 * nrow(testing)]
# remove column X, cvtd_timestamp, new_window and problem_id
testing_complete <- testing_complete[,-c(1,5,6,60)]
dim(testing_complete)    # 20 56

# remove corresponding columns in training samples
training_complete <- training[colSums(is.na(testing)) < 0.2 * nrow(testing)]
# remove column X, cvtd_timestamp, new_window.
training_complete <- training_complete[,-c(1,5,6)]
dim(training_complete)   # 19622 57

# create training and validation sets
set.seed(1985)
inTrain <- createDataPartition(y = training_complete$classe, p = 0.7, list = FALSE)
trainSet <- training_complete[inTrain, ]            
validationSet <- training_complete[-inTrain, ]      

# random forest
#modFit <- train(classe ~ . , data = trainSet, method = "rf", prox= TRUE, importance = TRUE, trainControl = trainControl(method = "oob")) #need to install `randomForest' package
modFit <- randomForest(classe ~., data = trainSet, importance = TRUE, proximity = TRUE)
# summarize model
summary(modFit)
print(modFit)

# assess importance of predictors
round(importance(modFit),2)
varImpPlot(modFit, main = "Variable Importance Plot")

# evaluate model at validation set
validationPred<- predict(object = modFit, newdata = validationSet[,-59])
confusionMatrix(validationPred, validationSet$classe)

# predict testing set
testingPred <- predict(object = modFit, newdata = testing_complete)

# store predictions as a character vector
answers <- as.character(testingPred)

# create submission text files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
