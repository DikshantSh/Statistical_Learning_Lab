library(tidyverse)
library(e1071)
library(MLmetrics)
library(caTools)

# Loading the data
data <- read_csv("D:\\Academics\\SEM-6\\StatLearning Lab\\archive\\drug200.csv",show_col_types = FALSE)
head(data, 5)

data$Drug <- as.factor(data$Drug)

# Checking for missing values
sum(is.na(data))

# Creating training and test sets
split <- sample.split(data$Drug, SplitRatio = 0.8)
trainset <- subset(data, split == TRUE)
testset <- subset(data, split == FALSE)

# Model Fitting and Evaluation
svmfit <- svm(Drug ~ ., data = trainset, kernel = 'linear', cost = 10, scale = FALSE)
summary(svmfit)

# Predict using the model
predictions <- predict(svmfit, testset)

# Confusion Matrix
ConfusionMatrix(predictions, testset$Drug)

# Accuracy of the model
Accuracy(predictions, testset$Drug)

# Model after tuning
set.seed(1)
tune.out <- tune(svm, Drug ~ ., data = trainset, kernel = 'linear', ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), scale = FALSE) 
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

# Confusion matrix
ypred <- predict(bestmod, testset)
ConfusionMatrix(ypred, testset$Drug)

# Accuracy
Accuracy(ypred, testset$Drug)