library(e1071)
library(MASS)
# library(MLmetrics)

# Data
dataset <- as.data.frame(Boston)
dataset

# Splitting the dataset
library(caTools)
split <- sample.split(dataset$medv, SplitRatio = 0.75)
trainset <- subset(dataset, split == TRUE)
testset <- subset(dataset, split == FALSE)

# Model fitting
svm.fit <- svm(medv ~ ., data = trainset, kernel = 'linear', cost = 10, scale = FALSE)
summary(svm.fit)

# R^2 value
ypred_train <- predict(svm.fit, trainset)
R2_Score(ypred_train, trainset$medv)

# Metrics
MSE(ypred_train, trainset$medv)
MAE(ypred_train, trainset$medv)
MAPE(ypred_train, trainset$medv)

# Model after tuning
set.seed(1)
tune.out <- tune(svm, medv ~ ., data = trainset, kernel = 'linear') 
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
bestmod$index # Support vectors

# R^2 value
ypred_train2 <- predict(bestmod, trainset)
R2_Score(ypred_train2, trainset$medv)

# Metrics
MSE(ypred_train2, trainset$medv)
MAE(ypred_train2, trainset$medv)
MAPE(ypred_train2, trainset$medv)

