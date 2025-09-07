library(gbm)
library(MASS)
library(MLmetrics)

## REGRESSION
data(Boston)
dataset <- as.data.frame(Boston)
dataset

# Splitting the data
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.7,0.3))
train <- dataset[sample,]
test <- dataset[!sample,]

# Training the model
boost.boston <- gbm(medv ~ ., data = train, distribution = 'gaussian', n.trees = 5000, interaction.depth = 4)
boost.boston
summary(boost.boston)

# Partial Importance Plot
par(mfrow = c(1,2))
plot(boost.boston, i = 'rm')
plot(boost.boston, i = 'lstat')

# Metrics
ypred <- predict(boost.boston, test)

MAE(ypred, test$medv)
MSE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)

# Training the model with shrinkage = 0.2
