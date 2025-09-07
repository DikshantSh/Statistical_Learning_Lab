# Load necessary libraries
library(rpart)
library(rpart.plot)
library(gbm)
library(randomForest)
library(MLmetrics)

# Import the dataset
data(iris)
head(iris)

# Data Preprocessing
# Splitting the data
sample <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE, prob = c(0.7,0.3))
train <- iris[sample,]
test <- iris[!sample,]

### CLASSIFICATION TREE
# For classification, we will use the column 'Species' as the response variable
# Training the model
tree.iris <- rpart(Species ~ ., data = train, method = 'class')
summary(tree.iris)
tree.iris
rpart.plot(tree.iris)

ypred <- predict(tree.iris, test, type = 'class')

# Metrics
ConfusionMatrix(ypred, test$Species)
Accuracy(ypred, test$Species)

# Pruning
plotcp(tree.iris)
tree.iris$cptable
index <- which.min(tree.iris$cptable[,'xerror'])
cpopt <- tree.iris$cptable[index, "CP"]
cpopt
opttree.iris <- prune(tree.iris, cp = cpopt)
rpart.plot(opttree.iris)

# Metrics
ConfusionMatrix(ypred, test$Species)
Accuracy(ypred, test$Species)

### REGRESSION TREE
# For regression, we will omit the 'Species' column and choose one of the continuous 
# variables as the response variable (Petal.Width)

# Training the model
tree.irisreg <- rpart(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = train, method = 'anova')
summary(tree.irisreg)
tree.irisreg
rpart.plot(tree.irisreg)

# Metrics
ypred <- predict(tree.irisreg, test)
ypred
MAE(ypred, test$Petal.Width)
MSE(ypred, test$Petal.Width)
RMSE(ypred, test$Petal.Width)
MAPE(ypred, test$Petal.Width)

# Pruning
plotcp(tree.irisreg)
tree.irisreg$cptable
index <- which.min(tree.irisreg$cptable[,'xerror'])
cpopt <- tree.irisreg$cptable[index, "CP"]
cpopt
opttree.irisreg <- prune(tree.irisreg, cp = cpopt)
rpart.plot(opttree.irisreg)

# Metrics
ypred <- predict(opttree.irisreg, test)
MAE(ypred, test$Petal.Width)
MSE(ypred, test$Petal.Width)
RMSE(ypred, test$Petal.Width)
MAPE(ypred, test$Petal.Width)

### BOOSTED MODEL
# Training the model
boost.iris <- gbm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = train, distribution = 'gaussian', n.trees = 5000, interaction.depth = 4)
boost.iris
summary(boost.iris)

# Partial Importance Plot
par(mfrow = c(1,2))
plot(boost.iris, i = 'Petal.Length')
plot(boost.iris, i = 'Sepal.Length')

# Metrics
ypred <- predict(boost.iris, test)

MAE(ypred, test$Petal.Width)
MSE(ypred, test$Petal.Width)
RMSE(ypred, test$Petal.Width)
MAPE(ypred, test$Petal.Width)

### RANDOM FOREST MODEL
# Training the model with mtry = 2
rf.iris <- randomForest(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = train, mtry = 2, importance = TRUE)
rf.iris

# Plot
ypred <- predict(rf.iris, test)
plot(ypred, test$Petal.Width)
abline(0,1)

# Metrics
MAE(ypred, test$Petal.Width)
MSE(ypred, test$Petal.Width)
RMSE(ypred, test$Petal.Width)
MAPE(ypred, test$Petal.Width)


# Training the model with ntree = 25
rf.iris <- randomForest(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = train, mtry = 2, ntree = 25, importance = TRUE)
rf.iris

# Plot
ypred <- predict(rf.iris, test)
plot(ypred, test$Petal.Width)
abline(0,1)

# Metrics
MAE(ypred, test$Petal.Width)
MSE(ypred, test$Petal.Width)
RMSE(ypred, test$Petal.Width)
MAPE(ypred, test$Petal.Width)

# Variable importance
importance(rf.iris)
varImpPlot(rf.iris)

# Training the model with mtry = 3
rf.iris <- randomForest(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length, data = train, mtry = 3, importance = TRUE)
rf.iris

# Plot
ypred <- predict(rf.iris, test)
plot(ypred, test$Petal.Width)
abline(0,1)

# Metrics
MAE(ypred, test$Petal.Width)
MSE(ypred, test$Petal.Width)
RMSE(ypred, test$Petal.Width)
MAPE(ypred, test$Petal.Width)

# Variable importance
importance(rf.iris)
varImpPlot(rf.iris)
