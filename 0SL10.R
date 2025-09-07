install.packages("rpart")
install.packages("rpart.plot")
library(catdata)
library(rpart)
library(rpart.plot)
library(MLmetrics)

# CLASSIFICATION

# Heart Data
data(heart)
?heart

Heartdata <- as.data.frame(heart)
Heartdata

# Splitting the data
sample <- sample(c(TRUE, FALSE), nrow(Heartdata), replace = TRUE, prob = c(0.7,0.3))
train <- Heartdata[sample,]
test <- Heartdata[!sample,]

# Training the model
tree.heart <- rpart(y ~ sbp + tobacco + ldl + adiposity + factor(famhist) + typea + obesity + alcohol + age, data = train, method = 'class')
summary(tree.heart)

tree.heart

rpart.plot(tree.heart)

# Confusion matrix
ypred <- predict(tree.heart, test, type = 'class')
ypred
table(predict = ypred, truth = test$y)

# Metrics
ConfusionMatrix(ypred, test$y)
Accuracy(ypred, test$y)
Precision(ypred, test$y)
Recall(ypred, test$y)
F1_Score(ypred, test$y)

# Pruning
plotcp(tree.heart)
tree.heart$cptable
index <- which.min(tree.heart$cptable[,'xerror'])
cpopt <- tree.heart$cptable[index, "CP"]
cpopt
opttree.heart <- prune(tree.heart, cp = cpopt)
rpart.plot(opttree.heart)

# Confusion matrix
ypred <- predict(opttree.heart, test, type = 'class')
ypred
table(predict = ypred, truth = test$y)

# Metrics
ConfusionMatrix(ypred, test$y)
Accuracy(ypred, test$y)
Precision(ypred, test$y)
Recall(ypred, test$y)
F1_Score(ypred, test$y)


# REGRESSION
library(MASS)
data(Boston)
dataset <- as.data.frame(Boston)
dataset

# Splitting the data
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace = TRUE, prob = c(0.7,0.3))
train <- dataset[sample,]
test <- dataset[!sample,]

# Training the model
tree.boston <- rpart(medv ~ ., data = train, method = 'anova')
summary(tree.boston)
tree.boston
rpart.plot(tree.boston)

# Metrics
ypred <- predict(tree.boston, test)
ypred
MAE(ypred, test$medv)
MSE(ypred, test$medv)
RMSE(ypred, test$medv)
MAPE(ypred, test$medv)

# Pruning
plotcp(tree.boston)
tree.boston$cptable
index <- which.min(tree.boston$cptable[,'xerror'])
cpopt <- tree.boston$cptable[index, "CP"]
cpopt
opttree.boston <- prune(tree.boston, cp = cpopt)
rpart.plot(opttree.boston)
