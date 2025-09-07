library(e1071)
dataset <- as.data.frame(iris)
dataset

library(caTools)
split <- sample.split(dataset$Species, SplitRatio = 0.75)
trainset <- subset(dataset, split == TRUE)
testset <- subset(dataset, split == FALSE)

svm.fit <- svm(Species ~ ., data = trainset, kernel = 'linear', cost = 10, scale = FALSE)
summary(svm.fit)

# plot the model
plot(svm.fit, data = trainset, Petal.Width ~ Petal.Length)

# Confusion matrix
ypred <- predict(svm.fit, testset)
table(predict = ypred, truth = testset$Species)

# Metrics
ConfusionMatrix(ypred, testset$Species)
Accuracy(ypred, testset$Species)
Precision(ypred, testset$Species)
Recall(ypred, testset$Species)
F1_Score(ypred, testset$Species)

# Radial kernel

svm.fit2 <- svm(Species ~ ., data = trainset, kernel = 'radial', cost = 10, scale = FALSE)
summary(svm.fit2)

# plot the model
plot(svm.fit2, data = trainset, Petal.Width ~ Petal.Length)

# Confusion matrix
ypred2 <- predict(svm.fit2, testset)
table(predict = ypred2, truth = testset$Species)

# Metrics
ConfusionMatrix(ypred2, testset$Species)
Accuracy(ypred2, testset$Species)
Precision(ypred2, testset$Species)
Recall(ypred2, testset$Species)
F1_Score(ypred2, testset$Species)

# Polynomial kernel

svm.fit3 <- svm(Species ~ ., data = trainset, kernel = 'polynomial', cost = 10, scale = FALSE)
summary(svm.fit3)

# plot the model
plot(svm.fit3, data = trainset, Petal.Width ~ Petal.Length)

# Confusion matrix
ypred3 <- predict(svm.fit3, testset)
table(predict = ypred3, truth = testset$Species)

# Metrics
ConfusionMatrix(ypred3, testset$Species)
Accuracy(ypred3, testset$Species)
Precision(ypred3, testset$Species)
Recall(ypred3, testset$Species)
F1_Score(ypred3, testset$Species)
