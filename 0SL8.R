library(e1071)
library(MLmetrics)

# Training Data Generation
set.seed(1)
x = matrix(rnorm (20*2), ncol = 2)
y = c(rep(-1,10), rep(1,10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = (3-y))
dat = data.frame(X = x, y = as.factor(y))

# Test Data Generation
xtest <- matrix(rnorm (20*2), ncol = 2)
ytest <- sample (c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
testdat <- data.frame(X = xtest, y = as.factor(ytest))

# Model Building
svmfit1 <- svm(y ~ ., data = dat, kernel = 'linear', cost = 10, scale = FALSE)
plot(svmfit1, dat)
summary(svmfit1)

# Support vectors
svmfit1$index

# Confusion matrix
ypred1 <- predict(svmfit1, testdat)
table(predict = ypred1, truth = testdat$y)

# Metrics
ConfusionMatrix(ypred1, testdat$y)
Accuracy(ypred1, testdat$y)
Precision(ypred1, testdat$y)
Recall(ypred1, testdat$y)
F1_Score(ypred1, testdat$y)

# Recall- TP/ (TP + FN) [INCORRECT]
cm <- ConfusionMatrix(ypred1, testdat$y)
TP <- cm[2,2]
FP <- cm[1,2]
TN <- cm[1,1]
FN <- cm[2,1]

TP/(TP + FN) # Recall
TP/(TP + FP) # Precision
(TP + TN)/ (TP + TN + FP + FN) # Accuracy

# Radial kernel
svmfit2 <- svm(y ~ ., data = dat, kernel = 'radial', cost = 10, scale = FALSE)
plot(svmfit2, dat)
summary(svmfit2)

# Support vectors
svmfit2$index

# Confusion matrix
ypred2 <- predict(svmfit2, testdat)
table(predict = ypred2, truth = testdat$y)

# Metrics
ConfusionMatrix(ypred2, testdat$y)
Accuracy(ypred2, testdat$y)
Precision(ypred2, testdat$y)
Recall(ypred2, testdat$y)
F1_Score(ypred2, testdat$y)

# Model after tuning
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = 'linear', ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100))) 
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
bestmod$index # Support vectors

# Confusion matrix
ypred3 <- predict(bestmod, testdat)
table(predict = ypred3, truth = testdat$y)

# Metrics
ConfusionMatrix(ypred3, testdat$y)
Accuracy(ypred3, testdat$y)
Precision(ypred3, testdat$y)
Recall(ypred3, testdat$y)
F1_Score(ypred3, testdat$y)


