# Week 3 - Logistic Regression

library(ISLR2)
library(dplyr)
library(boot)
library(MASS)
library(ggplot2)

head(Default)

dim(Default)

# Randomly divide in training and test data
trn <- sample(dim(Default)[1], 8000)
trn
Default_train <- Default[trn,]
Default_test <- Default[-trn,]
Default_test <- Default_test[,-1]
head(Default_test)

dim(Default_train)

# fit logistic regression model
# Model 1
glm.logit <- glm(default ~  balance + income, data = Default_train, family = binomial(link = "logit"))
summary(glm.logit)

# glm.probit <- glm(default ~  balance + income, data = Default_train, family = binomial(link = "probit"))
# summary(glm.probit)

# Model 2
glm.logit1 <- glm(default ~  balance, data = Default_train, family = binomial(link = "logit"))
summary(glm.logit1)

# Log Likelihood
L1 <- logLik(glm.logit)
L1
L0 <- logLik(glm.logit1)
L0

# Deviance
dev <- 2*(L1[1]-L0[1])
dev

# Chi-square
qchisq(0.95,1)

# dev > chisq, so balance + income is more informative (significantly)

# Model 3
glm.logit2 <- glm(default ~  balance + income + student, data = Default_train, family = binomial(link = "logit"))
summary(glm.logit2)

# Model 4
glm.logit3 <- glm(default ~  balance + student, data = Default_train, family = binomial(link = "logit"))
summary(glm.logit3)

# predict using test data set
pred <- predict(glm.logit3, Default_test, type = 'response')
pred[1:5]

# classify the prediction in default = 'Yes' or 'No'
pred_class <- ifelse(pred >= 0.5, 'Yes', 'No')
pred_class[1:5]

# Create confusion matrix
cm <- table(Obs = Default[-trn,]$default, Pred = pred_class)

TP <- cm[2,2]
TP
FP <- cm[1,2]
FP
TN <- cm[1,1]
FN <- cm[2,1]

sens <- TP/(TP+FN)
sens

spec <- TN/(TN+FP)
spec

f1 <- 2*TP/(2*TP+FP+FN)
f1

# Number of values in each class in default
table(Default_train$default)

# Test accuracy
mean(pred_class == Default[-trn,]$default)

contrasts(Default$default)

?predict.glm
? cv.glm


head(Default)
attach(Default)
# EDA using boxplot
boxplot(balance ~ default)
boxplot(income ~ default)

ggplot(Default, aes(x = default, y = balance, fill = default))+geom_boxplot()

ggplot(Default, aes(x = income, y = balance, col = default))+geom_point()

