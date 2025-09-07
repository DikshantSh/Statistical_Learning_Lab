library(ggplot2)
library(catdata)

data(heart)
head(heart)
summary(heart)
dim(heart) # 462 obs, 10 cols

heart
?heart

# Boxplot
# sbp tobacco ldl adiposity famhist typea obesity alcohol age

Heartdata <- as.data.frame(heart)
ggplot(Heartdata, aes(x = factor(y), y = sbp, fill = factor(y)))+geom_boxplot()
ggplot(Heartdata, aes(x = factor(y), y = tobacco, fill = factor(y)))+geom_boxplot()
ggplot(Heartdata, aes(x = factor(y), y = ldl, fill = factor(y)))+geom_boxplot()
ggplot(Heartdata, aes(x = factor(y), y = adiposity, fill = factor(y)))+geom_boxplot()
ggplot(Heartdata, aes(x = factor(y), y = typea, fill = factor(y)))+geom_boxplot()
ggplot(Heartdata, aes(x = factor(y), y = obesity, fill = factor(y)))+geom_boxplot()
ggplot(Heartdata, aes(x = factor(y), y = alcohol, fill = factor(y)))+geom_boxplot()
ggplot(Heartdata, aes(x = factor(y), y = age, fill = factor(y)))+geom_boxplot()

# Histogram
hist(Heartdata$age, main = 'Age Distribution', xlab = 'Age')

# Dimension of data
dim(Heartdata)

# Randomly divide in training and test data
trn <- sample(dim(Heartdata)[1], 370)
trn
heart_train <- Heartdata[trn,]
heart_test <- Heartdata[-trn,]
heart_test <- heart_test[,-1]

# Fit the LogReg model
glm.logit <- glm(y ~  tobacco + age + obesity, data = heart_train, family = binomial(link = "logit"))
summary(glm.logit)

glm.logit1 <- glm(y ~  .-adiposity-alcohol-sbp-obesity-ldl-typea, data = heart_train, family = binomial(link = "logit"))
summary(glm.logit1)

glm.logit2 <- glm(y ~  ., data = heart_train, family = binomial(link = "logit"))
summary(glm.logit2)

# Log Likelihood
L1 <- logLik(glm.logit1)
L1
L0 <- logLik(glm.logit2)
L0

# Deviance
dev <- 2*(L1[1]-L0[1])
dev

# Chi-square
qchisq(0.95,1)

# dev < chisq, so logit1 not more informative

# predict using test data set
pred <- predict(glm.logit1, heart_test, type = 'response')
pred[1:5]

# classify the prediction in default = 1 or 0
pred_class <- ifelse(pred >= 0.5, 1, 0)
pred_class[1:5]

# Create confusion matrix
cm <- table(Obs = Heartdata[-trn,]$y, Pred = pred_class)

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
table(heart_train$y)

# Test accuracy
mean(pred_class == Heartdata[-trn,]$y)

