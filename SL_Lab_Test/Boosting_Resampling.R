library(MASS)
library(ggplot2)
library(boot)

set.seed(2)

train = sample(506,380)
train[1:5]
head(Boston)
dim(Boston)

Boston.tr <- Boston[train,]
dim(Boston.tr)
head(Boston.tr)

lm.fit <- lm(medv ~ rm, data = Boston, subset = train)
summary(lm.fit)
attach(Boston)
Boston.test <- Boston[-train,]
dim(Boston.test)
Boston.new <- data.frame(rm = Boston.test[, 6])
dim(Boston.new)

medv_pred <- predict(lm.fit, newdata=Boston.new)

MSE <- mean((Boston.test$medv-medv_pred)^2)
MSE
#mean((observed-predict)(only for test data))
mean((medv - predict(lm.fit, Boston))[-train]^2)

#we can check residuals
medv - predict(lm.fit, Boston)[-train]

lm.fit2 <- lm(medv ~ poly(rm, 2), data = Boston, subset = train)
summary(lm.fit2)
MSE2 <- mean((medv - predict(lm.fit2, Boston))[-train]^2)
MSE2

plot(Boston$rm, Boston$medv)
#abline(lm.fit2, col="Red")

lm.fit3 <- lm(medv ~ poly(rm, 3), data = Boston, subset = train)
summary(lm.fit3)
MSE3 <- mean((medv - predict(lm.fit3, Boston))[-train]^2)
MSE3


library(boot)
# glm with no family arguement is same as lm
glm.fit <- glm(medv ~ rm, data=Boston)
summary(glm.fit)
cv.err <- cv.glm(Boston, glm.fit)
cv.err$delta  # gives prediction error of cross validation error

# Fit Linear Models of order 5
cv.error = rep(0,5)
cv.error

for(i in 1:5){
  glm.fit <- glm(medv ~ poly(rm, i), data=Boston)
  cv.error[i] <- cv.glm(Boston, glm.fit)$delta[2]
}
cv.error

plot(seq(1,5), cv.error, xlab = "Order", ylab = "CV Error", type = "o")


# k fold cv

set.seed(17)
cv.error.10 = rep(0,10)

for(i in 1:10){
  glm.fit <- glm(medv ~ poly(rm, i), data=Boston)
  cv.error.10[i] <- cv.glm(Boston, glm.fit, K=10)$delta[2]
}
cv.error.10

plot(seq(1,10), cv.error.10, xlab = "Order", ylab = "MSE", type = "o")