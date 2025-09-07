library(ISLR)
library(ggplot2)
library(boot)

set.seed(2)

# Randomly sample 196 out of 392 observations
train = sample(392,196)

dim(Auto)
head(Auto)
# View(Auto)

Auto.tr <- Auto[train,]
dim(Auto.tr)
head(Auto.tr)

# Fit linear regression model on training data
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
summary(lm.fit)

# Calculate MSE for validation set
# attach(Auto)
Auto.test <- Auto[-train,]

Auto.new <- data.frame(horsepower = Auto.test[,4])
dim(Auto.new)

mpg_pred <- predict(lm.fit, newdata = Auto.new)

MSE <- mean((Auto.test$mpg - mpg_pred)[-train]^2)
MSE

# Model 2 - Polynomial Regression model
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
summary(lm.fit2)



MSE2 <- mean((mpg - predict(lm.fit2, Auto))[-train]^2)
MSE2

# Model 3 - Polynomial Regression model
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
summary(lm.fit3)

MSE3 <- mean((mpg - predict(lm.fit3, Auto))[-train]^2)
MSE3

# GLM with no argument is same as LM
glm.fit <- glm(mpg ~ horsepower, data = Auto)
summary(glm.fit)

cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Fit linear models of order 1-5
cv.error = rep(0,5)
cv.error
for(i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[2]
}
cv.error

plot(seq(1,5), cv.error, xlab = 'Order', ylab = 'CV Error', type = 'o')

 # K-Fold CV
cv.error.5 = rep(0,5)
cv.error.5
for(i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error.5[i] <- cv.glm(Auto, glm.fit, K = 5)$delta[2]
}
cv.error.5

cv.error.10 = rep(0,10)
cv.error.10
for(i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[2]
}
cv.error.10

plot(seq(1,10), cv.error.10, xlab = 'Order', ylab = 'CV Error', type = 'o')

# Bootstrapping
# Var (alpha X + (1 - alpha) Y)

alpha.fn <- function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}

head(Portfolio)

alpha.fn(Portfolio, 1:100)

boot(Portfolio, alpha.fn, R = 1000)

boot.fn <- function(data, index){
  lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = index)
  return(coef(lm.fit))
}

boot.fn(Auto, 1:196)
boot(Auto, boot.fn, 1000)

# Assignment, library(MASS), use boston dataset [target- medv]
# Fit medv with number of rooms, for linear, poly 2 - 5
# Compare MSE with LOOCV, K- fold for K = 5
# Bootstrap with index [1:400] for R = 1000
# and determine parametric uncertainity