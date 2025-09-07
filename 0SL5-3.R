library(MASS)
library(boot)

set.seed(2)

# Randomly sample 380 (75%) out of 506 observations
train = sample(506,380)

dim(Boston)
head(Boston)
# View(Boston)
attach(Boston)

Boston.tr <- Boston[train,]
dim(Boston.tr)
head(Boston.tr)

# Fit linear regression model on training data
lm.fit <- lm(medv ~ rm, data = Boston, subset = train)
summary(lm.fit)

# Calculate MSE for validation set
# attach(Boston)
Boston.test <- Boston[-train,]

Boston.new <- data.frame(rm = Boston.test[,6])
dim(Boston.new)

medv_pred <- predict(lm.fit, newdata = Boston.new)

MSE <- mean((Boston.test$medv - medv_pred)[-train]^2)
MSE

# Model 2 - Polynomial Regression model
lm.fit2 <- lm(medv ~ poly(rm,2), data = Boston, subset = train)
summary(lm.fit2)

MSE2 <- mean((medv - predict(lm.fit2, Boston))[-train]^2)
MSE2

# Model 3 - Polynomial Regression model
lm.fit3 <- lm(medv ~ poly(rm,3), data = Boston, subset = train)
summary(lm.fit3)

MSE3 <- mean((medv - predict(lm.fit3, Boston))[-train]^2)
MSE3

# GLM with no argument is same as LM
glm.fit <- glm(medv ~ rm, data = Boston)
summary(glm.fit)

cv.err <- cv.glm(Boston, glm.fit)
cv.err$delta

# Fit linear models of order 1-5
cv.error = rep(0,5)
cv.error
for(i in 1:5){
  glm.fit <- glm(medv ~ poly(rm,i), data = Boston)
  cv.error[i] <- cv.glm(Boston, glm.fit)$delta[2]
}
cv.error

library(ggplot2)
plot(seq(1,5), cv.error, xlab = 'Order', ylab = 'CV Error', type = 'o')

# K-Fold CV
cv.error.5 = rep(0,5)
cv.error.5
for(i in 1:5){
  glm.fit <- glm(medv ~ poly(rm,i), data = Boston)
  cv.error.5[i] <- cv.glm(Boston, glm.fit, K = 5)$delta[2]
}
cv.error.5

plot(seq(1,5), cv.error.5, xlab = 'Order', ylab = 'CV Error', type = 'o')

# Bootstrapping
boot.fn <- function(data, index){
  lm.fit <- lm(medv ~ rm, data = Boston, subset = index)
  return(coef(lm.fit))
}

boot.fn(Boston, 1:400)
boot(Boston, boot.fn, 1000)
