library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)
Hitters = na.omit(Hitters)

# Set up data, i.e., x and y variables
x = model.matrix(Salary~., Hitters)[,-1] 

y = Hitters %>%
  select(Salary) %>%
  unlist() %>%
  as.numeric()


# Ridge
grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge_mod))

plot(ridge_mod, xvar = 'lambda', lable = TRUE)
plot(ridge_mod) # Draw plot of coefficients

cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

ridge_mod$lambda[50] # Display 50th lambda value
coef(ridge_mod)[,50]

ridge_mod$lambda[60]
coef(ridge_mod)[,60]


# estimate test error
set.seed(1)

train = Hitters %>%
  sample_frac(0.5)

test = Hitters %>%
  setdiff(train)

x_train = model.matrix(Salary~., train)[,-1]
x_test = model.matrix(Salary~., test)[,-1]

y_train = train %>% 
  select(Salary) %>%
  unlist() %>%
  as.numeric()

y_test = test %>% 
  select(Salary) %>%
  unlist() %>%
  as.numeric()

ridge_mod = glmnet(x_train, y_train, alpha = 0, lambda = grid)
ridge_pred = predict(ridge_mod, s = 4, newx = x_test)
mean((ridge_pred - y_test)^2)
plot(ridge_mod, xvar = 'lambda', label = TRUE)

mean((mean(y_train) - y_test)^2) # Calculate MSE

# With large value of lambda, the same values we get
ridge_pred = predict(ridge_mod, s = 1e10, newx = x_test)
mean((ridge_pred - y_test)^2)

# Check with least square
ridge_pred = predict(ridge_mod, s = 0, newx = x_test)
mean((ridge_pred - y_test)^2)

lm(Salary~., data = train)
predict(ridge_mod, s = 0, exact = T, type = 'coefficient')


# selection of tuning parameter lambda
set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha = 0)


# Lasso
lasso_mod = glmnet(x, y, alpha = 1, lambda = grid) # Fit lasso model on train

plot(lasso_mod) # Draw plot of coefficients

set.seed(1)
cv.out = cv.glmnet(x, y, alpha = 1) # Fit lasso
plot(cv.out) # Draw plot of training MSE as a function of

bestlam = cv.out$lambda.min # Select lambda that minimizes
bestlam

out = glmnet(x, y, alpha = 1, lambda )