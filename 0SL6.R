library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)
library(MASS)

# Load Boston dataset
data(Boston)
# Ensure no missing values
Boston = na.omit(Boston)

# Set up data, i.e., x and y variables
x = model.matrix(medv~., Boston)[,-1] 
y = Boston %>%
  dplyr::select(medv) %>%
  unlist() %>%
  as.numeric()

# Ridge
grid = 10^seq(10, -2, length = 100)
ridge_mod = glmnet(x, y, alpha = 0, lambda = grid)

# Draw plot of coefficients vs. L2 norm
plot(ridge_mod, xvar = "lambda", label = TRUE)

# Draw plot of MSE vs log lambda
cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

# Estimate test error
set.seed(1)

train = Boston %>%
  sample_frac(0.5)

test = Boston %>%
  setdiff(train)

x_train = model.matrix(medv~., train)[,-1]
x_test = model.matrix(medv~., test)[,-1]

y_train = train %>% 
  dplyr::select(medv) %>%
  unlist() %>%
  as.numeric()

y_test = test %>% 
  dplyr::select(medv) %>%
  unlist() %>%
  as.numeric()

ridge_mod = glmnet(x_train, y_train, alpha = 0, lambda = grid)
ridge_pred = predict(ridge_mod, s = cv.ridge$lambda.min, newx = x_test)
ridge_mse = mean((ridge_pred - y_test)^2)
ridge_mse

# Lasso
lasso_mod = glmnet(x, y, alpha = 1, lambda = grid)

# Draw plot of coefficients
plot(lasso_mod)

# Fit lasso using cross-validation
cv.out = cv.glmnet(x, y, alpha = 1)
plot(cv.out)

bestlam = cv.out$lambda.min

# Fit lasso with selected lambda
lasso_final = glmnet(x, y, alpha = 1, lambda = bestlam)

# Estimate test error
lasso_pred = predict(lasso_final, newx = x_test)
lasso_mse = mean((lasso_pred - y_test)^2)
lasso_mse
