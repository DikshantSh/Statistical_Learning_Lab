library(ISLR)
library(MASS)
library(glmnet)
library(dplyr)

# Load Boston dataset and remove rows with missing values
data(Boston)
Boston <- na.omit(Boston)

# View the dataset
View(Boston)

# Prepare the data
x <- model.matrix(medv ~ ., Boston)[, -1]
y <- Boston$medv
# Define the sequence of lambda values
grid <- 10^seq(10, -2, length = 100)

# Fit Ridge Regression
ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid)

# Dimensions of coefficients
dim(coef(ridge_mod))

# Plot the cross-validated mean squared error as a function of lambda
plot(ridge_mod, xvar = "lambda", label = TRUE)

# Plot the coefficients for a specific lambda (lambda 10)
plot(ridge_mod)

# Split data into train and test sets
set.seed(1)
train <- Boston %>% sample_frac(0.5)
test <- Boston %>% setdiff(train)

# Prepare train and test data
x_train <- model.matrix(medv ~ ., train)[, -1]
x_test <- model.matrix(medv ~ ., test)[, -1]

y_train <- train$medv
y_test <- test$medv

# Fit Ridge Regression on train data
ridge_mod <- glmnet(x_train, y_train, alpha = 0, lambda = grid)
# Perform cross-validation for Ridge Regression
cv.out <- cv.glmnet(x, y, alpha = 0, lambda = grid)

# Get the best lambda value
best_lambda_r <- cv.out$lambda.min
print(best_lambda_r)

# Predict on test data for lambda 4
ridge_pred <- predict(ridge_mod, s = best_lambda_r, newx = x_test)
mean((ridge_pred - y_test)^2)

# MSE with mean of train data
mean((mean(y_train) - y_test)^2)

# MSE with a large lambda
ridge_pred <- predict(ridge_mod, s = 1e10, newx = x_test)
mean((ridge_pred - y_test)^2)

# MSE with least squares
ridge_pred <- predict(ridge_mod, s = 0, newx = x_test)
mean((ridge_pred - y_test)^2)

# Fit linear model for comparison
lm_mod <- lm(medv ~ ., data = train)
lm_pred <- predict(lm_mod, newdata = test)
mean((lm_pred - y_test)^2)
# Fit Lasso Regression
lasso_mod <- glmnet(x, y, alpha = 1, lambda = grid)

# Plot the Lasso path
plot(lasso_mod)

# Perform cross-validation
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 1)

# Plot the cross-validation error
plot(cv.out)

# Get the best lambda value
best_lambda <- cv.out$lambda.min
best_lambda

# Split data into train and test sets (for Lasso)
set.seed(1)
train <- Boston %>% sample_frac(0.5)
test <- Boston %>% setdiff(train)

# Prepare train and test data
x_train <- model.matrix(medv ~ ., train)[, -1]
x_test <- model.matrix(medv ~ ., test)[, -1]

y_train <- train$medv
y_test <- test$medv

# Fit Lasso Regression on train data with best lambda
lasso_mod <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

# Predict on test data
lasso_pred <- predict(lasso_mod, s = best_lambda, newx = x_test)
mean((lasso_pred - y_test)^2)
