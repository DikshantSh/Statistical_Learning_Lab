
library(MASS)
head(Boston)
cv.error_b<-rep(0,5)
cv.error_b
# Set the seed for reproducibility
set.seed(123)

# Sample indices for training data (e.g., 70% of the data)
train_index <- sample(1:nrow(Boston), 0.7 * nrow(Boston))

# Create training and testing sets
train_data <- Boston[train_index, ]
test_data <- Boston[-train_index, ]

for(i in 1:5){
  glm.fit<-glm(Boston$medv~ poly(Boston$rm,i),data=train_data)
  cv.error_b[i]<-cv.glm(Boston,glm.fit)$delta[2]
}

#fit Linear Model of order 1-5
cv.error_b
plot(seq(1,5),cv.error_b,xlab="order",ylab="CV Error",type="o")

# Load the Boston dataset
data(Boston)

# Load required libraries
library(boot)

# Function to calculate mean squared error (MSE)
calc_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Define a function to fit the model and calculate MSE
fit_and_calc_mse <- function(data, indices) {
  subset <- data[-indices, ]
  model <- lm(medv ~ ., data = subset)
  preds <- predict(model, newdata = data[indices, ])
  mse <- mean((data$medv[indices] - preds)^2)
  return(mse)
}

# LOOCV for linear regression
loocv_mse_linear <- cv.glm(Boston, fit_and_calc_mse)$delta[1]

# LOOCV for polynomial regression (degree 2-5)
loocv_mse_poly <- sapply(2:5, function(degree) {
  fit_and_calc_mse_poly <- function(data, indices) {
    subset <- data[-indices, ]
    model <- lm(medv ~ poly(chas, degree) + poly(crim, degree) + ..., data = subset)  # Update with your predictors
    preds <- predict(model, newdata = data[indices, ])
    mse <- mean((data$medv[indices] - preds)^2)
    return(mse)
  }
  cv.glm(Boston, fit_and_calc_mse_poly)$delta[1]
})

# Print results
print("Linear Regression:")
print("LOOCV MSE:")
print(loocv_mse_linear)

print("Polynomial Regression:")
print(loocv_mse_poly)
