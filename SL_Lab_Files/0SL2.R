# Linear Regression Using R

# Generate random data for X and Y
X <- rnorm(100, 5, 1)
Y <- 100 + 2*X + rnorm(100, 2, 0.5)

# Fit a linear regression model
m1 <- lm(Y ~ X)
summary(m1)  # Summary of the regression model

anova(m1)  # Analysis of Variance (ANOVA) table for the regression model

# Plot X against Y with the regression line
plot(X, Y)
abline(lm(Y ~ X), col = 'red')  

# Calculate residuals
residuals(m1)  # Residuals of the model

# Normal probability plot of residuals
qqnorm(residuals(m1))

# Predicted values
yhat <- predict(m1)
resid.calc <- Y - yhat

# Residual plot
plot(yhat, resid.calc)

# Scatter plot of predicted Y values against residuals
plot(yhat, residuals(m1), xlab = 'Predicted Y', ylab = 'Residuals')

# Generate new data for prediction
xnew <- data.frame(xnew = rnorm(25, 5, 1))
ynew <- predict(m1, newdata = xnew)

# Install and load required packages
install.packages('car')
library(car)

# Load mtcars dataset
data(mtcars)

# Select relevant variables from mtcars dataset
library(dplyr)
mt1 <- mtcars %>% select(mpg, disp, hp, drat, wt, qsec)

# Create scatter plot matrix
pairs(mt1)

# Heatmap of correlation matrix
cmt1 <- cor(mt1)
heatmap(cmt1)

# Load corrplot package
library(corrplot)

# Create correlation plot
corrplot(cmt1)

# Fit a linear regression model using all variables
m2 <- lm(mpg ~ ., data = mt1)
summary(m2)  # Summary of the regression model

# Calculate Variance Inflation Factor (VIF)
vif(m2)

# Fit a linear regression model using selected variables
m3 <- lm(mpg ~ hp + drat + wt + qsec, data = mt1)
summary(m3)  # Summary of the regression model

# Calculate Variance Inflation Factor (VIF)
vif(m3)

# Fit a linear regression model removing one variable
m4 <- lm(mpg ~ . - disp, data = mt1)
summary(m4)  # Summary of the regression model

# Calculate Variance Inflation Factor (VIF)
vif(m4)

# Fit a linear regression model removing two variables
m5 <- lm(mpg ~ . - disp - hp, data = mt1)
summary(m5)  # Summary of the regression model

# Calculate Variance Inflation Factor (VIF)
vif(m5)

# ANOVA for model comparison
anova(m4)
