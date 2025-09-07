# Install and load necessary packages
library(ISLR2)
library(dplyr)
library(ggplot2)
library(splines)

# Load the Boston dataset
data("Boston")
View(Boston)
response_var <- "medv"
#linear model
linear_model <- lm(medv ~ ., data = Boston)
summary(linear_model)
degree <- 2  # degree =2 needed

poly_model <- lm(medv ~ poly(lstat, degree), data = Boston)
summary(poly_model)
anova(linear_model, poly_model)


cv_errors <- numeric(10)

# Fit models for different degrees
for (i in 1:10) {
  poly_model <- lm(medv ~ poly(lstat, i), data = Boston)
  cv_errors[i] <- mean((Boston$medv - predict(poly_model, Boston))^2)
}

# Plot cross-validation errors
plot(1:10, cv_errors, type = "b", xlab = "Degree", ylab = "CV Error")

#find best poly
best_degree<- which.min(cv_errors)
best_cv_error<-min(cv_errors)
print("best degree of polynomial",best_degree)
best_degree
print("corresponding cv_error",best_cv_error)
best_cv_error

#splines
# Fit a spline with varying knots
knots <- quantile(Boston$lstat, c(0.25, 0.5, 0.75))
spline_fit <- lm(medv ~ bs(lstat, knots = knots), data = Boston)

# Predictions for the spline
lstat_grid <- seq(min(Boston$lstat), max(Boston$lstat), length.out = 100)
pred_spline <- predict(spline_fit, newdata = list(lstat = lstat_grid), se = TRUE)

se_bands_spline <- with(pred_spline, cbind("upper" = fit + 2 * se.fit,
                                           "lower" = fit - 2 * se.fit))

# Plot the spline and error bands
ggplot() +
  geom_point(data = Boston, aes(x = lstat, y = medv)) +
  geom_line(aes(x = lstat_grid, y = pred_spline$fit)) +
  geom_ribbon(aes(x = lstat_grid, ymin = se_bands_spline[, "lower"], ymax = se_bands_spline[, "upper"]), alpha = 0.3) +
  labs(title = "Spline with Varying Knots") +
  xlim(range(Boston$lstat))
# Fit a GAM model
gam_model <- gam(medv ~ s(lstat, k = 5) + s(rm, k = 5) + s(age, k = 5), data = Boston)

# Summary of the GAM model
summary(gam_model)

# Plot GAM diagnostics
plot(gam_model, se = TRUE, col = "blue")
