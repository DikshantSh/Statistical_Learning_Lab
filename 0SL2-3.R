# Load required libraries
library(MASS)
library(ggplot2)
library(car)
library(dplyr)
library(ISLR)

# Load the airquality dataset
data(airquality)
head(airquality)

# Removing day and month columns from the data
aq <- airquality %>% dplyr::select(Ozone, Solar.R, Wind, Temp)

# Replacing the NULL values from Ozone and Solar.R with the mean value
aq$Ozone[is.na(aq$Ozone)] <- mean(aq$Ozone, na.rm = TRUE)
aq$Solar.R[is.na(aq$Solar.R)] <- mean(aq$Solar.R, na.rm = TRUE)

# Scatter Plot Matrix
plot(aq)

# Summary
summary(aq)

# Correlation Matrix
cor(aq)

# Univariate Normality using QQ Plot for Temp
qqnorm(aq$Temp)
qqline(aq$Temp)

# Simple Linear Regression
model_simple <- lm(Ozone ~ Temp, data = aq)
summary(model_simple)

plot(aq$Temp, aq$Ozone)
abline(model_simple, col='purple')

# Plotting residuals vs fitted values
plot(model_simple$fitted.values, model_simple$residuals)

# Multiple Linear Regression 
model_multiple <- lm(Ozone ~ ., data = aq)
summary(model_multiple)

# Plotting residuals vs fitted values
plot(model_multiple$fitted.values, model_multiple$residuals)

# VIF
vif(model_multiple)

# ANOVA table
anova(model_multiple)

# Backward Stepwise Regression
model_stepwise <- stepAIC(model_multiple, direction='backward', trace=FALSE)
summary(model_stepwise)

# Plotting residuals vs fitted values
plot(model_stepwise$fitted.values, model_stepwise$residuals)

# Cook's Distance
dist <- cooks.distance(model_multiple)
cooks_max <- which.max(dist)
cooks_max_value <- dist[cooks_max]
cooks_threshold <- 3*mean(dist)

influential <- dist[dist > cooks_threshold]
names_of_influential <- names(influential)
length(names_of_influential)

outliers <- aq[names_of_influential,]
outliers

data_wo_out <- aq %>% anti_join(outliers)

# Model without outliers
model_no_outliers <- lm(Ozone ~ ., data = data_wo_out)
summary(model_no_outliers)

# Plotting residuals vs fitted values
plot(model_no_outliers$fitted.values, model_no_outliers$residuals)
