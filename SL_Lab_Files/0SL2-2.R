library(MASS)
library(ggplot2)
library(car)
library(dplyr)
library(ISLR)

# for assignment, use airquality dataset, y = ozone, run regression, do everything taught, make best model

data(Boston)
summary(Boston)
head(Boston)
?Boston


# Univariate Normality using QQ Plot
qqnorm(Boston$lstat)
qqline(Boston$lstat)

# Correlation Matrix
cor(Boston)

# Linear Regression
plot(Boston$lstat, Boston$medv)
model <- lm(medv~lstat, data = Boston)
abline(model, col='purple')
summary(model)

# Multiple Linear Regression 
model <- lm(medv~., data = Boston)
summary(model)

# VIF
vif(model)

# ANOVA table
anova(model)

# other model
model <- lm(medv~ . -rad-tax, data = Boston)
summary(model)

# Confidence Interval
confint(model, level = 0.95)

# Prediction Interval
model <- lm(medv~lstat, data = Boston)
new_pt <- data.frame(lstat=c(4,5,6))
predict(model, newdata = new_pt)
predict(model, newdata = new_pt, interval='predict', level=0.95)

# Residual, fitted and deviance
residuals(model)
fitted(model)
deviance(model)

# NPP for Residuals
lm.std <- rstandard(model)
qqnorm(lm.std, ylab='Standardized Residuals' ,xlab='Normal Scores', main='Normal Prob Plot for Residuals')
qqline(lm.std)

# Backward Stepwise Regression
model <- lm(medv~., data=Boston)
summary(model)

model1 <- stepAIC(model, direction='backward',trace=FALSE)
summary(model1)

# Cooks Distance
model <- lm(medv~., data=Boston)
dist <- cooks.distance(model)
cooks.distance(model)[which.max(cooks.distance(model))]

influential <- dist[(dist>3*mean(dist))]
names_of_influential <- names(influential)
names_of_influential
length(names_of_influential)

outliers <- Boston[names_of_influential,]
outliers

data_wo_out <- Boston %>% anti_join(outliers)
data_wo_out

# Model without outliers
model <- lm(medv~., data=data_wo_out)
summary(model)