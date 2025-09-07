install.packages("tidyverse")
library(tidyverse)
library(ISLR)
library(MASS)
head(Default)
# View(Default)
str(Default)
# Check the assumption
Default[3:4] <- scale(Default[3:4])
# To return a vector of the means and std dev of the variables
apply(Default[3:4], 2, mean)
apply(Default[3:4], 2, sd)

set.seed(1)

# train and test sample
sample <- sample(c(TRUE, FALSE), nrow(Default),
                 replace = TRUE, prob = c(0.7, 0.3))
train <- Default[sample,]
test <- Default[!sample,]

head(train)
model <- lda(default~balance + income, data = train)
summary(model)
model

# use model for prediction
predicted <- predict(model, test)
p1 <- predicted$class
p1[1:5]
tab <- table(Predicted = p1, Actual = test$default)
tab
sum(diag(tab))/sum(tab)

# accuracy of the model
# confusion matrix and accuracy of testing data
p2 <- predict(model, train)$class
tab1 <- table(Predicted = p2, Actual = train$default)
tab1
sum(diag(tab1))/sum(tab1)




# Quadratic Discriminant Analysis
qmodel <- qda(default~balance + income, data = train)
qmodel
qpredicted <- predict(qmodel, test)
qp1 <- predicted$class
qp1[1:5]
