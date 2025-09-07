library(ISLR)
install.packages('caTools')
library(caTools)
library(dplyr)
library(class)
library(MASS)

Default
head(Default)

Default$student01 <- ifelse(Default$student == 'Yes', 1, 0)


# convert all variables into numeric
# unlike many of our previous methods, knn() requires that
Default$student = as.numeric(Default$student)
head(Default)

# splitting data into train and test data
set.seed(10000)
split <- sample.split(Default, SplitRatio = 0.8)
train_cl <- subset(Default, split == 'TRUE')
test_cl <- subset(Default, split == 'FALSE')

train_cl$balance <- scale(train_cl$balance)

# Feature scaling
train_scale <- scale(train_cl[,2:4])
test_scale <- scale(test_cl[,2:4])

head(train_scale)
head(test_scale)

# if you want to remove default column from dataset then
train_scale1 = (train_scale[,-1])
test_scale1 = (test_scale[,-1])

head(train_scale)
head(test_scale)

# Fitting KNN model to training dataset
classifir_knn <- knn(train = train_scale,
                     test = test_scale,
                     cl = train_cl$default,
                     k = 5)

classifir_knn

# Confusion matrix
cm <- table(test_cl$default,classifir_knn)
cm

