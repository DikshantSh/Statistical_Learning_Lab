install.packages("MLmetrics")
install.packages("e1071")
install.packages("dplyr")
library(dplyr)
library(e1071)
library(MLmetrics)
library(caTools)
library(caret)
library(gplots)
data <- read.csv("C:\\Users\\SHISHUPAL\\Desktop\\ISE\\SEM 6\\Statistical Learning\\LAB\\Dataset\\drug200.csv")
head(data)
str(data)
summary(data)
data=na.omit(data)
unique(data$Sex)
unique(data$BP)
unique(data$Cholesterol)
unique(data$Drug)
attach(data)
# Frequency table for Sex
sex_freq <- table(data$Sex)
print(sex_freq)

# Frequency table for BP
bp_freq <- table(data$BP)
print(bp_freq)

# Frequency table for Cholesterol
cholesterol_freq <- table(data$Cholesterol)
print(cholesterol_freq)

# Frequency table for Drug
drug_freq <- table(data$Drug)
print(drug_freq)
#Histogram for sex
barplot(sex_freq, main = "Frequency of Sex", xlab = "Sex", ylab = "Frequency", col = "skyblue")
barplot(bp_freq, main = "Frequency of BP", xlab = "BP", ylab = "Frequency", col = "lightgreen")
barplot(cholesterol_freq, main = "Frequency of Cholesterol", xlab = "Cholesterol", ylab = "Frequency", col = "salmon")
barplot(drug_freq, main = "Frequency of Drug", xlab = "Drug", ylab = "Frequency", col = "lightblue")

# Set up a 2x2 layout for plots
par(mfrow = c(2, 2))

# Plot histograms
barplot(sex_freq, main = "Frequency of Sex", xlab = "Sex", ylab = "Frequency", col = "skyblue")
barplot(bp_freq, main = "Frequency of BP", xlab = "BP", ylab = "Frequency", col = "lightgreen")
barplot(cholesterol_freq, main = "Frequency of Cholesterol", xlab = "Cholesterol", ylab = "Frequency", col = "salmon")
barplot(drug_freq, main = "Frequency of Drug", xlab = "Drug", ylab = "Frequency", col = "lightblue")

# Reset the layout to default (1x1)
par(mfrow = c(1, 1))
#Scatter Plot of Age vs. Na_to_K
plot(data$Age, data$Na_to_K, main = "Scatter Plot of Age vs. Na_to_K", xlab = "Age", ylab = "Na_to_K")

# Define colors based on Age
age_colors <- ifelse(data$Age < 30, "red", "blue")

# Plot the scatter plot with colors
plot(data$Age, data$Na_to_K, main = "Scatter Plot of Age vs. Na_to_K", 
     xlab = "Age", ylab = "Na_to_K", col = age_colors, pch = 16)

# Add a legend for colors
legend("topright", legend = c("Age < 30", "Age >= 30"), col = c("red", "blue"), pch = 16, title = "Age Group")
#boxplot of Na_k by Drug
par(mfrow = c(2, 2))
boxplot(Na_to_K ~ Drug, data = data, main = "Boxplot of Na_to_K by Drug", xlab = "Drug", ylab = "Na_to_K")
barplot(table(data$Drug), main = "Counts of Drugs", xlab = "Drug", ylab = "Count")
pie(table(data$Cholesterol), main = "Distribution of Cholesterol")
hist(data$Age, breaks = 10, main = "Histogram of Age", xlab = "Age", ylab = "Frequency")
par(mfrow = c(1, 1))
# Convert character variables to factors
data$Sex <- as.factor(data$Sex)
data$BP <- as.factor(data$BP)
data$Cholesterol <- as.factor(data$Cholesterol)
data$Drug <- as.factor(data$Drug)

# Print the modified data
head(data)


# Split the dataset into training and testing sets
set.seed(123)  # for reproducibility
split <- sample.split(data$Drug, SplitRatio = 0.75)
trainset <- subset(data, split == TRUE)
testset <- subset(data, split == FALSE)

# Train the SVM model
svm_model <- svm(Drug ~ ., data = trainset, kernel = "linear", cost = 10, scale = FALSE)

# Make predictions on the test set
predictions <- predict(svm_model, newdata = testset)

# Print confusion matrix
conf_matrix <- table(predictions, testset$Drug)
conf_matrix

# Calculate accuracy
Accuracy(predictions,testset$Drug)

# Perform parameter tuning using SVM with a linear kernel

cost_range <- c(0.001, 0.01, 0.1, 1, 5, 10, 100)

tune.out <- tune(svm, Drug~.,data=trainset, kernel = "linear", ranges = list(cost = cost_range))
# Summary of the tuning results
summary(tune.out)
bestmod<-tune.out$best.model
summary(bestmod)
bestmod$index #support vectors
ypred<-predict(bestmod,testset)
table(predict=ypred,truth=testset$Drug)

confusionMatrix(ypred,testset$Drug)
#metrics
Accuracy(ypred,testset$Drug)
Precision(ypred,testset$Drug)
Recall(ypred,testset$Drug)
F1_Score(ypred,testset$Drug)

