library(readxl)
data <- read_excel("C:\\Users\\diksh\\OneDrive\\Desktop\\IIT KGP\\Sem Sixth\\SL_Lab_Test\\collected_data.xlsx")
View(data)

#Data Preprocessing

#1a-1
#annual_income <- data['Annual family income']
annual_income <- data$'Annual family income'
print(annual_income)

adjusted_income <- ifelse(annual_income > 1000, annual_income / 100000, annual_income)
print(adjusted_income)

adjusted_data <- data
adjusted_data$'Annual family income' <- adjusted_income
View(adjusted_data)

#adjusted_income_sums <- sum(adjusted_income)
#print(adjusted_income_sums)

#1a-2
education_to_years <- function(education_level) {
  if (education_level == "12th") {
    return(12)
  } else if (education_level == "Diploma or Equivalent") {
    return(14)
  } else if (education_level == "Graduate" || education_level == "Graduate Profession Degree eg. B.Tech,MBBS,LLB,BBA,etc." || education_level == "Graduate (Arts/Science)" || education_level == "B.Com") {
    return(15)
  } else if (education_level == "Post Graduate" || education_level == "Post Graduate  Professional Degree eg. MD,Mtech,LLM,MBAetc." || education_level == "Post Graduate  (Arts/Science)" || education_level == "Post graduate MSC") {
    return(17)
  } else if (education_level == "Doctoral Degree") {
    return(25)
  } else if (education_level == "Less than High School" || education_level == "10th") {
    return(10)
  } else {
    return(NA) 
  }
}

new_data <- adjusted_data
unique(adjusted_data$`Father's education level`)
unique(adjusted_data$`Father's education level`)
unique(adjusted_data$`Highest level of education`)
new_data$`Father's education level` <- sapply(adjusted_data$`Father's education level`, education_to_years)
new_data$`Mother's education level` <- sapply(adjusted_data$`Mother's education level`, education_to_years)
new_data$`Highest level of education` <- sapply(adjusted_data$`Highest level of education`, education_to_years)
View(new_data)

#replacing the na value("prefer not to say" case), with the mean of highest education level
new_data$`Highest level of education` <- sapply(new_data$`Highest level of education`, function(x) ifelse(is.na(x), mean(new_data$`Highest level of education`, na.rm = TRUE), x))
View(new_data)

#1b
#b.	Do a preliminary data processing on the dataset and remove outliers? 

null_values <- sapply(new_data, function(x) sum(is.na(x)))
print(null_values)

scale_numeric_columns <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  data[, numeric_cols] <- scale(data[, numeric_cols])
  return(data)
}
new_data_scaled <- scale_numeric_columns(new_data)
View(new_data_scaled)

dim(new_data)
remove_outliers <- function(data, threshold = 2.5) {
  numeric_cols <- sapply(data, is.numeric)
  data_clean <- data
  for (col in names(data)[numeric_cols]) {
    q1 <- quantile(data[[col]], 0.25)
    q3 <- quantile(data[[col]], 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - threshold * iqr
    upper_bound <- q3 + threshold * iqr
    data_clean <- data_clean[data_clean[[col]] >= lower_bound & data_clean[[col]] <= upper_bound, ]
  }
  return(data_clean)
}

new_data_clean <- remove_outliers(new_data)
dim(new_data_clean)
View(new_data_clean)

#c.	Perform exploratory data analysis to depict how willingness to pay is associated with the other variables?
  
library(ggplot2)
plot(new_data)

numeric_cols <- sapply(new_data_clean, is.numeric)
numeric_data <- new_data_clean[, numeric_cols]
View(numeric_data)
pairs(numeric_data)

numeric_cols <- sapply(new_data_clean, is.numeric)
#numeric_cols
numeric_data <- new_data_clean[, numeric_cols]
#View(numeric_data)
correlation_matrix <- cor(numeric_data)
heatmap(correlation_matrix)

cor_with_willingness_to_pay <- correlation_matrix["Willingness to pay (in INR)", ]
print(cor_with_willingness_to_pay)

#2
model <- lm(`Willingness to pay (in INR)` ~ Age + `Total siblings (including yourself)` + `Position among siblings` + `No. of sisters` + `No. of brothers` + `Highest level of education` + `Father's education level` + `Mother's education level` + `Annual family income`, data = new_data_clean)
summary(model)

#3
# Assuming new_data_clean is your cleaned dataset

# Step 1: Divide response variable into two categories
train_data$`Willingness to pay (in INR)` <- ifelse(train_data$`Willingness to pay (in INR)` >= 1000, "High", "Low")

# Step 2: Split dataset into training and testing sets
set.seed(9)  
train_index <- sample(1:nrow(new_data_clean), 0.8 * nrow(new_data_clean))
train_data <- new_data_clean[train_index, ]
test_data <- new_data_clean[-train_index, ]

# Step 3: Implement SVM, Decision Tree, and Logistic Regression models
library(e1071)  # for SVM
library(rpart)  # for Decision Tree
library(glmnet)  # for Logistic Regression

# Step 4: Train each model
# SVM
train_data$`Willingness to pay (in INR)` <- as.factor(train_data$`Willingness to pay (in INR)`)
View(train_data)
numeric_cols <- sapply(train_data, is.numeric)
train_data <- train_data[, numeric_cols]
svm_model <- svm(`Willingness to pay (in INR)` ~ ., data = train_data, kernel = "linear")

# Decision Tree
dt_model <- rpart(WTP_category ~ ., data = train_data, method = "class")

# Logistic Regression
glm_model <- glm(WTP_category ~ ., data = train_data, family = "binomial")

# Step 5: Evaluate model performance on test data
# SVM
svm_pred <- predict(svm_model, test_data)
svm_conf_matrix <- table(test_data$`Willingness to pay (in INR)`, svm_pred)
svm_accuracy <- sum(diag(svm_conf_matrix)) / sum(svm_conf_matrix)
svm_f1_score <- F1_Score(svm_pred, test_data$WTP_category)
svm_accuracy
svm_f1_score
# Decision Tree
dt_pred <- predict(dt_model, test_data, type = "class")
dt_conf_matrix <- table(test_data$WTP_category, dt_pred)
dt_accuracy <- sum(diag(dt_conf_matrix)) / sum(dt_conf_matrix)
dt_f1_score <- F1_Score(dt_pred, test_data$WTP_category)

# Logistic Regression
glm_prob <- predict(glm_model, test_data, type = "response")
glm_pred <- ifelse(glm_prob > 0.5, "High", "Low")
glm_conf_matrix <- table(test_data$WTP_category, glm_pred)
glm_accuracy <- sum(diag(glm_conf_matrix)) / sum(glm_conf_matrix)
glm_f1_score <- F1_Score(glm_pred, test_data$WTP_category)

# Step 6: Compare model performance
cat("SVM Accuracy:", svm_accuracy, "\n")
cat("SVM F1 Score:", svm_f1_score, "\n")

cat("\nDecision Tree Accuracy:", dt_accuracy, "\n")
cat("Decision Tree F1 Score:", dt_f1_score, "\n")

cat("\nLogistic Regression Accuracy:", glm_accuracy, "\n")
cat("Logistic Regression F1 Score:", glm_f1_score, "\n")


new_data_clean$`Willingness to pay (in INR)` <- ifelse(new_data_clean$`Willingness to pay (in INR)` >= 1000, "High", "Low")
set.seed(9)  
train_indices <- sample(nrow(new_data_clean), 0.8 * nrow(new_data_clean))  # 80% for training
train_data <- numeric_data[train_indices, ]
test_data <- numeric_data[-train_indices, ]
dim(train_data)
View(train_data)
svm_model <- svm(`Willingness to pay (in INR)` ~ ., data = train_data)
svm_pred <- predict(svm_model, test_data)
svm_confusion <- table(Actual = test_data$`Willingness to pay (in INR)`, Predicted = svm_pred)

accuracy_svm <- sum(diag(svm_confusion)) / sum(svm_confusion)
precision_svm <- svm_confusion[2, 2] / sum(svm_confusion[, 2])
recall_svm <- svm_confusion[2, 2] / sum(svm_confusion[2, ])
f1_score_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)
precision_svm
accuracy_svm
recall_svm 
f1_score_svm

# Fit Decision Tree model
library(rpart)
dt_model <- rpart(`Willingness to pay (in INR)` ~ ., data = train_data, method = "class")

# Predict using Decision Tree model
dt_pred <- predict(dt_model, test_data, type = "class")

# Evaluate Decision Tree model
dt_confusion <- table(Actual = test_data$`Willingness to pay (in INR)`, Predicted = dt_pred)
accuracy_dt <- sum(diag(dt_confusion)) / sum(dt_confusion)
precision_dt <- dt_confusion[2, 2] / sum(dt_confusion[, 2])
recall_dt <- dt_confusion[2, 2] / sum(dt_confusion[2, ])
f1_score_dt <- 2 * (precision_dt * recall_dt) / (precision_dt + recall_dt)

# Fit Logistic Regression model
glm_model <- glm(`Willingness to pay (in INR)` ~ ., data = train_data, family = "binomial")

# Predict using Logistic Regression model
glm_prob <- predict(glm_model, test_data, type = "response")
glm_pred <- ifelse(glm_prob > 0.5, "High", "Low")

# Evaluate Logistic Regression model
glm_confusion <- table(Actual = test_data$`Willingness to pay (in INR)`, Predicted = glm_pred)
accuracy_glm <- sum(diag(glm_confusion)) / sum(glm_confusion)
precision_glm <- glm_confusion[2, 2] / sum(glm_confusion[, 2])
recall_glm <- glm_confusion[2, 2] / sum(glm_confusion[2, ])
f1_score_glm <- 2 * (precision_glm * recall_glm) / (precision_glm + recall_glm)

# Print evaluation metrics
cat("SVM Model:\n")
cat("Accuracy:", accuracy_svm, "\n")
cat("Precision:", precision_svm, "\n")
cat("Recall:", recall_svm, "\n")
cat("F1 Score:", f1_score_svm, "\n\n")

cat("Decision Tree Model:\n")
cat("Accuracy:", accuracy_dt, "\n")
cat("Precision:", precision_dt, "\n")
cat("Recall:", recall_dt, "\n")
cat("F1 Score:", f1_score_dt, "\n\n")

cat("Logistic Regression Model:\n")
cat("Accuracy:", accuracy_glm, "\n")
cat("Precision:", precision_glm, "\n")
cat("Recall:", recall_glm, "\n")
cat("F1 Score:", f1_score_glm, "\n")

