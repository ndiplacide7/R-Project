#Install required Packages if not exists

# install.packages("e1071")
# install.packages("caret")
# install.packages("rpart")

# Load necessary libraries

library(rpart)
library(e1071)  # For SVM
library(caret)  # For evaluation metrics and data partitioning

# Load the stagec dataset
data("stagec", package = "rpart")

# Data Exploration and Preprocessing
summary(stagec)  # Explore dataset structure and characteristics
str(stagec)
# Check for missing values
print(sum(is.na(stagec)))

# Clean the dataset (remove rows with missing values)
stagec <- na.omit(stagec)

# Ensure the target variable is a factor
stagec$pgstat <- factor(stagec$pgstat)

# Split the dataset into training and test sets
set.seed(123)
trainIndex <- createDataPartition(stagec$pgstat, p = 0.7, list = FALSE)
trainData <- stagec[trainIndex, ]
testData <- stagec[-trainIndex, ]

# Train Decision Tree Model
dt_model <- rpart(pgstat ~ ., data = trainData, method = "class")
dt_pred_train <- predict(dt_model, trainData, type = "class")
dt_pred_test <- predict(dt_model, testData, type = "class")

# Ensure predictions are factors with matching levels for confusionMatrix
dt_pred_train <- factor(dt_pred_train, levels = levels(trainData$pgstat))
dt_pred_test <- factor(dt_pred_test, levels = levels(trainData$pgstat))

# Train SVM Model
svm_model <- svm(pgstat ~ ., data = trainData, probability = TRUE)
svm_pred_train <- predict(svm_model, trainData)
svm_pred_test <- predict(svm_model, testData)

# Ensure predictions are factors with matching levels for confusionMatrix
svm_pred_train <- factor(svm_pred_train, levels = levels(trainData$pgstat))
svm_pred_test <- factor(svm_pred_test, levels = levels(trainData$pgstat))

# Evaluate Decision Tree Model
dt_train_conf <- confusionMatrix(dt_pred_train, trainData$pgstat)
dt_test_conf <- confusionMatrix(dt_pred_test, testData$pgstat)

# Evaluate SVM Model
svm_train_conf <- confusionMatrix(svm_pred_train, trainData$pgstat)
svm_test_conf <- confusionMatrix(svm_pred_test, testData$pgstat)

# Print the results to fill in the table
cat("Decision Tree In-Sample Metrics:\n")
print(dt_train_conf$overall["Accuracy"])
print(dt_train_conf$byClass[c("Sensitivity", "Specificity")])

cat("\nDecision Tree Out-of-Sample Metrics:\n")
print(dt_test_conf$overall["Accuracy"])
print(dt_test_conf$byClass[c("Sensitivity", "Specificity")])

cat("\nSVM In-Sample Metrics:\n")
print(svm_train_conf$overall["Accuracy"])
print(svm_train_conf$byClass[c("Sensitivity", "Specificity")])

cat("\nSVM Out-of-Sample Metrics:\n")
print(svm_test_conf$overall["Accuracy"])
print(svm_test_conf$byClass[c("Sensitivity", "Specificity")])

# Model Comparison and Selection
# Analyze the performance of both models and choose the most suitable one
if (dt_test_conf$overall["Accuracy"] > svm_test_conf$overall["Accuracy"]) {
  cat("\nDecision Tree model is recommended for this dataset.")
} else {
  cat("\nSVM model is recommended for this dataset.")
}