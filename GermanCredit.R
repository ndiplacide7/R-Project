# Load required libraries
library(kernlab)
library(datasets)
library(caret)
library(e1071)  # For SVM and Naive Bayes
library(MASS)   # For additional statistical functions

# Load and prepare data
data("GermanCredit")
attach(GermanCredit)
# view(GermanCredit)

# Clean the data - Check for missing values
sum(is.na(GermanCredit))  # Check for NA values

# Bivariate analysis with target variable
# Create contingency tables and chi-square tests for categorical variables
for(i in 1:ncol(GermanCredit)) {
  if(is.factor(GermanCredit[,i])) {
    cat("\nContingency table for", names(GermanCredit)[i], "vs Class:\n")
    print(table(GermanCredit[,i], GermanCredit$Class))
    print(chisq.test(GermanCredit[,i], GermanCredit$Class))
  }
}

# Numerical summaries by class
numerical_vars <- sapply(GermanCredit, is.numeric)
for(i in which(numerical_vars)) {
  cat("\nSummary statistics for", names(GermanCredit)[i], "by Class:\n")
  print(tapply(GermanCredit[,i], GermanCredit$Class, summary))
}

# Split data into training and testing sets
set.seed(123)  # For reproducibility
indices <- sample(1:nrow(GermanCredit), 0.75*nrow(GermanCredit), replace=FALSE)
training_data <- GermanCredit[indices,]
testing_data <- GermanCredit[-indices,]

# Prepare predictors and response variables
X_train <- training_data[,-which(names(training_data)=="Class")]
y_train <- training_data$Class
X_test <- testing_data[,-which(names(testing_data)=="Class")]
y_test <- testing_data$Class

# Train SVM model with cross-validation for parameter tuning
ctrl <- trainControl(method="cv", number=5)
# svm_model <- train(Class ~ .,
#                    data=training_data,
#                    method="svmRadial",
#                    trControl=ctrl,
#                    tuneLength=5)

# svm_model <- train(Class ~ .,
#                    data=training_data,
#                    method="svmLinear",
#                    trControl=ctrl,
#                    tuneLength=5)

svm_model <- train(Class ~ .,
                   data=training_data,
                   method="svmPoly",
                   trControl=ctrl,
                   tuneLength=5)

# Make predictions
svm_train_pred <- predict(svm_model, X_train)
svm_test_pred <- predict(svm_model, X_test)

# Evaluate SVM performance
svm_train_cm <- confusionMatrix(svm_train_pred, y_train)
svm_test_cm <- confusionMatrix(svm_test_pred, y_test)

# Train Naive Bayes model
nb_model <- naiveBayes(Class ~ ., data=training_data)

# Make predictions with Naive Bayes
nb_train_pred <- predict(nb_model, X_train)
nb_test_pred <- predict(nb_model, X_test)

# Evaluate Naive Bayes performance
nb_train_cm <- confusionMatrix(nb_train_pred, y_train)
nb_test_cm <- confusionMatrix(nb_test_pred, y_test)

# Print results
cat("\nSVM Results:\n")
cat("Training Accuracy:", svm_train_cm$overall["Accuracy"], "\n")
cat("Testing Accuracy:", svm_test_cm$overall["Accuracy"], "\n")
print(svm_test_cm$table)

cat("\nNaive Bayes Results:\n")
cat("Training Accuracy:", nb_train_cm$overall["Accuracy"], "\n")
cat("Testing Accuracy:", nb_test_cm$overall["Accuracy"], "\n")
print(nb_test_cm$table)

# Variable Importance
var_imp <- varImp(svm_model)
plot(var_imp)