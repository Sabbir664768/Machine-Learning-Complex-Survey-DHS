---
title: "UGC Project"
author: "Md Sabbir Hossain"
date: "9/25/2023"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.



```{r }
library(readr)
imputed_data <- read_csv("~/imputed_data.csv")

```



```{r }
#****logistic regression model

# Load required libraries
library(caret)
library(ROCR)  # For ROC and AUC-PR calculations
library(Metrics)  # For log loss calculation


# Fit the logistic regression model
model1 <- glm(antibiotic_f ~ v025 + v106 + v137 + v190 + v705 + v717 + b4 + m5 + m17 + h10 + hw1 + bmi, data = imputed_data1, family = binomial(link = "logit"))

# Model summary
summary(model1)

# Generate predictions based on the model
predicted <- predict(model1, newdata = imputed_data1, type = "response")

# Convert predicted probabilities to binary outcomes
predicted_binary <- ifelse(predicted >= 0.5, 1, 0)

# Calculate the accuracy of the logistic regression model
accuracy <- sum(predicted_binary == imputed_data1$antibiotic_f) / length(imputed_data1$antibiotic_f)
cat("Accuracy:", accuracy, "\n")

# Define the outcome variable and independent variables
outcome <- imputed_data1$antibiotic_f
independent_vars <- imputed_data1[, c("v025", "v106", "v137", "v190", "v705", "v717", "b4", "m5", "m17", "h10", "hw1", "bmi")]

# Fit the logistic regression model
log_reg_model <- glm(outcome ~ ., data = data.frame(cbind(outcome, independent_vars)), family = binomial(link = "logit"))

# Predict on the training data
predictions_log_reg <- predict(log_reg_model, newdata = data.frame(cbind(outcome, independent_vars)), type = "response")

# Convert predicted probabilities to binary outcomes
predicted_classes_log_reg <- ifelse(predictions_log_reg >= 0.5, 1, 0)

# Calculate accuracy for logistic regression
accuracy_log_reg <- sum(predicted_classes_log_reg == outcome) / length(outcome)
cat("Logistic Regression Accuracy:", accuracy_log_reg, "\n")

# Create confusion matrix
conf_matrix_log_reg <- table(Actual = outcome, Predicted = predicted_classes_log_reg)
print("Confusion Matrix:")
print(conf_matrix_log_reg)

# Calculate precision, recall, and F1-score for logistic regression
precision_log_reg <- conf_matrix_log_reg[2, 2] / (conf_matrix_log_reg[2, 2] + conf_matrix_log_reg[1, 2])
recall_log_reg <- conf_matrix_log_reg[2, 2] / (conf_matrix_log_reg[2, 2] + conf_matrix_log_reg[2, 1])
f1_score_log_reg <- 2 * (precision_log_reg * recall_log_reg) / (precision_log_reg + recall_log_reg)

cat("Precision:", precision_log_reg, "\n")
cat("Recall:", recall_log_reg, "\n")
cat("F1-Score:", f1_score_log_reg, "\n")

# Calculate ROC-AUC
roc_obj <- prediction(predictions_log_reg, outcome)
roc_perf <- performance(roc_obj, "auc")
roc_auc <- as.numeric(roc_perf@y.values)
cat("ROC-AUC:", roc_auc, "\n")


```



```{r }
#****KNN model


# Load required libraries
library(caret)
library(ROCR)  # For ROC and AUC-PR calculations
library(Metrics)  # For log loss calculation

# Load data (if not already loaded)
# Replace 'your_data_file.csv' with your actual data file path
# imputed_data1 <- read.csv("your_data_file.csv")

# Define the outcome variable and independent variables
outcome <- imputed_data1$antibiotic_f
independent_vars <- imputed_data1[, c("v025", "v106", "v137", "v190", "v705", "v717", "b4", "m5", "m17", "h10", "hw1", "bmi")]

# Split the data into training and testing sets (adjust the ratio as needed)
set.seed(123)  # Set a seed for reproducibility
train_index <- createDataPartition(outcome, p = 0.7, list = FALSE)
train_data <- independent_vars[train_index, ]
test_data <- independent_vars[-train_index, ]
train_outcome <- outcome[train_index]
test_outcome <- outcome[-train_index]

# Train a KNN model with k=20
library(class)
knn_model <- knn(train_data, test_data, train_outcome, k = 20)

# Calculate accuracy for KNN model
accuracy_knn <- sum(knn_model == test_outcome) / length(test_outcome)
cat("KNN Model Accuracy:", accuracy_knn, "\n")

# Create confusion matrix for KNN model
conf_matrix_knn <- table(Actual = test_outcome, Predicted = knn_model)
print("Confusion Matrix for KNN Model:")
print(conf_matrix_knn)

# Calculate precision, recall, and F1-score for KNN model
precision_knn <- conf_matrix_knn[2, 2] / (conf_matrix_knn[2, 2] + conf_matrix_knn[1, 2])
recall_knn <- conf_matrix_knn[2, 2] / (conf_matrix_knn[2, 2] + conf_matrix_knn[2, 1])
f1_score_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)

cat("KNN Model Precision:", precision_knn, "\n")
cat("KNN Model Recall:", recall_knn, "\n")
cat("KNN Model F1-Score:", f1_score_knn, "\n")

# Load the required libraries (if not already loaded)
library(ROCR)

# Load the required libraries (if not already loaded)
library(ROCR)

# Create an empty vector to store ROC-AUC values
roc_auc_values <- numeric()

# Define a range of k values to test
k_values <- seq(1, 50, by = 1)  # Adjust the range as needed

# Loop through different values of k
for (k in k_values) {
  # Train the KNN model
  knn_model <- knn(train_data, test_data, train_outcome, k = k)
  
  # Create a prediction object for ROC calculation
  knn_prediction <- prediction(as.numeric(knn_model == 1), test_outcome)
  
  # Calculate ROC curve and AUC for KNN model
  knn_performance <- performance(knn_prediction, "tpr", "fpr")
  knn_roc_auc <- as.numeric(performance(knn_prediction, "auc")@y.values)
  
  # Append the ROC-AUC value to the vector
  roc_auc_values <- c(roc_auc_values, knn_roc_auc)
}

# Find the value of k with the highest ROC-AUC
best_k <- k_values[which.max(roc_auc_values)]
best_roc_auc <- max(roc_auc_values)

# Print the best k and its corresponding ROC-AUC
cat("Best K for KNN:", best_k, "\n")
cat("ROC-AUC for Best K:", best_roc_auc, "\n")

```



```{r }


# SVM model

# Load required libraries
library(e1071)  # For SVM
library(caret)  # For evaluation metrics
library(ROCR)   # For ROC and AUC-PR calculations

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(outcome, p = 0.7, list = FALSE)
train_data <- independent_vars[train_index, ]
test_data <- independent_vars[-train_index, ]
train_outcome <- outcome[train_index]
test_outcome <- outcome[-train_index]

# Train an SVM model
svm_model <- svm(train_outcome ~ ., data = train_data, kernel = "radial", probability = TRUE)

# Predict on the testing data
svm_predictions <- predict(svm_model, test_data)

# Calculate accuracy for SVM model
accuracy_svm <- sum(svm_predictions == test_outcome) / length(test_outcome)
cat("SVM Model Accuracy:", accuracy_svm, "\n")

# Create confusion matrix for SVM model
conf_matrix_svm <- table(Actual = test_outcome, Predicted = svm_predictions)
print("Confusion Matrix for SVM Model:")
print(conf_matrix_svm)

# Calculate precision, recall, and F1-score for SVM model
precision_svm <- conf_matrix_svm[2, 2] / (conf_matrix_svm[2, 2] + conf_matrix_svm[1, 2])
recall_svm <- conf_matrix_svm[2, 2] / (conf_matrix_svm[2, 2] + conf_matrix_svm[2, 1])
f1_score_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)

cat("SVM Model Precision:", precision_svm, "\n")
cat("SVM Model Recall:", recall_svm, "\n")
cat("SVM Model F1-Score:", f1_score_svm, "\n")


# Load the required library
library(pROC)

# Predict decision values for SVM model
svm_decision_values <- as.numeric(predict(svm_model, test_data, probability = TRUE))

# Ensure test_outcome is a binary factor
test_outcome <- as.factor(test_outcome)

# Create a ROC curve object
roc_obj <- roc(response = test_outcome, predictor = svm_decision_values)

# Calculate ROC-AUC
svm_roc_auc <- auc(roc_obj)

cat("ROC-AUC for SVM Model:", svm_roc_auc, "\n")

```



```{r }


# XGBOOST model

#XGboost
install.packages("xgboost")

# Load the required libraries
library(xgboost)
library(pROC)   # For ROC and AUC-PR calculations

# Define the outcome variable and independent variables
outcome <- imputed_data1$antibiotic_f
independent_vars <- imputed_data1[, c("v025", "v106", "v137", "v190", "v705", "v717", "b4", "m5", "m17", "h10", "hw1", "bmi")]

# Prepare the XGBoost data
xgb_data <- xgb.DMatrix(data = as.matrix(independent_vars), label = outcome)

# Set the XGBoost parameters
params <- list(objective = "binary:logistic", eval_metric = "logloss", max.depth = 6, eta = 0.1, nrounds = 100)

# Train the XGBoost model
xgb_model <- xgboost(params = params, data = xgb_data, nrounds = params$nrounds)

# Make predictions on the training data using the XGBoost model
predictions_xgboost <- predict(xgb_model, newdata = as.matrix(independent_vars))

# Convert predicted probabilities to binary outcomes
predicted_classes_xgboost <- ifelse(predictions_xgboost >= 0.5, 1, 0)

# Calculate accuracy for XGBoost
accuracy_xgboost <- sum(predicted_classes_xgboost == outcome) / length(outcome)
print(paste("XGBoost Accuracy:", accuracy_xgboost))

# Calculate confusion matrix for XGBoost
conf_matrix_xgboost <- table(Actual = outcome, Predicted = predicted_classes_xgboost)

# Calculate precision, recall, and F1-score for XGBoost
precision_xgboost <- conf_matrix_xgboost[2, 2] / sum(conf_matrix_xgboost[, 2])
recall_xgboost <- conf_matrix_xgboost[2, 2] / sum(conf_matrix_xgboost[2, ])
f1_score_xgboost <- 2 * (precision_xgboost * recall_xgboost) / (precision_xgboost + recall_xgboost)

# Print evaluation metrics for XGBoost
cat("XGBoost Precision:", precision_xgboost, "\n")
cat("XGBoost Recall:", recall_xgboost, "\n")
cat("XGBoost F1-Score:", f1_score_xgboost, "\n")

# Print confusion matrix for XGBoost
print(conf_matrix_xgboost)

# Predict class probabilities for ROC and AUC-PR calculations
xgb_probabilities <- predict(xgb_model, newdata = as.matrix(independent_vars), type = "response")

# Create a ROC curve object
roc_obj_xgboost <- roc(outcome, xgb_probabilities)

# Calculate ROC-AUC for XGBoost
xgb_roc_auc <- auc(roc_obj_xgboost)

cat("ROC-AUC for XGBoost Model:", xgb_roc_auc, "\n")


```



```{r }

# ANN model with Sigmoide


#ANN
# Load the required libraries
install.packages("neuralnet")
library(neuralnet)
install.packages("ROCR")
library(ROCR)

# Define the outcome variable and independent variables
outcome <- imputed_data1$antibiotic_f
independent_vars <- imputed_data1[, c("v025", "v106", "v137", "v190", "v705", "v717", "b4", "m5", "m17", "h10", "hw1", "bmi")]

# Standardize the independent variables for ANN
scaled_independent_vars <- scale(independent_vars)

# Combine the independent and outcome variables for ANN
ann_data <- cbind(outcome, scaled_independent_vars)

# Fit the ANN model with sigmoid activation function
ann_model_sigmoid <- neuralnet(outcome ~ ., data = ann_data, hidden = 2, act.fct = "logistic")

# Print the model summary
print(ann_model_sigmoid)

# Predict on the training data using the ANN model with sigmoid activation function
predictions_ann_sigmoid <- compute(ann_model_sigmoid, scaled_independent_vars)$net.result
predictions_ann_sigmoid <- ifelse(predictions_ann_sigmoid >= 0.5, 1, 0)

# Calculate accuracy for ANN with sigmoid activation function
accuracy_ann_sigmoid <- sum(predictions_ann_sigmoid == outcome) / length(outcome)
print(paste("ANN Accuracy (Sigmoid Activation):", accuracy_ann_sigmoid))


# Calculate confusion matrix for ANN with sigmoid activation function
conf_matrix_ann_sigmoid <- table(Actual = outcome, Predicted = predictions_ann_sigmoid)
print("Confusion Matrix for ANN (Sigmoid Activation):")
print(conf_matrix_ann_sigmoid)

# Calculate precision, recall, and F1-score for ANN with sigmoid activation function
precision_ann_sigmoid <- conf_matrix_ann_sigmoid[2, 2] / sum(conf_matrix_ann_sigmoid[, 2])
recall_ann_sigmoid <- conf_matrix_ann_sigmoid[2, 2] / sum(conf_matrix_ann_sigmoid[2, ])
f1_score_ann_sigmoid <- 2 * (precision_ann_sigmoid * recall_ann_sigmoid) / (precision_ann_sigmoid + recall_ann_sigmoid)

cat("ANN Precision (Sigmoid Activation):", precision_ann_sigmoid, "\n")
cat("ANN Recall (Sigmoid Activation):", recall_ann_sigmoid, "\n")
cat("ANN F1-Score (Sigmoid Activation):", f1_score_ann_sigmoid, "\n")

install.packages("pROC")

# Load the required library
library(pROC)

# Create a ROC curve object
roc_ann <- roc(outcome, as.numeric(predictions_ann_sigmoid))

# Calculate ROC-AUC for ANN with sigmoid activation function
ann_roc_auc <- auc(roc_ann)

# Plot ROC curve
plot(roc_ann, main = "ROC Curve for ANN (Sigmoid Activation)")
abline(a = 0, b = 1, col = "red", lty = 2)

cat("ROC-AUC for ANN (Sigmoid Activation):", ann_roc_auc, "\n")



```


```{r }
#ANN with ReLU Activation




# Load the required libraries
install.packages("neuralnet")
library(neuralnet)
install.packages("ROCR")
library(ROCR)

# Define the outcome variable and independent variables
outcome <- imputed_data1$antibiotic_f
independent_vars <- imputed_data1[, c("v025", "v106", "v137", "v190", "v705", "v717", "b4", "m5", "m17", "h10", "hw1", "bmi")]

# Standardize the independent variables for ANN
scaled_independent_vars <- scale(independent_vars)

# Combine the independent and outcome variables for ANN
ann_data <- cbind(outcome, scaled_independent_vars)

# Fit the ANN model with ReLU activation function
ann_model_relu <- neuralnet(outcome ~ ., data = ann_data, hidden = 2, act.fct = "logistic", linear.output = TRUE)

# Print the model summary
print(ann_model_relu)

# Predict on the training data using the ANN model with ReLU activation function
predictions_ann_relu <- compute(ann_model_relu, scaled_independent_vars)$net.result
predictions_ann_relu <- ifelse(predictions_ann_relu >= 0.5, 1, 0)

# Calculate accuracy for ANN with ReLU activation function
accuracy_ann_relu <- sum(predictions_ann_relu == outcome) / length(outcome)
print(paste("ANN Accuracy (ReLU Activation):", accuracy_ann_relu))


# Calculate confusion matrix for ANN with ReLU activation function
conf_matrix_ann_relu <- table(Actual = outcome, Predicted = predictions_ann_relu)
print("Confusion Matrix for ANN (ReLU Activation):")
print(conf_matrix_ann_relu)

# Calculate precision, recall, and F1-score for ANN with ReLU activation function
precision_ann_relu <- conf_matrix_ann_relu[2, 2] / sum(conf_matrix_ann_relu[, 2])
recall_ann_relu <- conf_matrix_ann_relu[2, 2] / sum(conf_matrix_ann_relu[2, ])
f1_score_ann_relu <- 2 * (precision_ann_relu * recall_ann_relu) / (precision_ann_relu + recall_ann_relu)

cat("ANN Precision (ReLU Activation):", precision_ann_relu, "\n")
cat("ANN Recall (ReLU Activation):", recall_ann_relu, "\n")
cat("ANN F1-Score (ReLU Activation):", f1_score_ann_relu, "\n")

# Create a ROC curve object
ann_roc_obj_relu <- roc(outcome, as.numeric(predictions_ann_relu))

# Calculate ROC-AUC for ANN with ReLU activation function
ann_roc_auc_relu <- auc(ann_roc_obj_relu)

cat("ROC-AUC for ANN (ReLU Activation):", ann_roc_auc_relu, "\n")


```


```{r }
#Adaptive boosting


# Load required libraries
library(caret)
library(ada)     # For ADAboost
library(ROCR)    # For ROC and AUC-PR calculations

# Split the data into training and testing sets (replace with your data)
set.seed(123)
train_index <- createDataPartition(outcome, p = 0.7, list = FALSE)
train_data <- independent_vars[train_index, ]
test_data <- independent_vars[-train_index, ]
train_outcome <- outcome[train_index]
test_outcome <- outcome[-train_index]

# Train an ADABoost classifier
ada_model <- ada(x = train_data, y = train_outcome, iter = 50, type = "discrete")

# Predict on the testing data
ada_predictions <- predict(ada_model, newdata = test_data)

# Calculate accuracy for ADABoost
accuracy_ada <- sum(ada_predictions == test_outcome) / length(test_outcome)
cat("ADABoost Accuracy:", accuracy_ada, "\n")

# Create confusion matrix for ADABoost
conf_matrix_ada <- table(Actual = test_outcome, Predicted = ada_predictions)
print("Confusion Matrix for ADABoost:")
print(conf_matrix_ada)

# Calculate precision, recall, and F1-score for ADABoost
precision_ada <- conf_matrix_ada[2, 2] / (conf_matrix_ada[2, 2] + conf_matrix_ada[1, 2])
recall_ada <- conf_matrix_ada[2, 2] / (conf_matrix_ada[2, 2] + conf_matrix_ada[2, 1])
f1_score_ada <- 2 * (precision_ada * recall_ada) / (precision_ada + recall_ada)

cat("ADABoost Precision:", precision_ada, "\n")
cat("ADABoost Recall:", recall_ada, "\n")
cat("ADABoost F1-Score:", f1_score_ada, "\n")


# Load required library
library(pROC)

# Predict probabilities for ADABoost
ada_probabilities <- as.numeric(predict(ada_model, newdata = test_data, type = "response"))

# Ensure test_outcome is numeric (0 or 1)
test_outcome <- as.numeric(test_outcome)

# Calculate ROC-AUC for ADABoost
ada_roc_obj <- roc(response = test_outcome, predictor = ada_probabilities)
ada_roc_auc <- auc(ada_roc_obj)

cat("ROC-AUC for ADABoost:", ada_roc_auc, "\n")

```






