# Load required libraries for Risk Assessment
library(tidyverse)
library(caret)
library(pROC)
library(car)
library(ggcorrplot)
library(readxl)

# Load dataset
data <- read_excel("ACCT_Monitoring_FinalData.xlsx")

# Data Preprocessing
# 1. Handle missing values
data <- data %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# 2. Calculate derived feature for Credit Utilization
data <- data %>% mutate(Credit_Utilization = TOT_SPEND / CREDIT_LIMIT)

# 3. Create a target variable for risk assessment
# High risk is defined as Credit Utilization > 0.8
data <- data %>% mutate(Risk_Level = ifelse(Credit_Utilization > 0.8, 1, 0))

# 4. Select relevant features
selected_features <- c("CLI_AMOUNT", "TOT_SPEND", "NSF_PMTS", "PAYDEX", 
                       "Credit_Utilization", "Risk_Level")
data <- data %>% select(all_of(selected_features))

# Ensure target variable is a factor
data$Risk_Level <- as.factor(data$Risk_Level)

# Train-Test Split
set.seed(123)
train_index <- createDataPartition(data$Risk_Level, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Correlation:
cor_matrix <- cor(train_data %>% select(where(is.numeric)))
print(cor_matrix)

# Visualize correlations
ggcorrplot(cor_matrix, method = "circle", lab = TRUE)

# Fit a logistic regression model
model_glm <- glm(Risk_Level ~ ., data = train_data, family = "binomial", control = glm.control(maxit = 1000))

# Calculate VIF for logistic regression model
vif_results <- vif(model_glm)
print(vif_results)

# Predict on the test data
pred_glm <- predict(model_glm, newdata = test_data, type = "response")

# Evaluate with ROC and AUC
roc_glm <- roc(test_data$Risk_Level, pred_glm)
print(paste("AUC for Logistic Regression (GLM): ", auc(roc_glm)))

# Model Summary
summary(model_glm)

ggplot(train_data, aes(x = CLI_AMOUNT, color = Risk_Level)) + geom_density()


# Predictions
pred_probs <- predict(model_glm, test_data, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Performance Metrics
conf_matrix <- confusionMatrix(factor(pred_classes), test_data$Risk_Level)
print(conf_matrix)

# AUC-ROC
roc_curve <- roc(as.numeric(test_data$Risk_Level), pred_probs)
auc <- auc(roc_curve)
plot(roc_curve, col = "blue", main = "ROC Curve")
print(paste("AUC:", auc))

