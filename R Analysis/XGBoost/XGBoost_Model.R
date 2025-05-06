# Load necessary libraries for Transaction Evaluation
library(dplyr)
library(xgboost)
library(caret)
library(readxl)

# Load dataset
data_clean <- read_excel("ACCT_Monitoring_FinalData.xlsx")

# Convert Character Columns to Factors
data_clean <- data_clean %>%
  mutate(across(c(FUEL_ONLY_PARENT_ACCT, CITY, STATE, ZIP, LOCK_CODE, LOCK_REASON, LOCK_TYPE, PORTFOLIO, LOB_REPORTING), as.factor))

# Derive TransactionPlatform
data_clean <- data_clean %>%
  mutate(TransactionPlatform = case_when(
    NONFUEL_SPEND > FUEL_SPEND ~ "Non-Fuel Focused",
    FUEL_SPEND > NONFUEL_SPEND ~ "Fuel Focused",
    TRUE ~ "Unknown"
  ))

# Derive TransactionTimeFrame
data_clean <- data_clean %>%
  mutate(TransactionTimeFrame_30 = ifelse(as.numeric(difftime(OPT_IN_DATE + 30, FLEETCOR_OPEN_DATE, units = "days")) >= 0, "Within 30 Days", "Beyond 30 Days"))

# Create additional features
data_clean <- data_clean %>%
  mutate(SpendUtilization = TOT_SPEND / CREDIT_LIMIT,
         AverageSpendPerTransaction = TOT_SPEND / TOT_NUM_TRX,
         DefaultRisk = as.factor(ifelse(WO_AMOUNT > 0, 1, 0)))

# Handle Missing Values
data_clean$AverageSpendPerTransaction[is.na(data_clean$AverageSpendPerTransaction)] <- 0

# Train-Test Split
set.seed(123)
train_index <- createDataPartition(data_clean$DefaultRisk, p = 0.8, list = FALSE)
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# Convert date columns to numeric (e.g., as timestamps)
train_data <- train_data %>%
  mutate(OPT_IN_DATE = as.numeric(OPT_IN_DATE),
         FLEETCOR_OPEN_DATE = as.numeric(FLEETCOR_OPEN_DATE))

test_data <- test_data %>%
  mutate(OPT_IN_DATE = as.numeric(OPT_IN_DATE),
         FLEETCOR_OPEN_DATE = as.numeric(FLEETCOR_OPEN_DATE))

# Convert factors to numeric, including derived columns
train_data_numeric <- train_data %>%
  mutate(across(where(is.factor), ~ as.integer(as.factor(.)))) %>%
  mutate(across(where(is.character), ~ as.integer(as.factor(.))))  # This line is mainly for safety

test_data_numeric <- test_data %>%
  mutate(across(where(is.factor), ~ as.integer(as.factor(.)))) %>%
  mutate(across(where(is.character), ~ as.integer(as.factor(.))))  # This line is mainly for safety

# Check the structure of the numeric data
str(train_data_numeric)
# Create the DMatrix, including derived columns
train_matrix <- xgb.DMatrix(data = as.matrix(train_data_numeric), label = as.numeric(train_data$DefaultRisk) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data_numeric), label = as.numeric(test_data$DefaultRisk) - 1)





# Set Parameters and Train the Model
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 6,
  eta = 0.3
)

# Train the XGBoost Model
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 100,
  watchlist = list(train = train_matrix, test = test_matrix),
  verbose = 0
)

summary(xgb_model)

# Evaluate Model Performance
pred_probs <- predict(xgb_model, test_matrix)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)  # Convert probabilities to binary labels

# Create a confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(as.factor(pred_labels), as.factor(as.numeric(test_data$DefaultRisk) - 1))
print(conf_matrix)

# Feature Importance
importance <- xgb.importance(feature_names = colnames(train_data_numeric), model = xgb_model)
xgb.plot.importance(importance)

# Display the feature importance values
importance_values <- importance %>%
  arrange(desc(Gain)) # Arrange by Gain or any other metric (e.g., Cover, Frequency)

print(importance_values)


# Load necessary libraries for visualization
library(ggplot2)

# Analyze TransactionPlatform
transaction_platform_analysis <- data_clean %>%
  group_by(TransactionPlatform, DefaultRisk) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(transaction_platform_analysis, aes(x = TransactionPlatform, y = Percentage, fill = as.factor(DefaultRisk))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Transaction Platform Impact on Default Risk",
       x = "Transaction Platform",
       y = "Percentage",
       fill = "Default Risk") +
  theme_minimal()

# Analyze TransactionTimeFrame
transaction_timeframe_analysis <- data_clean %>%
  group_by(TransactionTimeFrame_30, DefaultRisk) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggplot(transaction_timeframe_analysis, aes(x = TransactionTimeFrame_30, y = Percentage, fill = as.factor(DefaultRisk))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Transaction Timeframe Impact on Default Risk",
       x = "Transaction Timeframe (30 Days)",
       y = "Percentage",
       fill = "Default Risk") +
  theme_minimal()

# Analyze SpendUtilization
ggplot(data_clean, aes(x = SpendUtilization, fill = as.factor(DefaultRisk))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Spend Utilization Distribution by Default Risk",
       x = "Spend Utilization",
       y = "Count",
       fill = "Default Risk") +
  theme_minimal()

# Analyze AverageSpendPerTransaction
ggplot(data_clean, aes(x = AverageSpendPerTransaction, fill = as.factor(DefaultRisk))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Average Spend Per Transaction Distribution by Default Risk",
       x = "Average Spend Per Transaction",
       y = "Count",
       fill = "Default Risk") +
  theme_minimal()
                      
