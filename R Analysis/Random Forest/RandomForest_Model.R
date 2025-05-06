#Required libraries
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(writexl)

#Loading Data
data <- read_excel("ACCT_Monitoring_FinalData.xlsx")

cross_sell_f_data
data1 <- read_excel("cross_sell_f_data.xlsx")
str(data1)

#Data Preprocessing - Convertion of categorical variables to factors
data$STATE <- as.factor(data$STATE)
data$LOCK_CODE <- as.factor(data$LOCK_CODE)
data$LOCK_REASON <- as.factor(data$LOCK_REASON)
data$PORTFOLIO <- as.factor(data$PORTFOLIO)

#Defining the Target Variable
data$PerformanceLabel <- as.factor(ifelse(data$TOT_NET_REV > median(data$TOT_NET_REV), "High", "Low"))

#Splitting data into test and train.
set.seed(123)
trainIndex <- createDataPartition(data$PerformanceLabel, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

#Random Forest Model
RandomForest_model <- randomForest(PerformanceLabel ~ CLI_AMOUNT + TERM_DAYS + DUE_DAYS + PAYDEX + VANTAGE_SCORE +
                                     NO_OF_PAYMENT + PAYMENT_AMOUNT + FUEL_SPEND + NONFUEL_SPEND + TOT_SPEND +
                                     FUEL_NUM_TRX + NONFUEL_NUM_TRX + FUEL_TRX_AMT + NONFUEL_TRX_AMT + NSF_AMT +
                                     NSF_PMTS + LOCK_DAYS + TOT_NET_REV + TOT_TRX_AMT + TOT_NUM_TRX + SEGMENT_SCORE + WO_AMOUNT,
                                   data = trainData, importance = TRUE, ntree = 100)

#Model Evaluation
predictions <- predict(RandomForest_model, testData)
confusionMatrix(predictions, testData$PerformanceLabel)

#Customer performance Evaluation:
importance_df <- as.data.frame(importance(RandomForest_model))
importance_df$Variable <- rownames(importance_df)
top_importance <- importance_df %>% arrange(desc(MeanDecreaseAccuracy)) %>% head(10)

ggplot(top_importance, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_segment(aes(x = Variable, xend = Variable, y = 0, yend = MeanDecreaseAccuracy), color="black") +
  geom_point(color="cyan4", size=4) +
  coord_flip() +
  labs(title = "Impact of varibles on Customer Performance ", y = "Mean Decrease in Accuracy", x = "Feature")

#Transaction Evaluation:
p1 <- ggplot(testData, aes(x = PerformanceLabel, y = FUEL_SPEND, fill = PerformanceLabel)) +
  geom_boxplot() +
  labs(title = "Fuel Spend by Performance Label", x = "Performance Label", y = "Fuel Spend") +
  theme_minimal() +
  scale_fill_manual(values = c("High" = "chartreuse4", "Low" = "orange"))

p2 <- ggplot(testData, aes(x = PerformanceLabel, y = NONFUEL_SPEND, fill = PerformanceLabel)) +
  geom_boxplot() +
  labs(title = "Non Fuel Spend by Performance Label", x = "Performance Label", y = "Non Fuel Spend") +
  theme_minimal() +
  scale_fill_manual(values = c("High" = "chartreuse4", "Low" = "orange"))


p3<- ggplot(testData, aes(x = PerformanceLabel, y = TOT_NUM_TRX, fill = PerformanceLabel)) +
  geom_boxplot() +
  labs(title = "Total Transactions Count by Performance Label", x = "Performance Label", y = "Total Number of Transactions") +
  theme_minimal() +
  scale_fill_manual(values = c("High" = "chartreuse4", "Low" = "orange"))

p4<- ggplot(testData, aes(x = PerformanceLabel, y = TOT_TRX_AMT, fill = PerformanceLabel)) +
  geom_boxplot() +
  labs(title = "Total Transaction Amount by Performance Label", x = "Performance Label", y = "Total Amount of Transaction") +
  theme_minimal() +
  scale_fill_manual(values = c("High" = "chartreuse4", "Low" = "orange"))

grid.arrange(
  arrangeGrob(p1, p2, ncol = 2, top = "Performance Analysis by Fuel Spend and Non Fuel Spend"),
  arrangeGrob(p3, p4, ncol = 2, top = "Performance Analysis by Transactions"),
  heights = c(1, 1),  
  padding = unit(1, "line")  
)

#Risk Assessment
#Risk Indicators and Suggested Treatments
risk_related_features <- c("PAYDEX", "TOT_NET_REV", "NSF_PMTS", "NO_OF_PAYMENT","WO_AMOUNT")
risk_importance_df <- importance_df %>% filter(Variable %in% risk_related_features)
risk_importance_df <- risk_importance_df %>%
  mutate(Treatment = case_when(
    Variable == "PAYDEX" ~ "Credit Monitoring or Term Tightening",
    Variable == "TOT_NET_REV" ~ "Proactive Communication on Dues",
    Variable == "NSF_PMTS" ~ "Inactive Account Closure",
    Variable == "NO_OF_PAYMENT" ~ "Increased Payment Frequency Monitoring",
    Variable == "WO_AMOUNT" ~ "Prioritize for Collection and Recovery Actions"
  ))

ggplot(risk_importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "maroon") +
  coord_flip() +
  labs(title = "Risk Indicators and Suggested Treatments",
       x = "Risk Indicator",
       y = "Mean Decrease in Gini") +
  geom_text(aes(label = Treatment), hjust = -0.1, size = 3, color = "black") +
  theme_minimal()

#Risk Associated Accounts
data$PredictedRisk <- predict(RandomForest_model, data)
high_risk_customers <- data %>% filter(PredictedRisk == "High")
View(high_risk_customers)

#Download Excel of High Risk Customers
write.csv(high_risk_customers, "High_Risk_Customers.csv", row.names = FALSE)
write_xlsx(high_risk_customers, path = "High_Risk_Customers.xlsx")

# View or print high-risk customers for verification
print(high_risk_customers)






