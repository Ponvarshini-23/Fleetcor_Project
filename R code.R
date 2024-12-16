# Load required libraries
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)

# Load the dataset
data <- read.csv("Cleaned_Sample_Cross_Sell_Eligibility.csv")

# Feature selection: Choose relevant columns
data <- data %>%
  select(CREDIT_LIMIT.y.y, FUEL_SPEND, NONFUEL_SPEND, TOT_SPEND, VANTAGE_SCORE, WO_AMOUNT)

# Handle missing values: Replace NA with median values
data$CREDIT_LIMIT.y.y[is.na(data$CREDIT_LIMIT.y.y)] <- median(data$CREDIT_LIMIT.y.y, na.rm = TRUE)
data$VANTAGE_SCORE[is.na(data$VANTAGE_SCORE)] <- median(data$VANTAGE_SCORE, na.rm = TRUE)

# Create the target variable (1 = Eligible, 0 = Not Eligible)
data$Eligibility <- ifelse(data$CREDIT_LIMIT.y.y > 1000 & data$VANTAGE_SCORE >= 600, 1, 0)
data$Eligibility <- as.factor(data$Eligibility)  # Convert to factor for classification

# Split data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data$Eligibility, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train Random Forest Model for Classification
rf_model <- randomForest(Eligibility ~ CREDIT_LIMIT.y.y + FUEL_SPEND + NONFUEL_SPEND + TOT_SPEND + VANTAGE_SCORE,
                         data = trainData, ntree = 100, importance = TRUE)

# Train Logistic Regression Model
logistic_model <- glm(Eligibility ~ CREDIT_LIMIT.y.y + FUEL_SPEND + NONFUEL_SPEND + TOT_SPEND + VANTAGE_SCORE,
                      data = trainData, family = "binomial")

# Predictions and Probabilities
# Random Forest
rf_predictions <- predict(rf_model, newdata = testData)  # Class predictions
rf_probabilities <- predict(rf_model, newdata = testData, type = "prob")[, 2]  # Probabilities for ROC curve

# Logistic Regression
logistic_predictions <- predict(logistic_model, newdata = testData, type = "response")
logistic_predicted_class <- ifelse(logistic_predictions > 0.5, 1, 0)

# Evaluate Model Performance
# Random Forest Metrics
rf_confusion <- confusionMatrix(rf_predictions, testData$Eligibility)
rf_roc <- roc(as.numeric(testData$Eligibility) - 1, rf_probabilities)
rf_auc <- auc(rf_roc)

# Logistic Regression Metrics
logistic_confusion <- confusionMatrix(as.factor(logistic_predicted_class), testData$Eligibility)
logistic_roc <- roc(as.numeric(testData$Eligibility) - 1, logistic_predictions)
logistic_auc <- auc(logistic_roc)

# Display Results
cat("Random Forest Performance:\n")
print(rf_confusion)
cat("\nAUC for Random Forest:", rf_auc, "\n\n")

cat("Logistic Regression Performance:\n")
print(logistic_confusion)
cat("\nAUC for Logistic Regression:", logistic_auc, "\n\n")

# Convert ROC objects to data frames
rf_roc_data <- data.frame(
  Sensitivity = rf_roc$sensitivities,
  Specificity = rf_roc$specificities
)

logistic_roc_data <- data.frame(
  Sensitivity = logistic_roc$sensitivities,
  Specificity = logistic_roc$specificities
)

# Plot ROC Curves
ggplot() +
  geom_line(data = rf_roc_data, aes(x = 1 - Specificity, y = Sensitivity, color = "Random Forest"), size = 1.2) +
  geom_line(data = logistic_roc_data, aes(x = 1 - Specificity, y = Sensitivity, color = "Logistic Regression"), size = 1.2) +
  labs(title = "ROC Curve Comparison",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  scale_color_manual(values = c("Random Forest" = "blue", "Logistic Regression" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )


# Model Selection
if (rf_auc > logistic_auc) {
  cat("Selected Model: Random Forest\n")
  final_model <- rf_model
} else {
  cat("Selected Model: Logistic Regression\n")
  final_model <- logistic_model
}

# Save Selected Model
saveRDS(final_model, "Selected_Model.rds")
cat("Final model saved as 'Selected_Model.rds'.\n")
