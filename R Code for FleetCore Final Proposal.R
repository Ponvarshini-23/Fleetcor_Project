# Load necessary libraries
library(ggplot2)
library(tidyr)

# 1. Feature Importance Chart
features <- c("Credit Limit", "Vantage Score", "Fuel Spend", "Non-Fuel Spend")
importance <- c(0.4, 0.3, 0.2, 0.1)

feature_importance <- data.frame(Feature = features, Importance = importance)

ggplot(feature_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Feature Importance for Cross-Sell Model",
       x = "Feature", y = "Importance") +
  theme_minimal()

# 2. Predicted vs Actual Chart
set.seed(123)
actual <- runif(100, 1000, 5000)  # Actual revenue values
predicted <- actual * 0.95 + rnorm(100, 0, 100)  # Mock predicted values

predicted_actual <- data.frame(Actual = actual, Predicted = predicted)

ggplot(predicted_actual, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Revenue",
       x = "Actual Revenue", y = "Predicted Revenue") +
  theme_minimal()

# 3. Risk Segment Pie Chart
risk_segments <- data.frame(Segment = c("Low Risk", "High Risk"),
                            Count = c(60, 40))

ggplot(risk_segments, aes(x = "", y = Count, fill = Segment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Customer Risk Segments") +
  theme_void() +
  scale_fill_manual(values = c("lightgreen", "salmon"))

# 4. Eligibility Trend Over Time
set.seed(123)
dates <- seq(as.Date("2023-01-01"), by = "month", length.out = 12)
eligible <- sample(50:100, 12, replace = TRUE)
not_eligible <- sample(10:50, 12, replace = TRUE)

trend <- data.frame(Date = dates, Eligible = eligible, Not_Eligible = not_eligible)
trend_long <- pivot_longer(trend, cols = c("Eligible", "Not_Eligible"),
                           names_to = "Status", values_to = "Count")

ggplot(trend_long, aes(x = Date, y = Count, color = Status, group = Status)) +
  geom_line(size = 1) +
  labs(title = "Eligibility Trend Over Time",
       x = "Date", y = "Number of Customers") +
  theme_minimal() +
  scale_color_manual(values = c("Eligible" = "blue", "Not_Eligible" = "red"))
