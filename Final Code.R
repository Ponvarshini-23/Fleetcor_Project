# Load necessary libraries
library(dplyr)
library(readxl)

# Define file paths for each dataset
files <- list(
  "All Acct Segment Score.xlsx",
  "Cross-Sell Acct.xlsx",
  "Non Cross-Sell Acct Info.xlsx",
  "Non Cross-Sell Aging Data.xlsx",
  "Non Cross-Sell DNB Score.xlsx",
  "Non Cross-Sell NSF Payment.xlsx",
  "Non Cross-Sell Payment.xlsx",
  "Non Cross-Sell Revenue.xlsx",
  "Non Cross-Sell Spend.xlsx",
  "Non Cross-Sell Vantage Score.xlsx",
  "Non Cross-Sell Write-Off.xlsx"
)

# Step 1: Load and Process Each Dataset Separately to Ensure Unique FAKE_ACCTCODE
# This loop will load each file, ensure unique `FAKE_ACCTCODE`, and retain only necessary columns

merged_data <- NULL  # Initialize merged_data as NULL

for (file in files) {
  temp_data <- read_excel(file, sheet = 1) %>%
    distinct(FAKE_ACCTCODE, .keep_all = TRUE)  # Keep only unique FAKE_ACCTCODE entries
  
  # Retain only necessary columns to reduce memory usage if needed
  # temp_data <- temp_data %>% select(FAKE_ACCTCODE, <other columns needed>)
  
  if (is.null(merged_data)) {
    merged_data <- temp_data  # Set the first dataset as the base
  } else {
    merged_data <- full_join(merged_data, temp_data, by = "FAKE_ACCTCODE", relationship = "many-to-many")
  }
}

# Step 2: Handle Missing Values
numeric_cols <- sapply(merged_data, is.numeric)
categorical_cols <- sapply(merged_data, is.character)

merged_data[numeric_cols] <- lapply(merged_data[numeric_cols], function(x) ifelse(is.na(x), 0, x))
merged_data[categorical_cols] <- lapply(merged_data[categorical_cols], function(x) ifelse(is.na(x), "Unknown", x))

# Step 3: Remove Duplicates
merged_data <- merged_data %>% distinct(FAKE_ACCTCODE, .keep_all = TRUE)

# Step 4: Sample Data (if dataset is large, sample before exporting)
set.seed(42)  # for reproducibility
sample_data <- merged_data %>% sample_n(min(500, nrow(merged_data)))

# Step 5: Export Data
write.csv(sample_data, "Cleaned_Sample_Cross_Sell_Eligibility.csv", row.names = FALSE)

# Output message
cat("Data cleaning and sampling complete. File 'Cleaned_Sample_Cross_Sell_Eligibility.csv' is ready.")
