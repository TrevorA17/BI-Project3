# Load dataset
HousingData <- read.csv("data/boston.csv", colClasses = c(
  CRIM = "numeric",
  ZN = "numeric",
  INDUS = "numeric",
  CHAS = "factor",  # Corrected from "numeric" to "factor"
  NOX = "numeric",
  RM = "numeric",
  AGE = "numeric",
  DIS = "numeric",
  RAD = "numeric",
  TAX = "numeric",
  PTRATIO = "numeric",
  B = "numeric",
  LSTAT = "numeric",
  MEDV = "numeric"
))

# Display the dataset
View(HousingData)

# Check for missing values in each column
missing_values <- colSums(is.na(HousingData))

# Print columns with missing values (if any)
columns_with_missing <- names(missing_values[missing_values > 0])
if (length(columns_with_missing) > 0) {
  cat("Columns with missing values:\n")
  print(columns_with_missing)
} else {
  cat("No missing values found in the dataset.\n")
}
