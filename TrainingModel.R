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
# Install and load the caret package if not already installed
install.packages("caret")
library(caret)

# Set seed for reproducibility
set.seed(123)

# Create an index for splitting the dataset (70% for training, 30% for testing)
split_index <- createDataPartition(HousingData$MEDV, p = 0.7, list = FALSE)

# Split the dataset into training and testing sets
train_data <- HousingData[split_index, ]
test_data <- HousingData[-split_index, ]

# Display the dimensions of the training and testing sets
cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")

