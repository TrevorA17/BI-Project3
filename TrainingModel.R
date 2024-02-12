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

# Install and load the boot package if not already installed
install.packages("boot")
library(boot)


# Set seed for reproducibility
set.seed(123)

# Define a function to compute the statistic of interest (e.g., mean of MEDV)
statistic_function <- function(data, indices) {
  subset_data <- data[indices, ]
  return(mean(subset_data$MEDV))
}

# Perform bootstrapping
boot_results <- boot(data = HousingData, statistic = statistic_function, R = 1000)

# Display bootstrap results
cat("Bootstrap Statistics:\n")
print(boot_results)

# Plot the bootstrap distribution
plot(boot_results, type = "basic", col = "blue", pch = 20)

# Install and load the caret package if not already installed
install.packages("caret")
library(caret)

# Assuming the dataset is already loaded as HousingData

# Set seed for reproducibility
set.seed(123)

# Define the training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Basic k-fold cross-validation
model_basic <- train(MEDV ~ ., data = HousingData, method = "lm", trControl = train_control)
print(model_basic)

# Repeated k-fold cross-validation (5 repetitions)
model_repeated <- train(MEDV ~ ., data = HousingData, method = "lm", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5))
print(model_repeated)

# Leave-One-Out Cross-Validation (LOOCV)
model_loocv <- train(MEDV ~ ., data = HousingData, method = "lm", trControl = trainControl(method = "LOOCV"))
print(model_loocv)

# Install and load the caret package if not already installed
install.packages("caret")
library(caret)

# Assuming the dataset is already loaded as HousingData

# Set seed for reproducibility
set.seed(123)

# Define the training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train a linear regression model
model <- train(MEDV ~ ., data = HousingData, method = "lm", trControl = train_control)

# Display the trained model
print(model)

# Make predictions on the training set (for illustration purposes)
predictions <- predict(model, newdata = HousingData)

# Evaluate the model (for illustration purposes)
rmse <- sqrt(mean((predictions - HousingData$MEDV)^2))
cat("Root Mean Squared Error (RMSE):", round(rmse, 2), "\n")


