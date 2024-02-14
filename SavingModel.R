# Install and load the necessary packages if not already installed
# install.packages("caret")

library(caret)

# Assuming the dataset is already loaded as HousingData

# Set seed for reproducibility
set.seed(123)

# Define the training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train Linear Regression Model
lr_model <- train(
  MEDV ~ .,
  data = HousingData,
  method = "lm",
  trControl = train_control
)

# Save the Linear Regression model
saveRDS(lr_model, "./models/saved_lr_model.rds")

# Load the saved Linear Regression model
loaded_lr_model <- readRDS("./models/saved_lr_model.rds")

# Arrange variables in the desired order for prediction
new_data <- data.frame(
  CRIM = 0.00632,
  ZN = 18,
  INDUS = 2.31,
  CHAS = "0",
  NOX = 0.538,
  RM = 6.575,
  AGE = 65.2,
  DIS = 4.09,
  RAD = 1,
  TAX = 296,
  PTRATIO = 15.3,
  B = 396.9,
  LSTAT = 4.98
)

# Use the loaded Linear Regression model to make predictions
predictions_loaded_lr_model <- predict(loaded_lr_model, newdata = new_data)

# Print predictions
cat("Predicted MEDV:", predictions_loaded_lr_model, "\n")
