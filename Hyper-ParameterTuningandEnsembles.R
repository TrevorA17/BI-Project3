# Install and load the caret package if not already installed
# install.packages("caret")
library(caret)

# Assuming the dataset is already loaded as HousingData

# Set seed for reproducibility
set.seed(123)

# Define the training control for cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train a bagged model using Random Forest
model_bagging <- train(MEDV ~ ., data = HousingData, method = "rf", trControl = train_control)

# Display the bagged model
print(model_bagging)





