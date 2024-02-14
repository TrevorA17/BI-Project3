# Load required libraries
library(plumber)

# Load the saved Linear Regression model
loaded_lr_model <- readRDS("./models/saved_lr_model.rds")

# Define the Plumber API
#* @apiTitle Boston Housing Regression Model API
#* @apiDescription Used to predict the median value of owner-occupied homes in Boston.

#* @param CRIM Per capita crime rate
#* @param ZN Proportion of residential land zoned for large lots
#* @param INDUS Proportion of non-retail business acres
#* @param CHAS Charles River dummy variable (1 if tract bounds river, 0 otherwise)
#* @param NOX Nitric oxides concentration
#* @param RM Average number of rooms per dwelling
#* @param AGE Proportion of units built prior to 1940
#* @param DIS Weighted distances to five Boston employment centers
#* @param RAD Index of accessibility to radial highways
#* @param TAX Property tax rate
#* @param PTRATIO Pupil-teacher ratio
#* @param B Proportion of residents of African American descent
#* @param LSTAT Percentage of lower status of the population

#* @get /predict_medv
#* @param CRIM
#* @param ZN
#* @param INDUS
#* @param CHAS
#* @param NOX
#* @param RM
#* @param AGE
#* @param DIS
#* @param RAD
#* @param TAX
#* @param PTRATIO
#* @param B
#* @param LSTAT
predict_medv <- function(
    CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, B, LSTAT
) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    CRIM = as.numeric(CRIM),
    ZN = as.numeric(ZN),
    INDUS = as.numeric(INDUS),
    CHAS = as.factor(CHAS),
    NOX = as.numeric(NOX),
    RM = as.numeric(RM),
    AGE = as.numeric(AGE),
    DIS = as.numeric(DIS),
    RAD = as.numeric(RAD),
    TAX = as.numeric(TAX),
    PTRATIO = as.numeric(PTRATIO),
    B = as.numeric(B),
    LSTAT = as.numeric(LSTAT)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_lr_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(prediction)
}

