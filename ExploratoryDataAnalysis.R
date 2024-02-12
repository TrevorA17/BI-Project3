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

# Summary statistics for numeric variables
summary(HousingData)

# Frequency distribution for categorical variables
table(HousingData$CHAS)  



