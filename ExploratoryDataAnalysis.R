# Load dataset
HousingData <- read.csv("data/boston.csv", colClasses = c(
  CRIM = "numeric",
  ZN = "numeric",
  INDUS = "numeric",
  CHAS = "numeric",
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
