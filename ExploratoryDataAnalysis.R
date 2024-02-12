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

# Calculate mean, median, and mode for RM (number of rooms)
mean_RM <- mean(HousingData$RM, na.rm = TRUE)
median_RM <- median(HousingData$RM, na.rm = TRUE)
mode_RM <- as.numeric(names(table(HousingData$RM))[which.max(table(HousingData$RM))])

cat("Measures of Central Tendency for RM (Number of Rooms):\n")
cat(paste0("Mean: ", round(mean_RM, 2)), "\n")
cat(paste0("Median: ", median_RM), "\n")
cat(paste0("Mode: ", mode_RM), "\n\n")

# Calculate mean, median, and mode for MEDV (Median value of owner-occupied homes in $1000s)
mean_MEDV <- mean(HousingData$MEDV, na.rm = TRUE)
median_MEDV <- median(HousingData$MEDV, na.rm = TRUE)
mode_MEDV <- as.numeric(names(table(HousingData$MEDV))[which.max(table(HousingData$MEDV))])

cat("Measures of Central Tendency for MEDV (Median Home Value):\n")
cat(paste0("Mean: ", round(mean_MEDV, 2)), "\n")
cat(paste0("Median: ", median_MEDV), "\n")
cat(paste0("Mode: ", mode_MEDV), "\n\n")


