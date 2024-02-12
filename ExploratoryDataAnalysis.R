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


# Calculate mean, median, and mode for AGE (proportion of owner-occupied units built prior to 1940)
mean_AGE <- mean(HousingData$AGE, na.rm = TRUE)
median_AGE <- median(HousingData$AGE, na.rm = TRUE)
mode_AGE <- as.numeric(names(table(HousingData$AGE))[which.max(table(HousingData$AGE))])

cat("Measures of Central Tendency for AGE (Proportion of Units Built Prior to 1940):\n")
cat(paste0("Mean: ", round(mean_AGE, 2)), "\n")
cat(paste0("Median: ", median_AGE), "\n")
cat(paste0("Mode: ", mode_AGE), "\n\n")

# Calculate mean, median, and mode for TAX (full-value property tax rate per $10,000)
mean_TAX <- mean(HousingData$TAX, na.rm = TRUE)
median_TAX <- median(HousingData$TAX, na.rm = TRUE)
mode_TAX <- as.numeric(names(table(HousingData$TAX))[which.max(table(HousingData$TAX))])

cat("Measures of Central Tendency for TAX (Property Tax Rate):\n")
cat(paste0("Mean: ", round(mean_TAX, 2)), "\n")
cat(paste0("Median: ", median_TAX), "\n")
cat(paste0("Mode: ", mode_TAX), "\n\n")

