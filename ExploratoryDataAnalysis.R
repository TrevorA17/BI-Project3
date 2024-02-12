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

# Calculate range, variance, and standard deviation for RM (number of rooms)
range_RM <- range(HousingData$RM, na.rm = TRUE)
variance_RM <- var(HousingData$RM, na.rm = TRUE)
sd_RM <- sd(HousingData$RM, na.rm = TRUE)

cat("Measures of Distribution for RM (Number of Rooms):\n")
cat(paste0("Range: ", paste(range_RM, collapse = " - ")), "\n")
cat(paste0("Variance: ", round(variance_RM, 2)), "\n")
cat(paste0("Standard Deviation: ", round(sd_RM, 2)), "\n\n")

# Calculate range, variance, and standard deviation for MEDV (Median Home Value)
range_MEDV <- range(HousingData$MEDV, na.rm = TRUE)
variance_MEDV <- var(HousingData$MEDV, na.rm = TRUE)
sd_MEDV <- sd(HousingData$MEDV, na.rm = TRUE)

cat("Measures of Distribution for MEDV (Median Home Value):\n")
cat(paste0("Range: ", paste(range_MEDV, collapse = " - ")), "\n")
cat(paste0("Variance: ", round(variance_MEDV, 2)), "\n")
cat(paste0("Standard Deviation: ", round(sd_MEDV, 2)), "\n\n")

# Calculate range, variance, and standard deviation for additional variables in the Boston Housing dataset

# Calculate range, variance, and standard deviation for AGE (proportion of owner-occupied units built prior to 1940)
range_AGE <- range(HousingData$AGE, na.rm = TRUE)
variance_AGE <- var(HousingData$AGE, na.rm = TRUE)
sd_AGE <- sd(HousingData$AGE, na.rm = TRUE)

cat("Measures of Distribution for AGE (Proportion of Units Built Prior to 1940):\n")
cat(paste0("Range: ", paste(range_AGE, collapse = " - ")), "\n")
cat(paste0("Variance: ", round(variance_AGE, 2)), "\n")
cat(paste0("Standard Deviation: ", round(sd_AGE, 2)), "\n\n")

# Calculate range, variance, and standard deviation for TAX (full-value property tax rate per $10,000)
range_TAX <- range(HousingData$TAX, na.rm = TRUE)
variance_TAX <- var(HousingData$TAX, na.rm = TRUE)
sd_TAX <- sd(HousingData$TAX, na.rm = TRUE)

cat("Measures of Distribution for TAX (Property Tax Rate):\n")
cat(paste0("Range: ", paste(range_TAX, collapse = " - ")), "\n")
cat(paste0("Variance: ", round(variance_TAX, 2)), "\n")
cat(paste0("Standard Deviation: ", round(sd_TAX, 2)), "\n\n")

# Calculate correlation between RM (number of rooms) and MEDV (Median Home Value)
correlation_RM_MEDV <- cor(HousingData$RM, HousingData$MEDV, use = "complete.obs")

cat("Measures of Relationship (Correlation) between RM and MEDV:\n")
cat(paste0("Correlation Coefficient: ", round(correlation_RM_MEDV, 2)), "\n\n")

# Calculate correlation between AGE (proportion of units built prior to 1940) and TAX (property tax rate)
correlation_AGE_TAX <- cor(HousingData$AGE, HousingData$TAX, use = "complete.obs")

cat("Measures of Relationship (Correlation) between AGE and TAX:\n")
cat(paste0("Correlation Coefficient: ", round(correlation_AGE_TAX, 2)), "\n\n")

# Assuming the dataset is already loaded as HousingData

# Perform ANOVA for the "CHAS" variable against the "RM" variable (number of rooms)
anova_result <- aov(RM ~ CHAS, data = HousingData)

# Print the ANOVA table
cat("ANOVA Results for RM (Number of Rooms) vs CHAS:\n")
print(summary(anova_result))

# Install and load ggplot2 if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Assuming the dataset is already loaded as HousingData

# Univariate plot for RM (number of rooms)
ggplot(HousingData, aes(x = RM)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of RM (Number of Rooms)",
       x = "Number of Rooms",
       y = "Frequency")

# Univariate plot for MEDV (Median Home Value)
ggplot(HousingData, aes(x = MEDV)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of MEDV (Median Home Value)",
       x = "Median Home Value ($1000s)",
       y = "Frequency")

# Univariate plot for AGE (proportion of owner-occupied units built prior to 1940)
ggplot(HousingData, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution of AGE (Proportion of Units Built Prior to 1940)",
       x = "Age of Units",
       y = "Frequency")

# Univariate plot for INDUS (proportion of non-retail business acres per town)
ggplot(HousingData, aes(x = INDUS)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Distribution of INDUS (Non-Retail Business Acres)",
       x = "Proportion of Non-Retail Business Acres",
       y = "Frequency")

# Univariate plot for TAX (full-value property tax rate per $10,000)
ggplot(HousingData, aes(x = TAX)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Distribution of TAX (Property Tax Rate)",
       x = "Property Tax Rate",
       y = "Frequency")

# Univariate plot for LSTAT (% lower status of the population)
ggplot(HousingData, aes(x = LSTAT)) +
  geom_histogram(binwidth = 1, fill = "brown", color = "black", alpha = 0.7) +
  labs(title = "Distribution of LSTAT (Percentage of Lower Status)",
       x = "Percentage of Lower Status",
       y = "Frequency")
