House Price Prediction Model
================
Trevor Okinda

- [Author Details](#author-details)
- [Setup Chunk](#setup-chunk)
- [Understanding the Dataset (Exploratory Data Analysis
  (EDA))](#understanding-the-dataset-exploratory-data-analysis-eda)
  - [Loading the Dataset](#loading-the-dataset)
    - [Source:](#source)
    - [Reference:](#reference)

# Author Details

|                       |                              |
|-----------------------|------------------------------|
| **Student ID Number** | 134780                       |
| **Student Name**      | Trevor Okinda                |
| **BBIT 4.2 Group**    | C                            |
| **Project Name**      | House Price Prediction Model |

# Setup Chunk

**Note:** the following KnitR options have been set as the global
defaults: <BR>
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

# Understanding the Dataset (Exploratory Data Analysis (EDA))

## Loading the Dataset

### Source:

The dataset that was used can be downloaded here: *\<<a
href="https://www.kaggle.com/datasets/fedesoriano/the-boston-houseprice-data\"
class="uri">https://www.kaggle.com/datasets/fedesoriano/the-boston-houseprice-data\</a>\>*

### Reference:

*\<Avagyan, Z. (2017). Weather CSV \[Data set\]. Kaggle.
<a href="https://www.kaggle.com/datasets/zaraavagyan/weathercsv\"
class="uri">https://www.kaggle.com/datasets/zaraavagyan/weathercsv\</a>\>  
Refer to the APA 7th edition manual for rules on how to cite datasets:
<https://apastyle.apa.org/style-grammar-guidelines/references/examples/data-set-references>*

``` r
# Load dataset
HousingData <- read.csv("boston.csv", colClasses = c(
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
```

``` r
#Measures of frequency
# Summary statistics for numeric variables
summary(HousingData)
```

    ##       CRIM                ZN             INDUS       CHAS         NOX        
    ##  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   0:471   Min.   :0.3850  
    ##  1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1: 35   1st Qu.:0.4490  
    ##  Median : 0.25651   Median :  0.00   Median : 9.69           Median :0.5380  
    ##  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14           Mean   :0.5547  
    ##  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10           3rd Qu.:0.6240  
    ##  Max.   :88.97620   Max.   :100.00   Max.   :27.74           Max.   :0.8710  
    ##        RM             AGE              DIS              RAD        
    ##  Min.   :3.561   Min.   :  2.90   Min.   : 1.130   Min.   : 1.000  
    ##  1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100   1st Qu.: 4.000  
    ##  Median :6.208   Median : 77.50   Median : 3.207   Median : 5.000  
    ##  Mean   :6.285   Mean   : 68.57   Mean   : 3.795   Mean   : 9.549  
    ##  3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188   3rd Qu.:24.000  
    ##  Max.   :8.780   Max.   :100.00   Max.   :12.127   Max.   :24.000  
    ##       TAX           PTRATIO            B              LSTAT      
    ##  Min.   :187.0   Min.   :12.60   Min.   :  0.32   Min.   : 1.73  
    ##  1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38   1st Qu.: 6.95  
    ##  Median :330.0   Median :19.05   Median :391.44   Median :11.36  
    ##  Mean   :408.2   Mean   :18.46   Mean   :356.67   Mean   :12.65  
    ##  3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23   3rd Qu.:16.95  
    ##  Max.   :711.0   Max.   :22.00   Max.   :396.90   Max.   :37.97  
    ##       MEDV      
    ##  Min.   : 5.00  
    ##  1st Qu.:17.02  
    ##  Median :21.20  
    ##  Mean   :22.53  
    ##  3rd Qu.:25.00  
    ##  Max.   :50.00

``` r
# Frequency distribution for categorical variables
table(HousingData$CHAS)  
```

    ## 
    ##   0   1 
    ## 471  35

``` r
#Measures of Central Tendency
# Calculate mean, median, and mode for RM (number of rooms)
mean_RM <- mean(HousingData$RM, na.rm = TRUE)
median_RM <- median(HousingData$RM, na.rm = TRUE)
mode_RM <- as.numeric(names(table(HousingData$RM))[which.max(table(HousingData$RM))])

cat("Measures of Central Tendency for RM (Number of Rooms):\n")
```

    ## Measures of Central Tendency for RM (Number of Rooms):

``` r
cat(paste0("Mean: ", round(mean_RM, 2)), "\n")
```

    ## Mean: 6.28

``` r
cat(paste0("Median: ", median_RM), "\n")
```

    ## Median: 6.2085

``` r
cat(paste0("Mode: ", mode_RM), "\n\n")
```

    ## Mode: 5.713

``` r
# Calculate mean, median, and mode for MEDV (Median value of owner-occupied homes in $1000s)
mean_MEDV <- mean(HousingData$MEDV, na.rm = TRUE)
median_MEDV <- median(HousingData$MEDV, na.rm = TRUE)
mode_MEDV <- as.numeric(names(table(HousingData$MEDV))[which.max(table(HousingData$MEDV))])

cat("Measures of Central Tendency for MEDV (Median Home Value):\n")
```

    ## Measures of Central Tendency for MEDV (Median Home Value):

``` r
cat(paste0("Mean: ", round(mean_MEDV, 2)), "\n")
```

    ## Mean: 22.53

``` r
cat(paste0("Median: ", median_MEDV), "\n")
```

    ## Median: 21.2

``` r
cat(paste0("Mode: ", mode_MEDV), "\n\n")
```

    ## Mode: 50

``` r
# Calculate mean, median, and mode for AGE (proportion of owner-occupied units built prior to 1940)
mean_AGE <- mean(HousingData$AGE, na.rm = TRUE)
median_AGE <- median(HousingData$AGE, na.rm = TRUE)
mode_AGE <- as.numeric(names(table(HousingData$AGE))[which.max(table(HousingData$AGE))])

cat("Measures of Central Tendency for AGE (Proportion of Units Built Prior to 1940):\n")
```

    ## Measures of Central Tendency for AGE (Proportion of Units Built Prior to 1940):

``` r
cat(paste0("Mean: ", round(mean_AGE, 2)), "\n")
```

    ## Mean: 68.57

``` r
cat(paste0("Median: ", median_AGE), "\n")
```

    ## Median: 77.5

``` r
cat(paste0("Mode: ", mode_AGE), "\n\n")
```

    ## Mode: 100

``` r
# Calculate mean, median, and mode for TAX (full-value property tax rate per $10,000)
mean_TAX <- mean(HousingData$TAX, na.rm = TRUE)
median_TAX <- median(HousingData$TAX, na.rm = TRUE)
mode_TAX <- as.numeric(names(table(HousingData$TAX))[which.max(table(HousingData$TAX))])

cat("Measures of Central Tendency for TAX (Property Tax Rate):\n")
```

    ## Measures of Central Tendency for TAX (Property Tax Rate):

``` r
cat(paste0("Mean: ", round(mean_TAX, 2)), "\n")
```

    ## Mean: 408.24

``` r
cat(paste0("Median: ", median_TAX), "\n")
```

    ## Median: 330

``` r
cat(paste0("Mode: ", mode_TAX), "\n\n")
```

    ## Mode: 666

``` r
# Calculate range, variance, and standard deviation for RM (number of rooms)
range_RM <- range(HousingData$RM, na.rm = TRUE)
variance_RM <- var(HousingData$RM, na.rm = TRUE)
sd_RM <- sd(HousingData$RM, na.rm = TRUE)

cat("Measures of Distribution for RM (Number of Rooms):\n")
```

    ## Measures of Distribution for RM (Number of Rooms):

``` r
cat(paste0("Range: ", paste(range_RM, collapse = " - ")), "\n")
```

    ## Range: 3.561 - 8.78

``` r
cat(paste0("Variance: ", round(variance_RM, 2)), "\n")
```

    ## Variance: 0.49

``` r
cat(paste0("Standard Deviation: ", round(sd_RM, 2)), "\n\n")
```

    ## Standard Deviation: 0.7

``` r
# Calculate range, variance, and standard deviation for MEDV (Median Home Value)
range_MEDV <- range(HousingData$MEDV, na.rm = TRUE)
variance_MEDV <- var(HousingData$MEDV, na.rm = TRUE)
sd_MEDV <- sd(HousingData$MEDV, na.rm = TRUE)

cat("Measures of Distribution for MEDV (Median Home Value):\n")
```

    ## Measures of Distribution for MEDV (Median Home Value):

``` r
cat(paste0("Range: ", paste(range_MEDV, collapse = " - ")), "\n")
```

    ## Range: 5 - 50

``` r
cat(paste0("Variance: ", round(variance_MEDV, 2)), "\n")
```

    ## Variance: 84.59

``` r
cat(paste0("Standard Deviation: ", round(sd_MEDV, 2)), "\n\n")
```

    ## Standard Deviation: 9.2

``` r
# Calculate range, variance, and standard deviation for additional variables in the Boston Housing dataset

# Calculate range, variance, and standard deviation for AGE (proportion of owner-occupied units built prior to 1940)
range_AGE <- range(HousingData$AGE, na.rm = TRUE)
variance_AGE <- var(HousingData$AGE, na.rm = TRUE)
sd_AGE <- sd(HousingData$AGE, na.rm = TRUE)

cat("Measures of Distribution for AGE (Proportion of Units Built Prior to 1940):\n")
```

    ## Measures of Distribution for AGE (Proportion of Units Built Prior to 1940):

``` r
cat(paste0("Range: ", paste(range_AGE, collapse = " - ")), "\n")
```

    ## Range: 2.9 - 100

``` r
cat(paste0("Variance: ", round(variance_AGE, 2)), "\n")
```

    ## Variance: 792.36

``` r
cat(paste0("Standard Deviation: ", round(sd_AGE, 2)), "\n\n")
```

    ## Standard Deviation: 28.15

``` r
# Calculate range, variance, and standard deviation for TAX (full-value property tax rate per $10,000)
range_TAX <- range(HousingData$TAX, na.rm = TRUE)
variance_TAX <- var(HousingData$TAX, na.rm = TRUE)
sd_TAX <- sd(HousingData$TAX, na.rm = TRUE)

cat("Measures of Distribution for TAX (Property Tax Rate):\n")
```

    ## Measures of Distribution for TAX (Property Tax Rate):

``` r
cat(paste0("Range: ", paste(range_TAX, collapse = " - ")), "\n")
```

    ## Range: 187 - 711

``` r
cat(paste0("Variance: ", round(variance_TAX, 2)), "\n")
```

    ## Variance: 28404.76

``` r
cat(paste0("Standard Deviation: ", round(sd_TAX, 2)), "\n\n")
```

    ## Standard Deviation: 168.54

``` r
# Calculate correlation between RM (number of rooms) and MEDV (Median Home Value)
correlation_RM_MEDV <- cor(HousingData$RM, HousingData$MEDV, use = "complete.obs")

cat("Measures of Relationship (Correlation) between RM and MEDV:\n")
```

    ## Measures of Relationship (Correlation) between RM and MEDV:

``` r
cat(paste0("Correlation Coefficient: ", round(correlation_RM_MEDV, 2)), "\n\n")
```

    ## Correlation Coefficient: 0.7

``` r
# Calculate correlation between AGE (proportion of units built prior to 1940) and TAX (property tax rate)
correlation_AGE_TAX <- cor(HousingData$AGE, HousingData$TAX, use = "complete.obs")

cat("Measures of Relationship (Correlation) between AGE and TAX:\n")
```

    ## Measures of Relationship (Correlation) between AGE and TAX:

``` r
cat(paste0("Correlation Coefficient: ", round(correlation_AGE_TAX, 2)), "\n\n")
```

    ## Correlation Coefficient: 0.51

``` r
# Perform ANOVA for the "CHAS" variable against the "RM" variable (number of rooms)
anova_result <- aov(RM ~ CHAS, data = HousingData)

# Print the ANOVA table
cat("ANOVA Results for RM (Number of Rooms) vs CHAS:\n")
```

    ## ANOVA Results for RM (Number of Rooms) vs CHAS:

``` r
print(summary(anova_result))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)  
    ## CHAS          1   2.08  2.0759   4.232 0.0402 *
    ## Residuals   504 247.23  0.4905                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
