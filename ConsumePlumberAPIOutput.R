# Install and load the required packages
if (!require("httr")) {
  install.packages("httr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
if (!require("jsonlite")) {
  install.packages("jsonlite", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
library(httr)
library(jsonlite)

# We set this as a constant port 5022 running on localhost
base_url <- "http://127.0.0.1:5022/rainfall"

# Create a named list called "params" with input parameters
params <- list(
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

# Modify the URL with parameters
query_url <- modify_url(url = base_url, query = params)

# Get the URL
print(query_url)

# Make a request to the Plumber API
model_prediction <- GET(query_url)

# Extract the results from the response
prediction_result <- content(model_prediction, as = "text", encoding = "utf-8")

# Parse the response into the right format
parsed_result <- fromJSON(prediction_result)

# Print the parsed result
print(parsed_result)
