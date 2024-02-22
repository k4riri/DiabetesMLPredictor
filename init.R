# Install required packages
packages <- c("shiny", "caret", "glmnet", "randomForest", "pROC", "e1071", "dplyr", "ROSE", "kernlab")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to each required package
sapply(packages, install_if_missing)
