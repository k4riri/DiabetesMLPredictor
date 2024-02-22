# Load necessary libraries
library(dplyr)
library(caret)
library(ROSE)
library(kernlab)
# ... include other libraries as needed ...

# Load the data
diabetes_data <- read.csv("DATASET.csv")

# Data Sampling
set.seed(123)
sample_size <- 500
sample_indices <- sample(nrow(diabetes_data), sample_size, replace = FALSE)
sampled_data <- diabetes_data[sample_indices, ]

# Oversampling using ROSE
data_upsampled <- ROSE::ovun.sample(diabetes ~ ., data = sampled_data, method = "over", seed = 123)$data

# Feature selection and normalization (example, adjust as needed)
dataA <- data_upsampled[, c("age", "bmi", "HbA1c_level", "blood_glucose_level")]
minmaxnorm <- function(x) { (x - min(x)) / (max(x) - min(x)) }
datanorm <- as.data.frame(lapply(dataA, minmaxnorm))

# Combining the normalized data with other variables (adjust according to your dataset)
dataY <- data_upsampled
dataY[, c("age", "bmi", "HbA1c_level", "blood_glucose_level")] <- datanorm

# Convert factors if necessary
dataY$smoking_history <- as.factor(dataY$smoking_history)
dataY$hypertension <- as.factor(dataY$hypertension)
dataY$heart_disease <- as.factor(dataY$heart_disease)
dataY$diabetes <- as.factor(dataY$diabetes)

# Save the processed data
saveRDS(dataY, "Desktop/processed_data.rds")
