#### Preamble ####
# Purpose: Models using SVM 
# Author: Rahma Binth Mohammad
# Contact: rahma.binthmohammad@mail.utoronto.ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? This code was explicitly used for the prediction submission

# Workspace setup 
library(tidyverse)
library(e1071)
library(dplyr)

# Read in the data
train_data <- read.csv("data/01-raw_data/train.csv", stringsAsFactors = FALSE)
test_data <- read.csv("data/01-raw_data/test.csv", stringsAsFactors = FALSE)

#Check for NA values, none found
colSums(is.na(train_data))
colSums(is.na(test_data))

# Remove the 'DoctorInCharge' column, irrelevant to our problem
train_data <- select(train_data, -DoctorInCharge)
test_data <- select(test_data, -DoctorInCharge)

# Remove the 'PatientID' column from the training data irrelevant to our problem
train_data <- select(train_data, -PatientID)

#Convert diagnosis to categorical variable
#train_data$Diagnosis <- as.factor(train_data$Diagnosis)

# Convert all 0/1 columns to factors, just in case?
binary_to_factor <- function(df) {
  binary_cols <- sapply(df, function(col) all(col %in% c(0, 1)))
  df[binary_cols] <- lapply(df[binary_cols], factor)
  return(df)
}

train_data <- binary_to_factor(train_data)
test_data <- binary_to_factor(test_data)

# Select features to be used in SVM based on the trends found previously
selected_features <- c("Ethnicity", 
                       "EducationLevel",
                       "MMSE",
                       "MemoryComplaints", 
                       "ADL", 
                       "FunctionalAssessment", 
                       "CholesterolTotal", 
                       "BehavioralProblems",
                       "FamilyHistoryAlzheimers",
                       "Confusion", "Disorientation",
                       "HeadInjury", "Forgetfulness", "DifficultyCompletingTasks",
                       "PersonalityChanges")

# Create a subset to contain the selected features and the target variable for the training data
train_selected <- train_data[, c(selected_features, "Diagnosis")]
# Create a subset to contain the selected features and the Patient ID for the test data
test_selected <- test_data[, c(selected_features, "PatientID")]

# Set seed for SVM model
set.seed(123)

#SVM - fine tuned
svm_model <- svm(Diagnosis ~ ., 
                 data = train_selected, 
                 kernel = "radial",  
                 cost = 20,  
                 gamma = 0.1, 
                 scale = TRUE,
                 class.weights = c("0" = 1, "1" = 1.3))   # useful if your features are on different scales

predictions_for_sub <- predict(svm_model, newdata = test_selected[, c(selected_features, "PatientID")])

# Create data table with PatientID and predictions 
prediction_results <- data.frame(
  PatientID = test_selected$PatientID,
  Diagnosis = as.numeric(as.character(predictions_for_sub))  # Convert to numeric
)
print(prediction_results)

# Writing data to a CSV file
write.csv(prediction_results, "data/02-prediction_data/submission.csv", row.names = FALSE)

# Save model in case to be used later if needed
saveRDS(
  svm_model,
  file = "models/svm_model.rds"
)

#Use this code to load the model later if needed
# loaded_model <- readRDS("models/svm_model.rds")


