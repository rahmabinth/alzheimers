#### Preamble ####
# Purpose: Cleans Data, Finds Trends, and Tests Different Models
# Author: Rahma Binth Mohammad
# Contact: rahma.binthmohammad@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run the "00-install.packages.R "file

# Load packages, install beforehand if needed
library(dplyr)
library(ggplot2)
library(caret)
library(e1071)


# Read in the data
train_data <- read.csv("data/01-raw_data/train.csv", stringsAsFactors = FALSE)

test_data <- read.csv("data/01-raw_data/test.csv", stringsAsFactors = FALSE)

#Check for NA values, none found
colSums(is.na(train_data))
colSums(is.na(test_data))

# Remove the 'DoctorInCharge' column, irrelevant to our problem
train_data <- select(train_data, -DoctorInCharge)
test_data <- select(test_data, -DoctorInCharge)


##########         Check data and trends           ################

# Create a subset of data where Diagnosis indicates Yes (1) 
alzheimers_data <- subset(train_data, Diagnosis == 1)

# Summary of all columns when Diagnosis == 1
summary(alzheimers_data)

#Check if any are under 60
#train_under_60 <- subset(train_data, Age < 60)
#nrow(train_under_60)

#Check if any are under 60 and diagnosed
#alzheimers_under_60 <- subset(alzheimers_data, Age < 60)
#nrow(alzheimers_under_60)

#Check if gender makes a difference
d_women <- subset(alzheimers_data, Gender == 1)
nrow(d_women) 

d_men <- subset(alzheimers_data, Gender == 0)
nrow(d_men)

#Check if ethnicity makes a difference
d_caucasian <- subset(alzheimers_data, Ethnicity == 0)
nrow(d_caucasian) 

d_african_am <- subset(alzheimers_data, Ethnicity == 1)
nrow(d_african_am) 

d_asian <- subset(alzheimers_data, Ethnicity == 2)
nrow(d_asian) 

d_other_eth <- subset(alzheimers_data, Ethnicity == 3)
nrow(d_other_eth) 

#Check if education makes a difference
d_none_ed <- subset(alzheimers_data, EducationLevel == 0)
nrow(d_none_ed) 

d_highschool <- subset(alzheimers_data, EducationLevel == 1)
nrow(d_highschool) 

d_bachelors <- subset(alzheimers_data, EducationLevel == 2)
nrow(d_bachelors) 

d_higher_ed <- subset(alzheimers_data, EducationLevel == 3)
nrow(d_higher_ed) 

#Check how Lifestyle Factors differ
d_smoking <- subset(alzheimers_data, Smoking == 1)
nrow(d_smoking) 

d_non_smoking <- subset(alzheimers_data, Smoking == 0)
nrow(d_non_smoking) 

d_alcohol <- subset(alzheimers_data, AlcoholConsumption > 2) 
nrow(d_alcohol)

d_sleep <- subset(alzheimers_data, SleepQuality >9)
nrow(d_sleep)

#Check Family History 
d_family <- subset(alzheimers_data, FamilyHistoryAlzheimers == 1)
nrow(d_family)

#Check Systolic BP
d_systolicBP <- subset(alzheimers_data, SystolicBP >95)
nrow(d_systolicBP)

set.seed(123)
# Create bar plots to check certain Cognitive Assessments against the Diagnosis
ggplot(train_data, aes(x = as.factor(BehavioralProblems), 
                       fill = as.factor(Diagnosis))) +
  geom_bar(position = "dodge") +
  labs(title = "Alzheimer's Diagnosis vs. BehavioralProblems",
       x = "BehavioralProblems (0 = No, 1 = Yes)",
       y = "Count",
       fill = "Diagnosis (0 = No, 1 = Yes)") +
  theme_minimal()

ggplot(train_data, aes(x = ADL, fill = as.factor(Diagnosis))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  labs(title = "Alzheimer's Diagnosis vs. ADL",
       x = "ADL",
       y = "Count",
       fill = "Diagnosis (0 = No, 1 = Yes)") +
  theme_minimal()

# Create a table with the number of people who smoke, diagnosed and undiagnosed
train_data %>%
  filter(Smoking == 1) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

# Create a table with the number of people who drink, diagnosed and undiagnosed
train_data %>%
  filter(AlcoholConsumption > 15) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

## Create a table with the number of people who have a functional impairement
# Finding - Lower Scores = greater impairement
train_data %>%
  filter(FunctionalAssessment < 5) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

## Create a table with the number of people who have a functional impairement
# Lower Scores = greater impairement
train_data %>%
  filter(BehavioralProblems == 1) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

train_data %>%
  filter(BehavioralProblems == 0) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

train_data %>%
  filter(ADL < 5) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

train_data %>%
  filter(Confusion == 1) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

train_data %>%
  filter(Confusion == 0) %>%
  group_by(Diagnosis) %>%
  summarise(Count = n())

## Add a column that indicates if the person has at least one medical condition 
train_data <- train_data %>% 
  mutate(HasMedicalCondition = ifelse(FamilyHistoryAlzheimers == 1 | 
                                        CardiovascularDisease == 1 | 
                                        Diabetes == 1 | 
                                        Depression == 1 | 
                                        HeadInjury == 1 | 
                                        Hypertension == 1, 1, 0))

# Add a column that indicates if the person has at least one medical condition 
test_data <- test_data %>% 
  mutate(HasMedicalCondition = ifelse(FamilyHistoryAlzheimers == 1 | 
                                        CardiovascularDisease == 1 | 
                                        Diabetes == 1 | 
                                        Depression == 1 | 
                                        HeadInjury == 1 | 
                                        Hypertension == 1, 1, 0))

# Count the number of people in each group (HasMedicalCondition vs Diagnosis)
summary_data <- train_data %>%
  group_by(HasMedicalCondition, Diagnosis) %>%
  summarise(Count = n(), .groups = "drop")

# Plot HasMedicalCondition vs Diagnosis
ggplot(summary_data, aes(x = as.factor(HasMedicalCondition), 
                         y = Count, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Alzheimer's Diagnosis vs. Medical History",
       x = "Has One or More Medical Conditions (0 = No, 1 = Yes)",
       y = "Number of People",
       fill = "Diagnosis (0 = No, 1 = Yes)") +
  theme_minimal()

## Add a column that indicates if the person has at least one cognitive issue
train_data <- train_data %>% 
  mutate(HasCognitiveIssues = ifelse(MMSE < 10 | 
                                       FunctionalAssessment < 5 | 
                                       MemoryComplaints == 1 | 
                                       BehavioralProblems == 1 | 
                                       ADL < 5, 1, 0))

## Add a column that indicates if the person has at least one cognitive issue
test_data <- test_data %>% 
  mutate(HasCognitiveIssues = ifelse(MMSE < 10 | 
                                       FunctionalAssessment < 5 | 
                                       MemoryComplaints == 1 | 
                                       BehavioralProblems == 1 | 
                                       ADL < 5, 1, 0))

# Count the number of people in each group (HasCognitiveIssues vs Diagnosis)
summary_cognitive <- train_data %>%
  group_by(HasCognitiveIssues, Diagnosis) %>%
  summarise(Count = n(), .groups = "drop")

# Plot HasCognitiveIssues vs Diagnosis
ggplot(summary_cognitive, aes(x = as.factor(HasCognitiveIssues), 
                              y = Count, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Alzheimer's Diagnosis vs. Cognitive Issues",
       x = "Has One or More Congnitive Conditions (0 = No, 1 = Yes)",
       y = "Number of People",
       fill = "Diagnosis (0 = No, 1 = Yes)") +
  theme_minimal()

# Add a column that indicates if the person has at least one symptom
train_data <- train_data %>% 
  mutate(HasSymptoms = ifelse(Confusion == 1 | 
                                Disorientation == 1 | 
                                PersonalityChanges == 1 | 
                                DifficultyCompletingTasks == 1 | 
                                Forgetfulness == 1, 1, 0))

# Add a column that indicates if the person has at least one symptom
test_data <- test_data %>% 
  mutate(HasSymptoms = ifelse(Confusion == 1 | 
                                Disorientation == 1 | 
                                PersonalityChanges == 1 | 
                                DifficultyCompletingTasks == 1 | 
                                Forgetfulness == 1, 1, 0))

# Count the number of people in each group (HasSymptoms vs Diagnosis)
summary_symptoms <- train_data %>%
  group_by(HasSymptoms, Diagnosis) %>%
  summarise(Count = n(), .groups = "drop")

# Plot HasSymptoms vs Diagnosis
ggplot(summary_symptoms, aes(x = as.factor(HasSymptoms), 
                             y = Count, fill = as.factor(Diagnosis))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Alzheimer's Diagnosis vs. Symptoms",
       x = "Has One or More Symptoms (0 = No, 1 = Yes)",
       y = "Number of People",
       fill = "Diagnosis (0 = No, 1 = Yes)") +
  theme_minimal()







#Split the data with similar proportions of diagnosed and undiagnoses
index_70 <- createDataPartition(train_data$Diagnosis, p = 0.7, list = FALSE)
train_70 <- train_data[index_70,]
validation_set <- train_data[-index_70,]

# Check the dimension of the data to ensure proportion and variables are present
dim(train_70)
dim(validation_set)

library(e1071)

train_70$Diagnosis <- as.factor(train_70$Diagnosis)
# Convert 0/1 columns to factors
binary_to_factor <- function(df) {
  binary_cols <- sapply(df, function(col) all(col %in% c(0, 1)))
  df[binary_cols] <- lapply(df[binary_cols], factor)
  return(df)
}

train_70 <- binary_to_factor(train_70)
validation_set <- binary_to_factor(validation_set)


# Remove the 'PAtientID' column, irrelevant to our problem
train_70 <- select(train_70, -PatientID)
validation_set <- select(validation_set, -PatientID)

selected_features <- c("MMSE", "Ethnicity","MemoryComplaints", "ADL", 
                       "FunctionalAssessment", "CholesterolTotal", "EducationLevel", 
                       "Gender", "BehavioralProblems", "Confusion", "Disorientation",
                       "HeadInjury", "Forgetfulness", "DifficultyCompletingTasks",
                       "PersonalityChanges")

# Subset to selected features + target
train_subset <- train_70[, c(selected_features, "Diagnosis")]
test_subset <- validation_set[, c(selected_features, "Diagnosis")]

set.seed(123)
svm_model <- svm(Diagnosis ~ ., 
                 data = train_subset, 
                 kernel = "radial",  # or "radial"
                 cost = 10,  
                 gamma = 0.1, # you can tune this
                 scale = TRUE,
                 class.weights = c("0" = 1, "1" = 1.5))       # useful if your features are on different scales
predictions <- predict(svm_model, newdata = test_subset[, selected_features])

confusionMatrix(predictions, as.factor(test_subset$Diagnosis))

tune_result <- tune(svm,
                    Diagnosis ~ .,
                    data = train_subset,
                    kernel = "radial",
                    ranges = list(cost = c(1, 10, 100),
                                  gamma = c(0.001, 0.01, 0.1)),
                    class.weights = c("0" = 1, "1" = 1.5))

summary(tune_result)
best_model <- tune_result$best.model

predictions <- predict(best_model, newdata = test_subset)

confusionMatrix(predictions, as.factor(test_subset$Diagnosis))

# Install if needed
# install.packages("pROC")

library(pROC)

# Get decision values (probabilities or distance from hyperplane)
svm_model_prob <- predict(svm_model, newdata = test_subset[, selected_features], decision.values = TRUE)

# Extract decision values (needed for ROC)
decision_values <- attributes(svm_model_prob)$decision.values

# Create ROC curve
roc_obj <- roc(test_subset$Diagnosis, as.numeric(decision_values))

# Plot ROC
plot(roc_obj, col = "blue", main = "ROC Curve for SVM Model")
auc(roc_obj)  # This will print the AUC value

plot(roc_obj, col = "blue", main = "ROC Curve for SVM Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal line

plot(roc_obj, col = "blue", main = paste("ROC Curve - AUC:", round(auc(roc_obj), 3)))
abline(a = 0, b = 1, lty = 2, col = "gray")


set.seed(123)

model <- glm(Diagnosis ~ HasMedicalCondition + HasSymptoms,
             data = train_70,
             family = binomial())

# View the summary 
summary(model)

model_2 <- glm(Diagnosis ~ HasCognitiveIssues + HasSymptoms,
               data = train_70,
               family = binomial())

# View the summary 
summary(model_2)

model_3 <- glm(Diagnosis ~ HasMedicalCondition + HasCognitiveIssues + HasSymptoms,
               data = train_70,
               family = binomial())

# View the summary 
summary(model_3)


model_4 <- glm(Diagnosis ~ HasCognitiveIssues, 
               data = train_70,
               family = binomial())

# View the summary 
summary(model_4)


model_6 <- glm(Diagnosis ~ HasCognitiveIssues + as.factor(Ethnicity), 
               data = train_70, 
               family = binomial())

summary(model_6)

# Make predictions on the test set
predictions <- predict(model_2, validation_set, type = "response")

# Convert probabilities to binary outcomes (0 or 1)
predicted_class <- ifelse(predictions > 0.35, 1, 0)

# Check the performance with confusion matrix
confusionMatrix(as.factor(predicted_class), as.factor(validation_set$Diagnosis))


