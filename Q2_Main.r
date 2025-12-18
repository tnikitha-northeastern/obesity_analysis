library(tidyverse)
library(ggplot2)
library(reshape2)
library(corrplot)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(keras)
library(nnet)
library(janitor)

path <- "/users/nikithathota/documents/Sleep_health_and_lifestyle_dataset_cleaned.csv"  # adjust if needed
raw <- readr::read_csv(path) %>% clean_names()
view(raw)
raw$bmi_category <- as.factor(raw$bmi_category)


## Hypotheses

## HYPOTHESES 1
## H₀: Sleep duration, stress, and physical activity do not predict BMI category better than random chance.
## H₁: Sleep duration, stress, and physical activity can accurately predict BMI category.

set.seed(123)  # for reproducibility

# Split into 80% train, 20% test
index <- sample(1:nrow(raw), 0.8 * nrow(raw))
train <- raw[index, ]
test  <- raw[-index, ]

rf_model <- randomForest(bmi_category ~ sleep_duration + stress_level + physical_activity_level, data = train, importance = TRUE)
importance(rf_model)
# Normal    Obese Overweight MeanDecreaseAccuracy MeanDecreaseGini
# sleep_duration          45.32739 15.89507   40.84705             55.24957         59.11523
# stress_level            28.56004 12.34416   19.43614             33.08198         28.68318
# physical_activity_level 36.69123 16.36035   37.13131             45.65747         42.68341
print(rf_model)

predictions <- predict(rf_model, test)

# Evaluate accuracy
confusion <- table(test$bmi_category, predictions)
print(confusion)

accuracy <- mean(predictions == test$bmi_category)
# Test Accuracy: 89.33 %
cat("Test Accuracy:", round(accuracy * 100, 2), "%\n")

tree1 <- getTree(rf_model, k = 1, labelVar = TRUE)

# Show the first few rows of that tree
head(tree1)
tree_model <- rpart(bmi_category ~ sleep_duration + stress_level + physical_activity_level, data = train)
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Example Decision Tree (1 of 500 in Random Forest)")

control <- trainControl(method = "cv", number = 5)
rf_cv <- train(bmi_category ~ sleep_duration + stress_level + physical_activity_level,
               data = raw, method = "rf", trControl = control)
rf_cv
rf_cv$results        # shows accuracy for each mtry
rf_cv$bestTune       # best mtry value
rf_cv$finalModel     # actual randomForest object
plot(rf_cv)


control_all_features <- trainControl(method = "cv", number = 5)
rf_cv_all_features <- train(bmi_category ~ ., data = raw, method = "rf", trControl = control_all_features)
rf_cv_all_features
rf_cv_all_features$results        # shows accuracy for each mtry
rf_cv_all_features$bestTune       # best mtry value
rf_cv_all_features$finalModel     # actual randomForest object
plot(rf_cv_all_features)



# Random Forest 
# 
# 374 samples
# 3 predictor
# 3 classes: 'Normal', 'Obese', 'Overweight' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 300, 299, 300, 299, 298 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy  Kappa    
# 2     0.943817  0.8896015
# 3     0.943817  0.8896015

# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 2.


# HYPOTHESIS 2
## H₀: All features contribute equally to BMI prediction.
## H₁: Some features (like sleep duration or stress) have greater predictive importance than others.

rf_model_all_features = randomForest(bmi_category ~ . - person_id, data = train, importance= TRUE)
importance(rf_model_all_features)
predictions_all_features <- predict(rf_model_all_features, test)
accuracy <- mean(predictions == test$bmi_category)
cat("Test Accuracy:", round(accuracy * 100, 2), "%\n")

importance(rf_model_all_features)
varImpPlot(rf_model_all_features)

obesity_by_occ <- raw %>%
  group_by(occupation) %>%
  summarise(
    total = n(),
    obese = sum(bmi_category == "Obese", na.rm = TRUE),
    obesity_rate = 100 * obese / total
  ) %>%
  arrange(desc(obesity_rate))

ggplot(obesity_by_occ, aes(x = reorder(occupation, obesity_rate), y = obesity_rate, fill = obesity_rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "Obesity Rate by Occupation",
       x = "Occupation", y = "Obesity Rate (%)") +
  scale_fill_viridis_c(option = "C") +
  theme_minimal(base_size = 12)

chisq.test(table(raw$occupation, raw$bmi_category))
# 	Pearson's Chi-squared test
# data:  table(raw$occupation, raw$bmi_category)
# X-squared = 365.41, df = 20, p-value < 2.2e-16


## -----------------------------
## DESCRIPTIVE STATISTICS
## -----------------------------

library(tidyverse)
library(janitor)
library(psych)      # for describe()
library(ggplot2)
library(corrplot)

# Basic structure
cat("Number of Rows:", nrow(raw), "\n")
cat("Number of Columns:", ncol(raw), "\n")

str(raw)

# Missing values summary
cat("\n--- Missing Values per Column ---\n")
print(colSums(is.na(raw)))

# Summary for numeric variables only
numeric_vars <- raw %>% select(where(is.numeric), -person_id)
cat("\n--- Summary of Numeric Variables ---\n")
summary(numeric_vars)

# Detailed numeric statistics (mean, sd, skewness, etc.)
cat("\n--- Detailed Numeric Descriptives (psych::describe) ---\n")
describe(numeric_vars)

# Frequency tables for categorical variables
cat("\n--- Frequency Tables for Categorical Variables ---\n")
cat("BMI Category:\n")
print(table(raw$bmi_category))

if("gender" %in% colnames(raw)) {
  cat("\nGender:\n")
  print(table(raw$gender))
}

if("occupation" %in% colnames(raw)) {
  cat("\nOccupation:\n")
  print(table(raw$occupation))
}

if("sleep_duration" %in% colnames(raw)) {
  cat("\nSleep Duration Distribution:\n")
  print(summary(raw$sleep_duration))
}

if("stress_level" %in% colnames(raw)) {
  cat("\nStress Level Distribution:\n")
  print(summary(raw$stress_level))
}

if("physical_activity_level" %in% colnames(raw)) {
  cat("\nPhysical Activity Distribution:\n")
  print(summary(raw$physical_activity_level))
}

# Correlation Matrix (numeric variables only)
cat("\n--- Correlation Matrix ---\n")
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.cex = 0.7)

# Plot distributions for key predictors
ggplot(raw, aes(x = sleep_duration)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
  labs(title = "Distribution of Sleep Duration (hours)")

ggplot(raw, aes(x = stress_level)) +
  geom_bar(fill = "#ff8c00") +
  labs(title = "Distribution of Stress Levels")

ggplot(raw, aes(x = physical_activity_level)) +
  geom_bar(fill = "#4682b4") +
  labs(title = "Physical Activity Level Distribution")



library(dplyr)
library(psych)
library(corrplot)

