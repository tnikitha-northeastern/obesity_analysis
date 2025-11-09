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

rf_model <- randomForest(bmi_category ~ sleep_duration + stress_level + physical_activity_level, data = train)
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

varImpPlot(rf_model)


set.seed(123)
control <- trainControl(method = "cv", number = 5)
rf_cv <- train(bmi_category ~ sleep_duration + stress_level + physical_activity_level,
               data = raw, method = "rf", trControl = control)
rf_cv

# HYPOTHESIS 2
## H₀: All features contribute equally to BMI prediction.
## H₁: Some features (like sleep duration or stress) have greater predictive importance than others.

rf_model_all_features = randomForest(bmi_category ~ ., data = train)
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
# data:  table(raw$occupation, raw$bmi_category)
# X-squared = 365.41, df = 20, p-value < 2.2e-16


# Logistic regression
glm_model <- multinom(bmi_category ~ ., data = train)

# Compare accuracy
# 0.9733333
mean(predict(glm_model, test) == test$bmi_category)
# 0.8933333
mean(predict(rf_model, test) == test$bmi_category)






