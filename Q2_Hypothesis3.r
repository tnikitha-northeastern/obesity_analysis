library(neuralnet)
library(caret)
library(dplyr)

path <- "/users/nikithathota/documents/Sleep_health_and_lifestyle_dataset_cleaned.csv"  # adjust if needed
raw <- readr::read_csv(path) %>% clean_names()

## HYPOTHESES 3 
##H₀: Simpler models (logistic regression) perform equally well as complex models (random forest, neural network).
## H₁: Complex ML models outperform traditional models for BMI prediction.

# Logistic regression
glm_model <- multinom(bmi_category ~ . - person_id, data = train)

# Compare accuracy
# 0.9733333
mean(predict(glm_model, test) == test$bmi_category)
# 0.8933333
mean(predict(rf_model, test) == test$bmi_category)
# 0.9866667
mean(predict(rf_cv, test) == test$bmi_category)



# Create dummy output columns
df <- raw %>%
  mutate(
    Class_1 = ifelse(bmi_category == "Normal", 1, 0),
    Class_2 = ifelse(bmi_category == "Overweight", 1, 0),
    Class_3 = ifelse(bmi_category == "Obese", 1, 0)
  )

# Normalize predictors
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
df_norm <- df %>%
  mutate(
    across(where(is.numeric), normalize)
  )

# Train-test split
set.seed(123)
index <- sample(1:nrow(df_norm), 0.7 * nrow(df_norm))
train <- df_norm[index, ]
test  <- df_norm[-index, ]
train <- train %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))
test <- test %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Build formula (multi-output)
predictors <- names(train)[!names(train) %in% c("bmi_category","Class_1","Class_2","Class_3", "person_id")]
formula <- as.formula(paste("Class_1 + Class_2 + Class_3 ~", paste(predictors, collapse = " + ")))

# Train network
nn_model <- neuralnet(
  formula,
  data = train,
  hidden = c(8,4),
  linear.output = FALSE,
  stepmax = 1e6
)
plot(nn_model)

nn_pred <- compute(nn_model, test[, predictors])$net.result

# Convert probabilities to predicted class index
pred_class <- apply(nn_pred, 1, which.max)

# Get true class index from one-hot encoded columns
true_class <- apply(test[, c("Class_1","Class_2","Class_3")], 1, which.max)

# Evaluate performance
nn_conf <- confusionMatrix(as.factor(pred_class), as.factor(true_class))
print(nn_conf)