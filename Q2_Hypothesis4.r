# Load libraries
library(tidyverse)
library(factoextra)
library(cluster)
library(janitor)

# Hypothesis 4
## H₀: There are no natural groupings in the population based on sleep and lifestyle factors.
## H₁: Distinct clusters exist that correspond to different health/lifestyle profiles.


path <- "/users/nikithathota/documents/Sleep_health_and_lifestyle_dataset_cleaned.csv"  # adjust if needed
raw <- readr::read_csv(path) %>% clean_names()

# Select only relevant lifestyle variables
X <- raw %>%
  select(sleep_duration, stress_level, physical_activity_level, quality_of_sleep) %>%
  drop_na()

# Scale data (important for K-means)
X_scaled <- scale(X)

# Choose number of clusters (you can try 2–6 and compare)
set.seed(123)
kmeans_result <- kmeans(X_scaled, centers = 4, nstart = 25)

# Visualize clusters
fviz_cluster(list(data = X_scaled, cluster = kmeans_result$cluster),
             ggtheme = theme_minimal(),
             main = "K-Means Clustering of Sleep and Lifestyle Factors")

# Add cluster assignment back to data
raw$Cluster <- kmeans_result$cluster

library(ggplot2)

raw$bmi_category <- as.factor(raw$bmi_category)
raw$bmi_category_num <- as.numeric(raw$bmi_category)

plot_data <- raw %>% drop_na(bmi_category_num, Cluster)

# Double-check
summary(plot_data$bmi_category_num)
table(plot_data$Cluster)

cluster_summary <- plot_data %>%
  group_by(Cluster) %>%
  summarise(
    avg_sleep = mean(sleep_duration, na.rm = TRUE),
    avg_stress = mean(stress_level, na.rm = TRUE),
    avg_activity = mean(physical_activity_level, na.rm = TRUE),
    avg_quality = mean(quality_of_sleep, na.rm = TRUE),
    avg_BMI = mean(bmi_category_num, na.rm = TRUE)
  )

print(cluster_summary)

anova_result <- aov(bmi_category_num ~ as.factor(Cluster), data = plot_data)
summary(anova_result)

# Conclusion for Your Hypothesis 4
# p-value (<2e-16) < 0.05 → Reject H₀
# Interpretation: There are statistically significant differences in BMI (or your chosen variable) across the four clusters.

bmi_summary <- plot_data %>%
  group_by(Cluster) %>%
  summarise(
    avg_bmi = mean(bmi_category_num, na.rm = TRUE),
    sd_bmi = sd(bmi_category_num, na.rm = TRUE),
    n = n(),
    se_bmi = sd_bmi / sqrt(n)
  )

ggplot(bmi_summary, aes(x = as.factor(Cluster), y = avg_bmi, fill = as.factor(Cluster))) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = avg_bmi - se_bmi, ymax = avg_bmi + se_bmi), width = 0.2) +
  geom_text(aes(label = round(avg_bmi, 2)), vjust = -0.6, size = 4) +
  labs(
    title = "Average BMI Category by Lifestyle Cluster",
    x = "Cluster", y = "Mean BMI Category (1=Normal, 2=Overweight, 3=Obese)",
    fill = "Cluster"
  ) +
  ylim(1, 3) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))



