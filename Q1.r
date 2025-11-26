library(tidyverse)
library(ggplot2)
library(reshape2)
library(corrplot)
library(janitor)
library(scales)
library(dplyr)

path <- "/users/nikithathota/documents/New_Nutrition_Physical_Activity_Obesity_Clean.csv"  # adjust if needed
raw <- readr::read_csv(path) %>% clean_names()

# ---- Filter to adult obesity (% of adults who have obesity), keep 2011–2023 ----
df <- raw %>%
  filter(class == "Obesity / Weight Status",
         str_detect(question, regex("have obesity", ignore_case = TRUE)),
         data_value_type == "Value",
         !str_detect(age_years, regex("UNKNOWN", ignore_case = TRUE)),
         year_start >= 2011, year_start <= 2023, !location_desc %in% c("District of Columbia", "National")) %>%
  select(year = year_start,
         state = location_desc,
         state_abbr = location_abbr,
         value = data_value,
         age = age_years,
         sex = sex,
  )

# Prevalence Estimation Over the Years by Age Group
df_by_age_year <- df %>%
  group_by(age, year) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(age, year)

ggplot(df_by_age_year, aes(x = year, y = value, color = age, group = age)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.1) +
  labs(title = "Mean Obesity prevalance Over the Years by Age Group",
       x = "Year", y = "Mean Obesity Prevalence (%)", color = "Age Group") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))



# Prevalence Estimates by Age Group
ggplot(df, aes(x = age, y = value, fill = age)) +
  geom_boxplot() +
  labs(title = "Obesity prevalence by Age Group",
       x = "Age Group",
       y = "obesity Prevalence") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")


# Order states by latest (2023) level to make the heatmap more readable
state_year <- df %>%
  group_by(year, state, state_abbr) %>%
  summarize(obesity_pct = mean(value, na.rm = TRUE), .groups = "drop")

state_order <- state_year %>%
  filter(year == 2023) %>%
  arrange(desc(obesity_pct)) %>%
  pull(state)

# Heat map over years.
ggplot(state_year %>% mutate(state = factor(state, levels = state_order)),
       aes(x = year, y = state, fill = obesity_pct)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Mean Obesity Prevalence (%)", option = "C", direction = -1) +
  labs(title = "Mean Obesity Prevalence by State and Year",
       x = "Year", y = NULL) +
  theme_minimal(base_size = 12)

## Regional Trends in adult obesity (2011-2023)
northeast <- c("Maine","Vermont","New Hampshire","Massachusetts","Connecticut","Rhode Island",
               "New York","New Jersey","Pennsylvania")
midwest   <- c("Ohio","Indiana","Illinois","Michigan","Wisconsin","Minnesota","Iowa","Missouri",
               "North Dakota","South Dakota","Nebraska","Kansas")
south     <- c("Delaware","Maryland","Virginia","West Virginia","Kentucky",
               "North Carolina","South Carolina","Georgia","Florida","Alabama","Mississippi",
               "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
west      <- c("Montana","Idaho","Wyoming","Colorado","New Mexico","Arizona","Utah","Nevada",
               "Washington","Oregon","California","Alaska","Hawaii")

state_year <- state_year %>%
  mutate(region = case_when(
    state %in% northeast ~ "Northeast",
    state %in% midwest   ~ "Midwest",
    state %in% south     ~ "South",
    state %in% west      ~ "West",
    TRUE                 ~ NA_character_
  ))

state_year <- state_year %>% filter(!is.na(region))

regional_year <- state_year %>%
  group_by(year, region) %>%
  summarize(regional_pct = mean(obesity_pct, na.rm = TRUE), .groups = "drop")

ggplot(regional_year, aes(year, regional_pct, color = region)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 1.6) +
  labs(title = "Regional Trends in Adult Obesity (2011–2023)",
       x = "Year", y = "Mean obesity prevalence (%)", color = "Region") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  theme_minimal(base_size = 12)


# --- Top 10 highest-prevalence states in 2023 ---
low10_2023 <- state_year %>%
  filter(year == 2023) %>%
  slice_min(obesity_pct, n = 10) %>%
  arrange(desc(obesity_pct)) %>%                           # order for a nice ascending bar chart
  mutate(state = factor(state, levels = state))      # keep this order on the plot

top10_2023 <- state_year %>%
  filter(year == 2023) %>%
  slice_max(obesity_pct, n = 10) %>%
  arrange(obesity_pct) %>%                           # order for a nice ascending bar chart
  mutate(state = factor(state, levels = state))      # keep this order on the plot

ggplot(top10_2023, aes(x = state, y = obesity_pct, fill = obesity_pct)) +
  geom_col() +
  # geom_vline(xintercept = nat_avg_2023, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_fill_gradient(
    name = "Obesity Rate (%)",
    low = "#A7C7E7",    # light blue for lower values
    high = "#08306B"    # dark blue for higher values
  ) +
  labs(title = "10 Most Obese States in 2023",
       x = "US State",
       y = "Obesity Rate (%)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )


ggplot(low10_2023, aes(x = state, y = obesity_pct, fill = obesity_pct)) +
  geom_col() +
  # geom_vline(xintercept = nat_avg_2023, linetype = "dashed", color = "red") +
  coord_flip() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_fill_gradient(
    name = "Obesity Rate (%)",
    low = "#A7C7E7",    # light blue for lower values
    high = "#08306B"    # dark blue for higher values
  ) +
  labs(title = "10 Least Obese States in 2023",
       x = "US State",
       y = "Obesity Rate (%)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )


#### Hypotheses
##H₀ (Null): There is no statistically significant change in the mean obesity prevalence between 2011 and 2023.
##H₁ (Alternative): The mean obesity prevalence in 2023 is significantly different from the mean prevalence in 2011.

##TWO WAY ANOVA TEST : 
df_subset <- df_by_age_year %>% filter(year %in% c(2011, 2023))
anova_two_way <- aov(value ~ factor(year) * factor(age), data = df_subset)
summary(anova_two_way)
# Df Sum Sq Mean Sq
# factor(year)              1  109.6  109.58
# factor(age)               5  426.2   85.24
# factor(year):factor(age)  5    2.4    0.48

# Without DC and National
# Df Sum Sq Mean Sq 
# factor(year)              1  113.6  113.60
# factor(age)               5  425.8   85.17
# factor(year):factor(age)  5    2.9    0.57

#### Hypotheses
##H₀ (Null): There is a statistically significant change in mean obesity prevalence over time (2011–2023).
##H₁ (Alternative):  There is no statistically significant change in mean obesity prevalence over time (2011–2023).
anova_two_way <- aov(value ~ factor(year) * factor(age), data = df_by_age_year)
summary(anova_two_way)
# Df Sum Sq Mean Sq
# factor(year)             12  344.4    28.7
# factor(age)               5 2861.6   572.3
# factor(year):factor(age) 60   24.3     0.4

# Without DC and National
# Df Sum Sq Mean Sq
# factor(year)             12  352.3    29.4
# factor(age)               5 2872.8   574.6
# factor(year):factor(age) 60   25.6     0.4
  
## H₀ (Null): There is no statistically significant difference in the mean obesity prevalence among the four designated U.S. Census regions.
##H₁ (Alternative): At least one U.S. Census region exhibits a mean obesity prevalence that is statistically different from the others.
##  TWO WAY ANOVA TEST : 
anova_regional <- aov(regional_pct ~ factor(region) * factor(year), data = regional_year) 
summary(anova_regional)
# Df Sum Sq Mean Sq
# factor(region)               3  337.7  112.55
# factor(year)                12  238.2   19.85
# factor(region):factor(year) 36    6.6    0.18

# Without DC and National
# Df Sum Sq Mean Sq
# factor(region)               3  382.3  127.42
# factor(year)                12  242.9   20.24
# factor(region):factor(year) 36    6.8    0.19
  
## H₀ (Null): There is no significant difference in mean obesity prevalence among the different age groups.
##H₁ (Alternative): There is a significant difference in mean obesity prevalence among at least two of the age groups.
anova_age <- aov(value ~ factor(age), data = df)
summary(anova_age)
# Df Sum Sq Mean Sq F value Pr(>F)    
# factor(age)    5 157386   31477    1258 <2e-16 ***
#   Residuals   4284 107152      25                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Without DC and National
# Df Sum Sq Mean Sq F value Pr(>F)    
# factor(age)    5 152257   30451    1234 <2e-16 ***
#   Residuals   4128 101891      25                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova_region <- aov(regional_pct ~ factor(region), data = regional_year )
summary(anova_region)
# Df Sum Sq Mean Sq F value Pr(>F)    
# factor(region)  3  337.7   112.5   22.07  4e-09 ***
#   Residuals      48  244.8     5.1                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Without DC and National
# Df Sum Sq Mean Sq F value   Pr(>F)    
# factor(region)  3  382.3   127.4    24.5 9.27e-10 ***
#   Residuals      48  249.7     5.2                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova_year<- aov(value ~ factor(year), data = df)
summary(anova_year)
# Df Sum Sq Mean Sq F value Pr(>F)    
# factor(year)   12  18943  1578.6   27.49 <2e-16 ***
#   Residuals    4277 245594    57.4                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Without DC and National
# Df Sum Sq Mean Sq F value Pr(>F)    
# factor(year)   12  18673  1556.1   27.23 <2e-16 ***
#   Residuals    4121 235475    57.1                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ------------------------------------------
# DESCRIPTIVE STATISTICS FOR OBESITY DATA
# ------------------------------------------

library(dplyr)
library(psych)   # for describe()
library(janitor)

# ---- BASIC COUNTS ----
cat("Total Rows:", nrow(df), "\n") #4134
cat("Total States:", n_distinct(df$state), "\n") #53
cat("Years Covered:", min(df$year), "to", max(df$year), "\n") # 2011 to 2023
cat("Age Groups:", paste(unique(df$age), collapse = ", "), "\n") 
# Age Groups: 18 - 24, 25 - 34, 35 - 44, 45 - 54, 55 - 64, 65 or older


# ---- OVERALL SUMMARY FOR OBESITY VALUES ----
overall_summary <- df %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    n = n()
  )

overall_summary
# A tibble: 1 × 7
# mean median    sd   min   max   IQR     n
# <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <int>
#   1  30.6   31.4  7.84   3.8  59.7   9.8  4134

# ---- USING PSYCH PACKAGE FOR MORE DETAILED SUMMARY ----
describe(df$value)
# vars    n  mean   sd median trimmed  mad min  max range  skew kurtosis   se
# X1    1 4134 30.59 7.84   31.4   30.92 7.41 3.8 59.7  55.9 -0.33    -0.11 0.12

# ---- SUMMARY BY AGE GROUP ----
age_summary <- df %>%
  group_by(age) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

age_summary %>% arrange(desc(mean))


# ---- SUMMARY BY YEAR ----
year_summary <- df %>%
  group_by(year) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

year_summary
# year  mean median    sd   min   max   IQR     n
# <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <int>
#   1  2011  27.5   28.3  6.90   9.3  41    8.68   318
# 2  2012  27.9   28.8  7.10   9    43.4  7.87   318
# 3  2013  28.4   29.4  7.27  10    46.2  8.67   318
# 4  2014  28.9   29.8  7.30  10.6  43.9  9.15   318
# 5  2015  29.1   29.8  7.37  10.2  44.7  9.27   318
# 6  2016  29.6   30.2  7.50   8.9  47.5  9.38   318
# 7  2017  30.5   31.4  7.76   9.5  47.1  9.5    318
# 8  2018  31.1   31.7  7.72  10.2  48.2  9.75   318
# 9  2019  31.9   32.4  7.39  12    50.2  9.68   318
# 10  2020  32.0   32.4  7.75  10.9  49.2  9.95   318
# 11  2021  33.5   33.8  8.14  12.8  59.7 11.2    318
# 12  2022  33.7   34.3  8.07   3.8  57.6 11.6    318
# 13  2023  33.6   33.7  7.88  13    49.5 10.9    318

# ---- SUMMARY BY REGION (using your regional_year dataset) ----
region_summary <- regional_year %>%
  group_by(region) %>%
  summarise(
    mean = mean(regional_pct, na.rm = TRUE),
    median = median(regional_pct, na.rm = TRUE),
    sd = sd(regional_pct, na.rm = TRUE),
    min = min(regional_pct, na.rm = TRUE),
    max = max(regional_pct, na.rm = TRUE),
    IQR = IQR(regional_pct, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

region_summary
# A tibble: 4 × 8
# region     mean median    sd   min   max   IQR     n
# <chr>     <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <int>
#   1 Midwest    32.2   32.2  2.62  28.4  35.8  3.56    13
# 2 Northeast  27.4   27.4  1.85  24.8  29.9  3.49    13
# 3 South      33.5   33.3  2.41  29.8  36.8  3.81    13
# 4 West       27.6   27.2  2.17  24.6  30.9  2.62    13


unique(df$state)

view(raw)
