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
         year_start >= 2011, year_start <= 2023) %>%
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
  scale_fill_viridis_c(name = "Mean Obesity Prevalence (%)", option = "C") +
  labs(title = "Mean Obesity Prevalence by State and Year",
       x = "Year", y = NULL) +
  theme_minimal(base_size = 12)

## Regional Trends in adult obesity (2011-2023)
northeast <- c("Maine","Vermont","New Hampshire","Massachusetts","Connecticut","Rhode Island",
               "New York","New Jersey","Pennsylvania")
midwest   <- c("Ohio","Indiana","Illinois","Michigan","Wisconsin","Minnesota","Iowa","Missouri",
               "North Dakota","South Dakota","Nebraska","Kansas")
south     <- c("Delaware","Maryland","District of Columbia","Virginia","West Virginia","Kentucky",
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
anova_two_way <- aov(value ~ factor(year) * factor(age), data = df_subset)
summary(anova_two_way)
##Df Sum Sq Mean Sq
##factor(year)             12  340.7    28.4
##factor(age)               5 2823.2   564.6
##factor(year):factor(age) 60   25.3     0.4
  
## H₀ (Null): There is no statistically significant difference in the mean obesity prevalence among the four designated U.S. Census regions.
##H₁ (Alternative): At least one U.S. Census region exhibits a mean obesity prevalence that is statistically different from the others.
##  TWO WAY ANOVA TEST : 
anova_regional <- aov(regional_pct ~ factor(region) * factor(year), data = regional_year) summary(anova_regional)
summary(anova_regional)
# Df Sum Sq Mean Sq
  # factor(region)               3  231.3   77.11
  # factor(year)                12  198.9   16.57
  # factor(region):factor(year) 36    4.7    0.13
  
## H₀ (Null): There is no significant difference in mean obesity prevalence among the different age groups.
##H₁ (Alternative): There is a significant difference in mean obesity prevalence among at least two of the age groups.
anova_age <- aov(value ~ factor(age), data = df)
 summary(anova_age)
# Df Sum Sq Mean Sq F value Pr(>F)    
# factor(age)    5 153608   30722    1238 <2e-16 ***
#   Residuals   4274 106094      25                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

