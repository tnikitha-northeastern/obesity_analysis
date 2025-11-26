df_2023 <- raw %>%
  filter(class == "Obesity / Weight Status",
         str_detect(question, regex("have obesity", ignore_case = TRUE)),
         data_value_type == "Value",
         !str_detect(age_years, regex("UNKNOWN", ignore_case = TRUE)),
         year_start == 2023) %>%
  select(year = year_start,
         state = location_desc,
         state_abbr = location_abbr,
         value = data_value,
         age = age_years,
         sex = sex,
  )

state_year_2023 <- df_2023 %>%
  group_by(year, state, state_abbr) %>%
  summarize(obesity_pct = mean(value, na.rm = TRUE), .groups = "drop")

state_year_2023 <- state_year_2023 %>%
  mutate(region = case_when(
    state %in% northeast ~ "Northeast",
    state %in% midwest   ~ "Midwest",
    state %in% south     ~ "South",
    state %in% west      ~ "West",
    TRUE                 ~ NA_character_
  ))

state_order_2023 <- state_year_2023 %>%
  filter(year == 2023) %>%
  arrange(desc(obesity_pct)) %>%
  pull(state)

regional_year_2023 <- state_year_2023 %>%
  group_by(year, region) %>%
  summarize(regional_pct = mean(obesity_pct, na.rm = TRUE), .groups = "drop")

region_summary_2023 <- regional_year %>%
  group_by(region) %>%
  filter(year == 2023) %>%
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

region_summary_2023