# install.packages(c("readr","dplyr","stringr","tidyr"))  # if needed
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# -------------------------------
# 1) Load your data
# -------------------------------

df <- suppressMessages(read_csv(path, show_col_types = FALSE))

# Inspect likely columns (uncomment to peek)
# names(df)
# head(df)

# -------------------------------
# 2) Identify age-strata column
#    We prefer explicit Age(years); otherwise use Stratification* when it's "Age (years)"
# -------------------------------
has_age_col <- "Age(years)" %in% names(df)

if (!has_age_col && all(c("StratificationCategory1","Stratification1") %in% names(df))) {
  df <- df %>%
    mutate(`Age(years)` = ifelse(
      str_detect(`StratificationCategory1`, regex("^Age", ignore_case = TRUE)),
      `Stratification1`, NA_character_
    ))
  has_age_col <- TRUE
}

if (!has_age_col) {
  stop("No age-strata column found. Look for 'Age(years)' or Stratification fields labeled 'Age (years)'.")
}

# Ensure core numeric columns exist / are named consistently
# BRFSS-style files often use Data_Value for the percent/prevalence
stopifnot("Data_Value" %in% names(df))

# -------------------------------
# 3) Tidy the age labels so they match a standard weight table
#    This helper maps lots of common forms to normalized bins (e.g., '18-24', '25 - 34', '18–24', etc.)
#    Adjust patterns if your age labels differ.
# -------------------------------
normalize_age_label <- function(x) {
  x <- str_squish(str_replace_all(x, "[–—to]", "-"))  # normalize dashes "18–24" → "18-24"
  x <- str_replace_all(x, "\\s", "")                  # remove spaces "25 - 34" → "25-34"
  x <- str_to_lower(x)
  x <- case_when(
    x %in% c("18-24","18to24","18–24") ~ "18-24",
    x %in% c("25-34","25to34","25–34") ~ "25-34",
    x %in% c("35-44","35to44","35–44") ~ "35-44",
    x %in% c("45-54","45to54","45–54") ~ "45-54",
    x %in% c("55-64","55to64","55–64") ~ "55-64",
    x %in% c("65-74","65to74","65–74") ~ "65-74",
    x %in% c("75-99","75+","75andolder","75andover","75orolder") ~ "75+",
    x %in% c("65+","65andover","65orolder") ~ "65+",
    x %in% c("18-44","18to44","18–44") ~ "18-44",
    x %in% c("45-64","45to64","45–64") ~ "45-64",
    TRUE ~ x
  )
  x
}

df <- df %>%
  mutate(age_norm = normalize_age_label(`Age(years)`))

# -------------------------------
# 4) Provide/choose standard population weights
#    IMPORTANT: Your weights must match your age_norm bins.
#    Two common presets are given. Pick ONE that matches your bins or replace with your own.
# -------------------------------

# (A) 7-bin example often used when data have 10-year groups + 75+
std_weights_7 <- tribble(
  ~age_norm, ~weight,
  "18-24", 0.162,  # <-- replace with your official standard if you have it
  "25-34", 0.214,
  "35-44", 0.216,
  "45-54", 0.179,
  "55-64", 0.132,
  "65-74", 0.069,
  "75+",   0.028
)

# (B) 3-bin example sometimes used in BRFSS reporting
# std_weights_3 <- tribble(
#   ~age_norm, ~weight,
#   "18-44", 0.487,  # <-- replace with your official standard if you have it
#   "45-64", 0.342,
#   "65+",   0.171
# )

# >>>>> CHOOSE one of these (or define your own) to match your actual age bins:
std_weights <- std_weights_7  # or: std_weights <- std_weights_3

# Normalize to sum to 1 (safety)
std_weights <- std_weights %>%
  group_by() %>%
  mutate(weight = weight / sum(weight))

# -------------------------------
# 5) Keep only rows with an age bin present in the chosen standard
#    (and warn if we’re missing any)
# -------------------------------
age_bins_in_data <- sort(unique(df$age_norm))
age_bins_in_std  <- sort(unique(std_weights$age_norm))

missing_in_std <- setdiff(age_bins_in_data, age_bins_in_std)
if (length(missing_in_std) > 0) {
  warning("These age bins exist in your data but have NO standard weight: ",
          paste(missing_in_std, collapse = ", "),
          "\nThey will be dropped from the age-adjusted calculation.")
}

df_use <- df %>%
  semi_join(std_weights, by = c("age_norm"))

# -------------------------------
# 6) (Optional) limit to the indicator(s) you want to adjust
#    e.g., pick a specific Question or Topic
# -------------------------------
# Example filter (uncomment & customize):
# df_use <- df_use %>% filter(str_detect(Question, "Percent of adults aged 18 years and older"))

as_num <- function(x) suppressWarnings(as.numeric(x))

has_n <- "Sample_Size" %in% names(df_use)

dfw <- df_use %>%
  mutate(
    p = as_num(Data_Value) / 100,
    n = if (has_n) as_num(Sample_Size) else NA_real_
  ) %>%
  left_join(std_weights, by = "age_norm")
# -------------------------------
# 7) Compute age-adjusted prevalence by Location & Year (and any other grouping you keep)
#    Assumes Data_Value is a PERCENT (0–100). We convert to proportion.
# -------------------------------
# Helper: robust numeric conversion
group_keys <- intersect(
  c("YearStart","LocationAbbr","LocationDesc","Class","Topic","Question","Data_Value_Unit"),
  names(dfw)
)

# as_num <- function(x) suppressWarnings(as.numeric(x))
# 
# df_use <- df_use %>%
#   mutate(p = as_num(Data_Value) / 100)
# 
# # Choose your grouping keys (keep ones you need)
# group_keys <- intersect(
#   c("YearStart","LocationAbbr","LocationDesc","Class","Topic","Question","Data_Value_Unit"),
#   names(df_use)
# )
# 
# # Join in weights
# dfw <- df_use %>%
#   left_join(std_weights, by = "age_norm")
# 
# # -------------------------------
# # 8) Variance / CI handling:
# #    Path A: If 'DataValueStdErr' exists (as %), convert to proportion SE.
# #    Path B: Else, if Sample_Size exists, use binomial variance p*(1-p)/n.
# #    Path C: Else, we’ll return just the age-adjusted estimate (no CI).
# # -------------------------------
# has_se_col <- "DataValueStdErr" %in% names(dfw)
# has_n_col  <- "Sample_Size"     %in% names(dfw)
# 
# # dfw <- dfw %>%
# #   mutate(
# #     se_p = case_when(
# #       has_se_col ~ as_num(DataValueStdErr)/100,
# #       has_n_col  ~ sqrt( p * (1 - p) / as_num(Sample_Size) ),
# #       TRUE       ~ NA_real_
# #     )
# #   )

# -------------------------------
# 9) Direct age standardization:
#    p_adj = sum(w_i * p_i) / sum(w_i)
#    Var(p_adj) ≈ sum( w_i^2 * Var(p_i) ) / (sum(w_i))^2, assuming independence
# -------------------------------
adj <- dfw %>%
  group_by(across(all_of(group_keys))) %>%
  summarise(
    n_age_groups = n_distinct(age_norm),
    # point estimate
    p_adj = sum(weight * p, na.rm = TRUE) / sum(weight[!is.na(p)], na.rm = TRUE),
    # variance via binomial if n available; otherwise NA
    var_adj = if (has_n) {
      # per-age var_i ≈ p_i*(1-p_i)/n_i ; combine as sum(w^2 * var_i) / (sum w)^2
      num <- sum( (weight^2) * (p * (1 - p) / n), na.rm = TRUE )
      den <- (sum(weight[!is.na(p) & !is.na(n)], na.rm = TRUE))^2
      ifelse(den > 0, num / den, NA_real_)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    se_adj   = ifelse(is.na(var_adj), NA_real_, sqrt(var_adj)),
    p_adj_pct = 100 * p_adj,
    lcl95_pct = ifelse(is.na(se_adj), NA_real_, 100 * (p_adj - 1.96 * se_adj)),
    ucl95_pct = ifelse(is.na(se_adj), NA_real_, 100 * (p_adj + 1.96 * se_adj))
  ) %>%
  arrange(YearStart, LocationAbbr)

# -------------------------------
# 10) Results
# -------------------------------
print(head(adj, 12))

adj_simple <- adj %>%
  select(YearStart, LocationAbbr, LocationDesc,
         p_adj_pct, lcl95_pct, ucl95_pct, n_age_groups)

readr::write_csv(adj_simple, "age_adjusted_prevalence.csv")  # uncomment to save

# Quick message if no CI
if (!has_n) message("No 'Sample_Size' column found. Returned point estimates only (no CI).")