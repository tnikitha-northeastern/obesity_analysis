# ---- Age-adjustment checker for BRFSS-style CSVs ----
# Usage: set the path to your file and run.
# It prints a verdict plus example rows that look age-adjusted (if any).

# install.packages(c("readr","dplyr","stringr")) # uncomment if needed
library(readr)
library(dplyr)
library(stringr)

# 1) Load data
path <- "/users/nikithathota/documents/New_Nutrition_Physical_Activity_Obesity_Clean.csv"  # adjust if needed
df <- suppressMessages(read_csv(path, show_col_types = FALSE))

# 2) Columns we’ll scan for “age-adjusted” signals (use what exists)
text_cols <- intersect(
  c("Question","Data_Value_Type","DataValueTypeID","Measure","Topic",
    "Response","Class","Datasource","StratificationCategory1","Stratification1"),
  names(df)
)

# 3) Heuristics / rules
#    - Positive if any text col contains "age adjusted" (with/without hyphen)
#    - Positive if Data_Value_Type contains phrases like "Age-adjusted prevalence"
#    - Negative evidence if you have explicit age strata (e.g., "Age(years)") with many groups
pat_ageadj <- regex("age\\s*[-]?\\s*adjust", ignore_case = TRUE)

has_age_adjustment_keyword <- any(df %>%
                                    select(all_of(text_cols)) %>%
                                    mutate(.row_text = do.call(paste, c(., sep = " | "))) %>%
                                    pull(.row_text) %>%
                                    str_detect(pat_ageadj),
                                  na.rm = TRUE
)

# Data_Value_Type quick look
dv_type_vals <- if ("Data_Value_Type" %in% names(df)) unique(df$Data_Value_Type) else character(0)
has_ageadjusted_in_type <- any(str_detect(dv_type_vals, pat_ageadj))

# Check for explicit age strata (suggests crude/stratified rather than age-adjusted totals)
has_age_strata_col <- "Age(years)" %in% names(df)
n_age_levels <- if (has_age_strata_col) n_distinct(df[["Age(years)"]], na.rm = TRUE) else 0

# 4) Rows that *look* age-adjusted (for user inspection)
suspect_rows <- df %>%
  mutate(.row_id = row_number()) %>%
  mutate(.flag_age_adj = if (length(text_cols)) {
    do.call(paste, c(select(., all_of(text_cols)), sep = " | ")) %>% str_detect(pat_ageadj)
  } else FALSE) %>%
  filter(.flag_age_adj) %>%
  select(.row_id, any_of(c("LocationAbbr","LocationDesc","YearStart","YearEnd")),
         any_of(text_cols)) %>%
  head(10)

# 5) Verdict logic
#    If we see explicit "age-adjusted" signals in text or type → likely age-adjusted present.
#    If not, and we see many age strata → likely not age-adjusted (crude/stratified).
verdict <- dplyr::case_when(
  has_ageadjusted_in_type ~ "LIKELY AGE-ADJUSTED (based on Data_Value_Type).",
  has_age_adjustment_keyword ~ "LIKELY AGE-ADJUSTED (keyword found in metadata).",
  !has_age_adjustment_keyword && has_age_strata_col && n_age_levels > 3 ~
    "LIKELY NOT AGE-ADJUSTED (appears stratified by age groups).",
  TRUE ~ "INCONCLUSIVE from metadata—no explicit 'age-adjusted' signal found."
)

# 6) Print summary
cat("\n--- AGE-ADJUSTMENT CHECK ---\n")
cat("Columns scanned:", paste(text_cols, collapse = ", "), "\n")
cat("Unique Data_Value_Type values:", ifelse(length(dv_type_vals)==0, "(none)", 
                                             paste(dv_type_vals, collapse = " | ")), "\n")
if (has_age_strata_col) cat("Age strata levels detected in 'Age(years)':", n_age_levels, "\n")
cat("Keyword 'age-adjusted' found in metadata? ", has_age_adjustment_keyword, "\n")
cat("Data_Value_Type mentions age-adjustment?  ", has_ageadjusted_in_type, "\n")
cat("\nVERDICT:", verdict, "\n")

if (nrow(suspect_rows) > 0) {
  cat("\nExample rows that look age-adjusted (showing up to 10):\n")
  print(suspect_rows)
} else {
  cat("\nNo rows explicitly mentioning age-adjustment were found in the scanned fields.\n")
}

# 7) Optional: show a quick tally to see if dataset mixes adjusted & crude
if ("Data_Value_Type" %in% names(df)) {
  cat("\nCounts by Data_Value_Type:\n")
  print(df %>% count(Data_Value_Type, sort = TRUE))
}

# 8) Optional: sanity check—look for phrases that usually indicate crude totals
if ("Question" %in% names(df)) {
  crude_hint <- df %>%
    filter(str_detect(Question, regex("percent of adults aged 18 years and older", ignore_case = TRUE))) %>%
    slice_head(n = 5) %>%
    select(YearStart, LocationAbbr, Question)
  if (nrow(crude_hint) > 0) {
    cat("\nSample 'crude-sounding' Questions (up to 5):\n")
    print(crude_hint)
  }
}
