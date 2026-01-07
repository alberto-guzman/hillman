# =============================================================================
# PROPENSITY SCORE MATCHING: PA STUDENTS (2017-2023)
# TWO APPROACHES: Year-only AND Year+School matching
# =============================================================================
# Purpose: Generate two matched datasets for comparison
# Approach 1: Year-only matching (better external validity, larger sample)
# Approach 2: Year+School matching (stronger causal ID, within-school comparison)
# Output: Two matched datasets for outcome analysis
# =============================================================================

library(dplyr)
library(MatchIt)
library(cobalt)
library(here)

# =============================================================================
# 0. FILTER OUT 2020
# =============================================================================

# Remove 2020 cohort due to COVID disruptions
merged_df_pa_covars <- merged_df_pa_covars |>
  filter(year != 2020)

message("Removed 2020 cohort")
message("Remaining years: 2017-2019, 2021-2023")
message("Total observations: ", nrow(merged_df_pa_covars))

# =============================================================================
# 1. PRE-MATCHING DIAGNOSTICS
# =============================================================================

# Check treatment/control distribution by year and school
school_summary <- merged_df_pa_covars %>%
  filter(!is.na(aun), !is.na(treated_in_year)) %>%
  group_by(aun, year) %>%
  summarise(
    n_treated = sum(treated_in_year == 1),
    n_control = sum(treated_in_year == 0),
    total = n(),
    .groups = "drop"
  )

# Identify schools with only treated or only control students
problem_schools <- school_summary %>%
  filter(n_treated == 0 | n_control == 0) %>%
  arrange(desc(total))

message("Schools with only treated or only control: ", nrow(problem_schools))

# =============================================================================
# 2. PREPARE DATA FOR MATCHING
# =============================================================================

# Define covariates for matching (excluding higher_ed_enrollment_2yr)
cols_to_fill <- c(
  "gender",
  "grade",
  "gpa",
  "psat_math",
  "stipend",
  "house_size",
  "racially_marginalized",
  "bi_multi_racial",
  "urban",
  "suburban",
  "rural",
  "disability",
  "neg_school",
  "us_citizen",
  "first_gen"
)

# Remove contamination: ever-treated students can only appear as treated
merged_df_pa_covars <- merged_df_pa_covars |>
  filter(
    treated_ever == 0 |
      (treated_ever == 1 & treated_in_year == 1)
  )

message(
  "Removed applications where ever-treated students appeared in non-treatment years"
)

# Create missing indicators and fill NAs with 0
merged_df_pa_covars <- merged_df_pa_covars %>%
  mutate(across(
    all_of(cols_to_fill),
    list(miss = ~ if_else(is.na(.), 1, 0)),
    .names = "{col}_miss"
  )) %>%
  mutate(across(all_of(cols_to_fill), ~ replace_na(., 0)))

# Remove rows with missing treatment or school identifier
merged_df_pa_covars <- merged_df_pa_covars %>%
  filter(!is.na(treated_in_year), !is.na(aun))

# =============================================================================
# 3A. PROPENSITY SCORE MATCHING: YEAR-ONLY (PRIMARY ANALYSIS)
# =============================================================================

message("\n=== MATCHING APPROACH 1: YEAR-ONLY ===\n")

m.out_year <- matchit(
  treated_in_year ~
    gender +
    grade +
    gpa +
    psat_math +
    stipend +
    house_size +
    racially_marginalized +
    bi_multi_racial +
    urban +
    suburban +
    rural +
    disability +
    neg_school +
    us_citizen +
    first_gen +
    gpa_miss +
    psat_math_miss +
    stipend_miss +
    house_size_miss +
    racially_marginalized_miss +
    disability_miss +
    neg_school_miss +
    us_citizen_miss +
    first_gen_miss,
  data = merged_df_pa_covars,
  method = "nearest",
  exact = ~year, # Match within year ONLY
  distance = "glm",
  caliper = 1,
  replace = TRUE
)

# Summary and balance
summary(m.out_year)
bal.tab(m.out_year, un = TRUE, thresholds = c(m = 0.1))

# Extract matched data
matched_data_year <- match.data(m.out_year)

# Filter to schools with both treated and control
matched_data_year_filtered <- matched_data_year %>%
  group_by(aun) %>%
  filter(
    sum(treated_in_year == 1, na.rm = TRUE) > 0 &
      sum(treated_in_year == 0, na.rm = TRUE) > 0
  ) %>%
  ungroup()

message("\n--- Year-Only Matching Results ---")
message(
  "Matched treated students: ",
  sum(matched_data_year_filtered$treated_in_year == 1)
)
message(
  "Matched control students: ",
  sum(matched_data_year_filtered$treated_in_year == 0)
)

# =============================================================================
# 3B. PROPENSITY SCORE MATCHING: YEAR + SCHOOL (ROBUSTNESS CHECK)
# =============================================================================

message("\n=== MATCHING APPROACH 2: YEAR + SCHOOL ===\n")

m.out_year_school <- matchit(
  treated_in_year ~
    gender +
    grade +
    gpa +
    psat_math +
    stipend +
    house_size +
    racially_marginalized +
    bi_multi_racial +
    urban +
    suburban +
    rural +
    disability +
    neg_school +
    us_citizen +
    first_gen +
    gpa_miss +
    psat_math_miss +
    stipend_miss +
    house_size_miss +
    racially_marginalized_miss +
    disability_miss +
    neg_school_miss +
    us_citizen_miss +
    first_gen_miss,
  data = merged_df_pa_covars,
  method = "nearest",
  exact = ~ year + aun, # Match within YEAR AND SCHOOL
  distance = "glm",
  caliper = 1,
  replace = TRUE # With replacement for within-school
)

# Summary and balance
summary(m.out_year_school)
bal.tab(m.out_year_school, un = TRUE, thresholds = c(m = 0.1))

# Extract matched data
matched_data_year_school <- match.data(m.out_year_school)

# Filter to schools with both treated and control (already guaranteed by exact matching)
matched_data_year_school_filtered <- matched_data_year_school %>%
  group_by(aun) %>%
  filter(
    sum(treated_in_year == 1, na.rm = TRUE) > 0 &
      sum(treated_in_year == 0, na.rm = TRUE) > 0
  ) %>%
  ungroup()

message("\n--- Year + School Matching Results ---")
message(
  "Matched treated students: ",
  sum(matched_data_year_school_filtered$treated_in_year == 1)
)
message(
  "Matched control students: ",
  sum(matched_data_year_school_filtered$treated_in_year == 0)
)

# =============================================================================
# 4. COMPARE THE TWO APPROACHES
# =============================================================================

message("\n=== COMPARISON OF MATCHING APPROACHES ===\n")

# Create comparison summary
comparison_approaches <- tibble(
  Approach = c("Year-Only", "Year + School"),
  Treated = c(
    sum(matched_data_year_filtered$treated_in_year == 1),
    sum(matched_data_year_school_filtered$treated_in_year == 1)
  ),
  Control = c(
    sum(matched_data_year_filtered$treated_in_year == 0),
    sum(matched_data_year_school_filtered$treated_in_year == 0)
  ),
  Total = Treated + Control,
  Retention_Pct = round(
    100 * Treated / nrow(filter(merged_df_pa_covars, treated_in_year == 1)),
    1
  )
)

print(comparison_approaches)

# =============================================================================
# 5. SAVE BOTH MATCHED DATASETS
# =============================================================================

# Save Year-Only matched data (PRIMARY ANALYSIS)
saveRDS(
  matched_data_year_filtered,
  here("data", "matched_pa_students_year_only.rds")
)

write_csv(
  matched_data_year_filtered,
  here("data", "matched_pa_students_year_only.csv")
)

saveRDS(
  m.out_year,
  here("data", "matchit_object_year_only.rds")
)

message("\n=== Year-Only Matched Data Saved ===")
message("File: data/matched_pa_students_year_only.rds")
message("Total observations: ", nrow(matched_data_year_filtered))

# Save Year + School matched data (ROBUSTNESS CHECK)
saveRDS(
  matched_data_year_school_filtered,
  here("data", "matched_pa_students_year_school.rds")
)

write_csv(
  matched_data_year_school_filtered,
  here("data", "matched_pa_students_year_school.csv")
)

saveRDS(
  m.out_year_school,
  here("data", "matchit_object_year_school.rds")
)

message("\n=== Year + School Matched Data Saved ===")
message("File: data/matched_pa_students_year_school.rds")
message("Total observations: ", nrow(matched_data_year_school_filtered))

# =============================================================================
# 6. FINAL ASSESSMENT FOR BOTH APPROACHES
# =============================================================================

# Function to create assessment
create_assessment <- function(matched_data, approach_name) {
  cat("\n==============================================\n")
  cat("ASSESSMENT:", approach_name, "\n")
  cat("==============================================\n\n")

  pre_match_treated <- merged_df_pa_covars |> filter(treated_in_year == 1)
  matched_treated <- matched_data |> filter(treated_in_year == 1)

  cat("Pre-matched treated students: ", nrow(pre_match_treated), "\n")
  cat("Matched treated students:     ", nrow(matched_treated), "\n")
  cat(
    "Retention rate:               ",
    round(100 * nrow(matched_treated) / nrow(pre_match_treated), 1),
    "%\n\n"
  )

  # Characteristics
  pre_stats <- pre_match_treated |>
    summarise(
      pct_stipend = mean(stipend == 1, na.rm = TRUE) * 100,
      pct_racially_marg = mean(racially_marginalized == 1, na.rm = TRUE) * 100,
      mean_gpa = mean(gpa, na.rm = TRUE),
      mean_psat = mean(psat_math, na.rm = TRUE)
    )

  matched_stats <- matched_treated |>
    summarise(
      pct_stipend = mean(stipend == 1, na.rm = TRUE) * 100,
      pct_racially_marg = mean(racially_marginalized == 1, na.rm = TRUE) * 100,
      mean_gpa = mean(gpa, na.rm = TRUE),
      mean_psat = mean(psat_math, na.rm = TRUE)
    )

  cat("Characteristics (Pre-matched → Matched):\n")
  cat(
    "  Stipend need:        ",
    round(pre_stats$pct_stipend, 1),
    "% → ",
    round(matched_stats$pct_stipend, 1),
    "%\n"
  )
  cat(
    "  Racially marginalized:",
    round(pre_stats$pct_racially_marg, 1),
    "% → ",
    round(matched_stats$pct_racially_marg, 1),
    "%\n"
  )
  cat(
    "  Mean GPA:            ",
    round(pre_stats$mean_gpa, 2),
    " → ",
    round(matched_stats$mean_gpa, 2),
    "\n"
  )
  cat(
    "  Mean PSAT:           ",
    round(pre_stats$mean_psat, 0),
    " → ",
    round(matched_stats$mean_psat, 0),
    "\n\n"
  )
}

# Create assessments for both approaches
create_assessment(matched_data_year_filtered, "YEAR-ONLY MATCHING")
create_assessment(matched_data_year_school_filtered, "YEAR + SCHOOL MATCHING")

# =============================================================================
# END OF SCRIPT
# =============================================================================
