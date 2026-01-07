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
# 0. FILTER OUT
# =============================================================================

# Remove 2020 cohort due to COVID disruptions
merged_df_pa_covars <- merged_df_pa_covars |>
  filter(year != 2020)

# Keep only grades 10-11 (primary program grades)
merged_df_pa_covars <- merged_df_pa_covars |>
  filter(grade %in% c(10, 11))

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
  exact = ~year,
  distance = "glm",
  caliper = 1,
  replace = TRUE
)

# Summary and balance
summary(m.out_year)
bal.tab(m.out_year, un = TRUE, thresholds = c(m = 0.2))

# Year-only matching balance by year
bal.tab(
  m.out_year,
  cluster = "year",
  un = TRUE,
  thresholds = c(m = 0.2)
)

# Extract matched data
matched_data_year <- match.data(m.out_year)

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
bal.tab(m.out_year_school, un = TRUE, thresholds = c(m = 0.2))

# Year-only matching balance by year
bal.tab(
  m.out_year_school,
  cluster = "year",
  un = TRUE,
  thresholds = c(m = 0.2)
)

# Extract matched data
matched_data_year_school <- match.data(m.out_year_school)


# =============================================================================
# 5. SAVE BOTH MATCHED DATASETS
# =============================================================================

# Save Year-Only matched data (PRIMARY ANALYSIS)
saveRDS(
  matched_data_year,
  here("data", "matched_pa_students_year_only.rds")
)

saveRDS(
  matched_data_year_school,
  here("data", "matched_pa_students_year_school")
)

# =============================================================================
# 7. CLEAN UP WORKSPACE
# =============================================================================

rm(
  list = setdiff(
    ls(),
    c(
      "matched_data_year",
      "matched_data_year_school",
      "m.out_year",
      "m.out_year_school"
    )
  )
)

# =============================================================================
# END OF SCRIPT
# =============================================================================
