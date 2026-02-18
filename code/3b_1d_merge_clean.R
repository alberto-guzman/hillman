# =============================================================================
# Standardized variables
# =============================================================================

merged_clean <- merged_df |>
  mutate(
    # --- GPA: convert any remaining 0s to NA (already standardized in applicants)
    gpa = na_if(gpa, 0),

    # --- PSAT Math: convert any remaining 0s to NA (already cleaned)
    psat_math = na_if(psat_math, 0),

    # --- Stipend: yes/no → 1/0
    stipend = case_when(
      str_detect(
        stipend,
        regex("\\b(yes|stipend eligible|yes student)\\b", ignore_case = TRUE)
      ) ~
        1L,
      TRUE ~ 0L
    ),

    # --- Household size: already cleaned, but double-check range
    house_size = suppressWarnings(as.integer(house_size)),
    house_size = if_else(between(house_size, 1L, 11L), house_size, NA_integer_),

    # --- First-gen: yes/no → 1/0
    first_gen = case_when(
      str_detect(tolower(as.character(first_gen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(first_gen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Fix 2023 reverse-coded first-gen
    first_gen = if_else(
      year == 2023 & !is.na(first_gen),
      1L - first_gen, # Flip 0→1 and 1→0
      first_gen
    ),

    # --- Race / identity flags -----------------------------------------------
    racially_marginalized = case_when(
      str_detect(
        tolower(self_identity),
        "black|african|latino|latinx|hispanic|native|indian|pacific"
      ) ~
        1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),
    bi_multi_racial = case_when(
      str_detect(tolower(self_identity), "multi|bi-?racial|two or more") ~ 1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),

    # --- Geography flags ------------------------------------------------------
    urban = if_else(
      str_detect(tolower(geographic_location), "urban"),
      1L,
      0L,
      missing = NA_integer_
    ),
    suburban = if_else(
      str_detect(tolower(geographic_location), "suburban"),
      1L,
      0L,
      missing = NA_integer_
    ),
    rural = if_else(
      str_detect(tolower(geographic_location), "rural"),
      1L,
      0L,
      missing = NA_integer_
    ),

    # --- Disability: yes/no → 1/0
    disability = case_when(
      str_detect(
        documented_disability,
        regex("^\\s*no\\b", ignore_case = TRUE)
      ) ~
        0L,
      !is.na(documented_disability) ~ 1L,
      TRUE ~ NA_integer_
    ),

    # --- Negative school environment flag
    neg_school = case_when(
      str_detect(school_impact, regex("^\\s*no\\b", ignore_case = TRUE)) ~ 0L,
      !is.na(school_impact) ~ 1L,
      TRUE ~ NA_integer_
    ),

    # --- Citizenship: yes/no → 1/0
    us_citizen = case_when(
      str_detect(tolower(as.character(american_citizen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(american_citizen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# =============================================================================
# MERGE NSC OUTCOME DATA
# =============================================================================

library(haven)

# Load NSC outcome data
outcomes <- read_dta(here(
  "data/files_for_danielle_nsc",
  "clean_Hillman_2017_2025.dta"
)) |>
  janitor::clean_names() |>
  rename(
    first_name = firstname,
    last_name = lastname
  ) |>
  select(
    first_name,
    last_name,
    hs_grad_year,
    seamless_enroll,
    seamless_enroll_stem,
    enrolled_ever_nsc,
    enrolled_ever_stem,
    degree_ever_nsc,
    degree_ever_stem_nsc,
    degree_6years_all_nsc,
    bachdegree_6years_all_nsc,
    reten_fall_enter,
    reten_fall_enter_stem,
    reten_fall_enter2,
    reten_fall_enter_stem2,
    pers_fall_enter,
    pers_fall_enter_stem,
    pers_fall_enter3,
    degree_in6_grad,
    ste_mdegree_in6_grad,
    public4yr_initial,
    x4yr_initial,
    firsttime_fulltime
  )

message("Loaded NSC outcome data: ", nrow(outcomes), " records")

# Check for duplicates BEFORE removing them
outcomes_dupes <- outcomes |>
  group_by(first_name, last_name, hs_grad_year) |>
  filter(n() > 1) |>
  ungroup()

if (nrow(outcomes_dupes) > 0) {
  message(
    "Found ",
    nrow(outcomes_dupes),
    " duplicate outcome records (",
    round(100 * nrow(outcomes_dupes) / nrow(outcomes), 1),
    "%)"
  )

  # Investigate duplicates - are they truly identical or different?
  dupes_summary <- outcomes_dupes |>
    group_by(first_name, last_name, hs_grad_year) |>
    summarise(
      n = n(),
      seamless_enroll_same = length(unique(seamless_enroll)) == 1,
      .groups = "drop"
    )

  message(
    "  Duplicates with identical outcomes: ",
    sum(dupes_summary$seamless_enroll_same)
  )
  message(
    "  Duplicates with different outcomes: ",
    sum(!dupes_summary$seamless_enroll_same)
  )

  # Remove duplicates - keep first occurrence
  outcomes <- outcomes |>
    group_by(first_name, last_name, hs_grad_year) |>
    slice(1) |>
    ungroup()

  message("After de-duplication: ", nrow(outcomes), " unique outcome records")
} else {
  message("No duplicate outcome records found")
}

# Merge outcomes with cleaned data
message("\n=== Merging NSC outcomes with applicant data ===\n")

merged_clean <- merged_clean |>
  left_join(
    outcomes,
    by = c("first_name", "last_name", "hs_grad_year")
  )

# Check merge statistics
merge_stats <- merged_clean |>
  summarise(
    n_total = n(),
    n_matched = sum(!is.na(seamless_enroll)),
    pct_matched = round(100 * n_matched / n_total, 1),
    n_unmatched = sum(is.na(seamless_enroll)),
    pct_unmatched = round(100 * n_unmatched / n_total, 1)
  )

message("Merge statistics:")
message("  Total applicants: ", merge_stats$n_total)
message(
  "  Matched to NSC: ",
  merge_stats$n_matched,
  " (",
  merge_stats$pct_matched,
  "%)"
)
message(
  "  Unmatched: ",
  merge_stats$n_unmatched,
  " (",
  merge_stats$pct_unmatched,
  "%)"
)

# Check match rates by treatment status
match_by_treatment <- merged_clean |>
  group_by(treated_in_year) |>
  summarise(
    n = n(),
    n_matched = sum(!is.na(seamless_enroll)),
    pct_matched = round(100 * n_matched / n, 1)
  )

message("\nMatch rates by treatment status:")
print(match_by_treatment)

# Code NSC non-matches as 0 for ENROLLMENT outcomes only
# (other outcomes are conditional on enrollment and should remain NA)
merged_clean <- merged_clean |>
  mutate(across(
    c(
      seamless_enroll,
      seamless_enroll_stem,
      enrolled_ever_nsc,
      enrolled_ever_stem
    ),
    ~ if_else(is.na(.), 0, .)
  ))

# Filter to keep cohorts 2018-2022 with valid data
message("\n=== Filtering to graduation cohorts 2018-2022 ===\n")

pre_filter_n <- nrow(merged_clean)

# Calculate removal statistics BEFORE filtering
removal_stats <- merged_clean |>
  summarise(
    total = n(),
    missing_grad_year = sum(is.na(hs_grad_year)),
    missing_grade = sum(is.na(grade)),
    invalid_grade = sum(!is.na(grade) & (grade < 9 | grade > 12)),
    outside_range = sum(
      !is.na(hs_grad_year) &
        !is.na(grade) &
        grade >= 9 &
        grade <= 12 &
        (hs_grad_year < 2018 | hs_grad_year > 2022)
    )
  )

# Apply filter
merged_clean <- merged_clean |>
  filter(
    !is.na(hs_grad_year),
    !is.na(grade),
    grade >= 9,
    grade <= 12,
    hs_grad_year >= 2018,
    hs_grad_year <= 2022
  )

post_filter_n <- nrow(merged_clean)

message("Removed ", pre_filter_n - post_filter_n, " observations:")
message("  Missing hs_grad_year: ", removal_stats$missing_grad_year)
message("  Missing grade: ", removal_stats$missing_grade)
message("  Invalid grade (not 9-12): ", removal_stats$invalid_grade)
message("  hs_grad_year outside 2018-2022: ", removal_stats$outside_range)
message("Remaining: ", post_filter_n, " observations")

# Select final variables (including outcomes)
merged_clean <- merged_clean |>
  select(
    first_name,
    last_name,
    year,
    hs_grad_year,
    treated_ever,
    treated_in_year,
    total_times_treated,
    treated_before_2017,
    first_treatment_year,
    high_school,
    state,
    gender,
    grade,
    gpa,
    psat_math,
    stipend,
    house_size,
    racially_marginalized,
    bi_multi_racial,
    urban,
    suburban,
    rural,
    disability,
    neg_school,
    us_citizen,
    first_gen,
    # NSC outcomes - enrollment
    seamless_enroll,
    seamless_enroll_stem,
    enrolled_ever_nsc,
    enrolled_ever_stem,
    # NSC outcomes - degrees (conditional on enrollment)
    degree_ever_nsc,
    degree_ever_stem_nsc,
    degree_6years_all_nsc,
    bachdegree_6years_all_nsc,
    # Additional NSC outcomes (conditional on enrollment)
    reten_fall_enter,
    reten_fall_enter_stem,
    reten_fall_enter2,
    reten_fall_enter_stem2,
    pers_fall_enter,
    pers_fall_enter_stem,
    pers_fall_enter3,
    degree_in6_grad,
    ste_mdegree_in6_grad,
    public4yr_initial,
    x4yr_initial,
    firsttime_fulltime
  )

# Final summary
message("\n=== Cleaning Complete ===")
message("Dataset includes ", nrow(merged_clean), " students")
message("  Treated: ", sum(merged_clean$treated_in_year == 1))
message("  Control: ", sum(merged_clean$treated_in_year == 0))
message(
  "  Graduation cohorts: ",
  paste(sort(unique(merged_clean$hs_grad_year)), collapse = ", ")
)

# Summary by application year
message("\n=== Sample by Application Year ===")
merged_clean |>
  group_by(year, treated_in_year) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(
    names_from = treated_in_year,
    values_from = n,
    names_prefix = "treated_",
    values_fill = 0
  ) |>
  mutate(
    total = treated_0 + treated_1,
    pct_treated = round(100 * treated_1 / total, 1)
  ) |>
  print()

# Summary by graduation cohort
message("\n=== Sample by Graduation Cohort ===")
merged_clean |>
  group_by(hs_grad_year, treated_in_year) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(
    names_from = treated_in_year,
    values_from = n,
    names_prefix = "treated_",
    values_fill = 0
  ) |>
  mutate(
    total = treated_0 + treated_1,
    pct_treated = round(100 * treated_1 / total, 1)
  ) |>
  print()

rm(
  list = setdiff(
    ls(),
    c("alum", "applicants", "merged_df", "merged_clean", "outcomes")
  )
)
