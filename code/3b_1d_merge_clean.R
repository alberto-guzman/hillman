# =============================================================================
# 3b_1d_merge_clean.R
#
# Purpose: Standardize covariates in the merged applicant/alumni dataset,
#          merge NSC college outcome data, and restrict to the analytic sample
#          (graduation cohorts 2018–2022, NSC-matched students only).
#
# Input:   `merged_df` — one row per student (from script 3a)
#          data/files_for_danielle_nsc/clean_Hillman_2017_2025.dta — NSC data
# Output:  `merged_clean`   — analysis-ready dataset restricted to students
#                             with an NSC record, standardized covariates,
#                             and NSC outcomes attached
#          `merged_clean_n` — applicant and alumni counts by year
#          output/n_merged_clean_by_year.csv
# =============================================================================

# =============================================================================
# STANDARDIZE COVARIATES
# =============================================================================
merged_clean <- merged_df |>
  mutate(
    gpa = na_if(gpa, 0),
    psat_math = na_if(psat_math, 0),

    stipend = case_when(
      str_detect(
        stipend,
        regex("\\b(yes|stipend eligible|yes student)\\b", ignore_case = TRUE)
      ) ~ 1L,
      TRUE ~ 0L
    ),

    house_size = suppressWarnings(as.integer(house_size)),
    house_size = if_else(between(house_size, 1L, 11L), house_size, NA_integer_),

    first_gen = case_when(
      str_detect(tolower(as.character(first_gen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(first_gen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # 2023 first_gen question was reverse-coded in the source data ("did a
    # parent attend college?" vs. "will you be the first?") — flip it here.
    first_gen = if_else(
      year == 2023 & !is.na(first_gen),
      1L - first_gen,
      first_gen
    ),

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

    # Any non-"no" non-missing response is coded 1 (presence of disability/
    # negative school environment). Verify raw values with count() if recoding.
    disability = case_when(
      str_detect(
        documented_disability,
        regex("^\\s*no\\b", ignore_case = TRUE)
      ) ~ 0L,
      !is.na(documented_disability) ~ 1L,
      TRUE ~ NA_integer_
    ),
    neg_school = case_when(
      str_detect(school_impact, regex("^\\s*no\\b", ignore_case = TRUE)) ~ 0L,
      !is.na(school_impact) ~ 1L,
      TRUE ~ NA_integer_
    ),

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

# De-duplicate NSC records: keep the row with the most non-missing outcomes;
# break ties by preferring seamless_enroll = 1.
outcomes_dupes <- outcomes |>
  group_by(first_name, last_name, hs_grad_year) |>
  filter(n() > 1) |>
  ungroup()

if (nrow(outcomes_dupes) > 0) {
  message(
    "Found ",
    nrow(outcomes_dupes),
    " duplicate NSC records — deduplicating"
  )
  outcomes <- outcomes |>
    group_by(first_name, last_name, hs_grad_year) |>
    arrange(
      desc(rowSums(!is.na(across(everything())))),
      desc(seamless_enroll),
      .by_group = TRUE
    ) |>
    slice(1) |>
    ungroup()
  message("After de-duplication: ", nrow(outcomes), " unique outcome records")
} else {
  message("No duplicate NSC records found")
}

merged_clean <- merged_clean |>
  left_join(outcomes, by = c("first_name", "last_name", "hs_grad_year"))

# Check match rates by treatment status before applying filter
match_by_treatment <- merged_clean |>
  group_by(treated_in_year) |>
  summarise(
    n = n(),
    n_matched = sum(!is.na(seamless_enroll)),
    pct_matched = round(100 * n_matched / n, 1),
    .groups = "drop"
  )

message("NSC match rates by treatment status:")
print(match_by_treatment)

# Drop students with no NSC record rather than coding enrollment as 0.
# Absence from NSC is not equivalent to non-enrollment; coding it 0 would
# bias enrollment estimates if non-match rates differ by treatment status.
pre_nsc_filter_n <- nrow(merged_clean)

merged_clean <- merged_clean |>
  filter(!is.na(seamless_enroll))

message(
  "NSC filter: ",
  nrow(merged_clean),
  " retained, ",
  pre_nsc_filter_n - nrow(merged_clean),
  " dropped"
)
# =============================================================================
# FILTER TO ANALYTIC SAMPLE (graduation cohorts 2018–2022)
# =============================================================================
# Earlier cohorts have incomplete NSC follow-up; later cohorts are too recent
# for 6-year degree outcomes.

pre_filter_n <- nrow(merged_clean)

removal_stats <- merged_clean |>
  summarise(
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

message(
  "Cohort filter removed ",
  pre_filter_n - nrow(merged_clean),
  " observations:"
)
message("  Missing hs_grad_year: ", removal_stats$missing_grad_year)
message("  Missing grade: ", removal_stats$missing_grade)
message("  Invalid grade: ", removal_stats$invalid_grade)
message("  Outside 2018-2022: ", removal_stats$outside_range)
message("Remaining: ", nrow(merged_clean))

# =============================================================================
# SELECT FINAL VARIABLE SET
# =============================================================================

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

# =============================================================================
# FINAL COUNTS
# =============================================================================

merged_clean_n <- merged_clean |>
  count(year, name = "n_total") |>
  left_join(
    merged_clean |>
      filter(treated_in_year == 1) |>
      count(year, name = "n_alumni"),
    by = "year"
  ) |>
  mutate(
    n_alumni = replace_na(n_alumni, 0L),
    n_applicants = n_total - n_alumni,
    pct_alumni = round(n_alumni / n_total * 100, 1)
  ) |>
  select(year, n_applicants, n_alumni, n_total, pct_alumni)

write_csv(merged_clean_n, here("output", "n_merged_clean_by_year.csv"))

merged_clean_n

rm(
  list = setdiff(
    ls(),
    c(
      "merged_clean",
      "merged_clean_n",
      "merged_df",
      "merged_n",
      "alum",
      "alum_n",
      "applicants",
      "applicant_n"
    )
  )
)
