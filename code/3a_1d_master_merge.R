# =============================================================================
# 3a_1d_master_merge.R
#
# Purpose: Join applicants to alumni treatment indicators, construct the
#          per-student treatment timeline, remove COVID year (2020), and
#          reduce to one observation per student.
#
# Input:   `applicants` — cleaned applicant data (from script 1)
#          `alum`       — wide treatment indicators (from script 2)
# Output:  `merged_df`  — one row per student, one record per first-treatment
#                         year (treated) or last application year (control).
#                         Application years span 2017-2019, 2021-2023 (2020
#                         excluded for COVID); script 3b further restricts the
#                         analytic sample to HS-grad cohorts 2018-2021.
#          `merged_n`   — applicant and alumni counts by application year
#          output/counts/n_merged_by_year.csv
# =============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(here)

dir.create(here("output", "counts"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# JOIN APPLICANTS TO ALUMNI
# =============================================================================
# Non-matching applicants (never treated) get 0 across all treated_* columns.

# Only bring in treatment flags from alum — all other alum columns either
# conflict with applicant columns (e.g. gender) or are not needed downstream.
# first_treatment_year / total_times_treated are recalculated below from
# applicant records so they reflect application-year timing.

# Guardrail: alum must be unique on (first_name, last_name) for the join to
# behave as 1:m. If two distinct students collapse to the same cleaned-name
# pair (or script 2's structure changes), the join would silently duplicate
# applicant rows.
stopifnot(!any(duplicated(alum[, c("first_name", "last_name")])))

merged_df <- applicants |>
  left_join(
    alum |> select(
      first_name, last_name,
      treated_ever, treated_before_2017,
      starts_with("treated_20")
    ),
    by = c("first_name", "last_name"),
    relationship = "many-to-one"
  ) |>
  mutate(
    across(
      starts_with("treated_20"),
      ~ if_else(is.na(.), 0L, as.integer(.))
    ),
    treated_ever       = if_else(is.na(treated_ever), 0L, treated_ever),
    treated_before_2017 = if_else(is.na(treated_before_2017), 0L, treated_before_2017)
  )

# Flag whether the student was treated in their application year.
# The case_when below enumerates 2017-2023; if a future cohort year appears,
# the script must be updated alongside script 2's pivot range. This guardrail
# raises a clear error rather than silently coding such students as untreated.
unhandled_years <- setdiff(unique(merged_df$year), 2017:2023)
if (length(unhandled_years) > 0) {
  stop(
    "Unhandled application years (extend the case_when below + script 2's pivot range): ",
    paste(sort(unhandled_years), collapse = ", ")
  )
}

merged_df <- merged_df |>
  mutate(
    treated_in_year = case_when(
      year == 2017 ~ treated_2017,
      year == 2018 ~ treated_2018,
      year == 2019 ~ treated_2019,
      year == 2020 ~ treated_2020,
      year == 2021 ~ treated_2021,
      year == 2022 ~ treated_2022,
      year == 2023 ~ treated_2023,
      TRUE ~ 0L
    )
  )

# =============================================================================
# DROP 2020 (COVID)
# =============================================================================
# Program disruption in 2020 invalidates treatment/control comparability.
# Filter happens before first_treatment_year is computed so that timing
# reflects only in-sample (post-COVID) cohorts.

pre_year_filter <- nrow(merged_df)

merged_df <- merged_df |>
  filter(year != 2020)

message(
  "Removed ",
  pre_year_filter - nrow(merged_df),
  " observations (2020 COVID year)"
)

# =============================================================================
# IDENTIFY EACH TREATED STUDENT'S FIRST PROGRAM YEAR
# =============================================================================
# Recomputed from applicant records (post-COVID filter) so first_treatment_year
# reflects application-year timing for cohorts 2017–2019, 2021–2023.
# Students with no in-sample treatment year (pre-2017 alumni or 2020-only)
# get first_treatment_year = 0; they are dropped explicitly in the dedup
# step below.

first_treatment <- merged_df |>
  filter(treated_in_year == 1) |>
  group_by(first_name, last_name) |>
  summarise(
    first_treatment_year = min(year),
    total_times_treated  = n(),
    .groups = "drop"
  )

merged_df <- merged_df |>
  left_join(first_treatment, by = c("first_name", "last_name")) |>
  mutate(
    first_treatment_year = if_else(is.na(first_treatment_year), 0L, as.integer(first_treatment_year)),
    total_times_treated  = if_else(is.na(total_times_treated),  0L, total_times_treated)
  )

# =============================================================================
# DEDUPLICATE TO ONE ROW PER STUDENT
# =============================================================================
# Treated students: keep the row for their FIRST treatment year.
# Control students: keep the row for their MOST RECENT application year.
# Treated students with no in-sample treatment year (first_treatment_year = 0)
# are pre-2017 alumni or 2020-only treated; both are excluded from the analysis
# by design and dropped here with an explicit count.
#
# NOTE — timing asymmetry: treated covariates are measured at first treatment
# (earlier on average); control covariates at last application (later on
# average). The propensity score model exact-matches on year, which partially
# addresses this. Verify covariate balance by year in script 5
# (bal.tab cluster = "year").

pre_duplicate_filter <- nrow(merged_df)

n_orphan_treated <- merged_df |>
  filter(treated_ever == 1, first_treatment_year == 0) |>
  distinct(first_name, last_name) |>
  nrow()

merged_df_clean <- merged_df |>
  group_by(first_name, last_name) |>
  filter(
    (treated_ever == 1 & first_treatment_year > 0 & year == first_treatment_year) |
      (treated_ever == 0 & year == max(year))
  ) |>
  ungroup()

message(
  "Removed ",
  pre_duplicate_filter - nrow(merged_df_clean),
  " duplicate applications — ",
  nrow(merged_df_clean),
  " students remaining"
)
message(
  "Excluded ",
  n_orphan_treated,
  " treated students with no in-sample treatment year ",
  "(pre-2017 alumni or 2020-only treated)"
)

# Verify no duplicates remain
remaining_duplicates <- merged_df_clean |>
  group_by(first_name, last_name) |>
  filter(n() > 1) |>
  summarise(
    n_applications = n(),
    years_applied = paste(sort(unique(year)), collapse = ", "),
    treated_ever = first(treated_ever),
    .groups = "drop"
  )

if (nrow(remaining_duplicates) > 0) {
  warning(
    "Unexpected duplicates remain: ",
    nrow(remaining_duplicates),
    " students"
  )
  print(remaining_duplicates)
}

# =============================================================================
# FINAL COUNTS
# =============================================================================

merged_df <- merged_df_clean

merged_n <- merged_df |>
  count(year, name = "n_total") |>
  left_join(
    merged_df |>
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

write_csv(merged_n, here("output", "counts", "n_merged_by_year.csv"))

merged_n

rm(
  list = setdiff(
    ls(),
    c("alum", "alum_n", "applicants", "applicant_n", "merged_df", "merged_n")
  )
)
