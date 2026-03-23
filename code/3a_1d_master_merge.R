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
#                         year (treated) or last application year (control)
#          `merged_n`   — applicant and alumni counts by year
#          output/n_merged_by_year.csv
# =============================================================================

dir.create(here("output", "counts"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# JOIN APPLICANTS TO ALUMNI
# =============================================================================
# Non-matching applicants (never treated) get 0 across all treated_* columns.

merged_df <- applicants |>
  left_join(alum, by = c("first_name", "last_name")) |>
  mutate(
    across(
      starts_with("treated_20"),
      ~ ifelse(is.na(.), 0L, as.integer(.))
    ),
    treated_ever = ifelse(is.na(treated_ever), 0L, treated_ever),
    treated_before_2017 = ifelse(
      is.na(treated_before_2017),
      0L,
      treated_before_2017
    )
  )

# Flag whether the student was treated in their application year
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

# Identify each treated student's first program year
first_treatment <- merged_df |>
  filter(treated_in_year == 1) |>
  group_by(first_name, last_name) |>
  summarise(
    first_treatment_year = min(year[treated_in_year == 1], na.rm = TRUE),
    total_times_treated = sum(treated_in_year == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    first_treatment_year = if_else(
      is.infinite(first_treatment_year),
      NA_real_,
      first_treatment_year
    )
  )

merged_df <- merged_df |>
  left_join(first_treatment, by = c("first_name", "last_name")) |>
  mutate(
    first_treatment_year = ifelse(
      is.na(first_treatment_year),
      0,
      first_treatment_year
    ),
    total_times_treated = ifelse(
      is.na(total_times_treated),
      0L,
      total_times_treated
    )
  )

# =============================================================================
# DROP 2020 (COVID)
# =============================================================================
# Program disruption in 2020 invalidates treatment/control comparability.

pre_year_filter <- nrow(merged_df)

merged_df <- merged_df |>
  filter(year != 2020)

message(
  "Removed ",
  pre_year_filter - nrow(merged_df),
  " observations (2020 COVID year)"
)

# =============================================================================
# DEDUPLICATE TO ONE ROW PER STUDENT
# =============================================================================
# Treated students: keep the row for their FIRST treatment year.
# Control students: keep the row for their MOST RECENT application year.
#
# NOTE — timing asymmetry: treated covariates are measured at first treatment
# (earlier on average); control covariates at last application (later on
# average). The propensity score model exact-matches on year, which partially
# addresses this. Verify covariate balance by year in script 5
# (bal.tab cluster = "year").

pre_duplicate_filter <- nrow(merged_df)

merged_df_clean <- merged_df |>
  group_by(first_name, last_name) |>
  filter(
    (treated_ever == 1 & year == first_treatment_year) |
      (treated_ever == 0 & year == max(year))
  ) |>
  slice(1) |>
  ungroup()

message(
  "Removed ",
  pre_duplicate_filter - nrow(merged_df_clean),
  " duplicate applications — ",
  nrow(merged_df_clean),
  " students remaining"
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
