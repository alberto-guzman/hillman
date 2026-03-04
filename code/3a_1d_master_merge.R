# =============================================================================
# 3a_1d_master_merge.R
#
# Purpose: Join applicants to alumni treatment indicators, construct the
#          per-student treatment timeline, remove COVID year (2020), and
#          reduce to one observation per student.
#
# Input:   `applicants` — cleaned applicant data (from script 1)
#          `alum`       — wide treatment indicators (from script 2)
# Output:  `merged_df`  — one row per student, one record per
#                         first-treatment year (treated) or last application
#                         year (control)
# =============================================================================

# --- Join applicants to alumni and fill treatment NAs with 0 ----------------
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

# --- Flag whether the student was treated in their application year ----------
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

# --- Identify each treated student's first program year ---------------------
first_treatment <- merged_df |>
  filter(treated_ever == 1) |>
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
# FILTER APPLICATION YEARS
# =============================================================================
# Remove 2020 (COVID disruption invalidates treatment/control comparability).

message("\n=== FILTERING PROBLEMATIC APPLICATION YEARS ===\n")

pre_year_filter <- nrow(merged_df)

message("Current application year distribution:")
merged_df |>
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

merged_df <- merged_df |>
  filter(year != 2020)

post_year_filter <- nrow(merged_df)

message("\nRemoved ", pre_year_filter - post_year_filter, " observations")
message("  Year 2020 (COVID)")
message("Remaining: ", post_year_filter, " observations")

# =============================================================================
# FILTER TO ONE OBSERVATION PER STUDENT
# =============================================================================
# Treated students: keep the row for their FIRST treatment year.
# Control students: keep the row for their MOST RECENT application year.
#
# NOTE — timing asymmetry: treated covariates are measured at the year of
# first treatment (earlier on average), while control covariates are measured
# at the last application year (later on average). This means a control who
# applied in 2017 and again in 2021 is represented by their 2021 application.
# The propensity score model (script 5) exact-matches on year, which partially
# addresses this; however, if grade or other covariates drift meaningfully
# across application years, the asymmetry could induce residual imbalance.
# Verify covariate balance by year in script 5 (bal.tab cluster = "year").

message("\n=== FILTERING TO ONE OBSERVATION PER STUDENT ===\n")

pre_duplicate_filter <- nrow(merged_df)

merged_df_clean <- merged_df |>
  group_by(first_name, last_name) |>
  filter(
    (treated_ever == 1 & year == first_treatment_year) |
      (treated_ever == 0 & year == max(year))
  ) |>
  ungroup()

post_duplicate_filter <- nrow(merged_df_clean)

message(
  "Removed ",
  pre_duplicate_filter - post_duplicate_filter,
  " duplicate applications"
)
message("Remaining: ", post_duplicate_filter, " observations")

# =============================================================================
# VERIFICATION
# =============================================================================

message("\n=== FINAL SAMPLE SUMMARY ===\n")

message("Total observations: ", nrow(merged_df_clean))
message(
  "  Treated (first treatment year): ",
  sum(merged_df_clean$treated_in_year == 1)
)
message(
  "  Never treated (last application): ",
  sum(merged_df_clean$treated_ever == 0)
)

message("\n=== Treatment by Application Year ===\n")
merged_df_clean |>
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

message("\n=== Distribution of Times Treated ===\n")
merged_df_clean |>
  filter(treated_ever == 1) |>
  count(total_times_treated) |>
  arrange(total_times_treated) |>
  print()

# Check for remaining duplicates (should be NONE)
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
  message("\n=== WARNING: Unexpected Duplicates Remain ===\n")
  message(
    "Number of students with multiple records: ",
    nrow(remaining_duplicates)
  )
  print(remaining_duplicates)
} else {
  message("\n=== ✓ No Duplicates Remaining ===\n")
  message("Each student appears exactly once in the dataset")
}

message("\n=== Students Treated Before 2017 ===\n")
pre_2017_summary <- merged_df_clean |>
  filter(treated_before_2017 == 1) |>
  summarise(
    n = n(),
    n_treated_in_year = sum(treated_in_year == 1),
    n_control = sum(treated_in_year == 0)
  )

message("Total: ", sum(merged_df_clean$treated_before_2017))
if (sum(merged_df_clean$treated_before_2017) > 0) {
  message(
    "  Appearing as treated in 2017-2023: ",
    pre_2017_summary$n_treated_in_year
  )
  message("  Appearing as control: ", pre_2017_summary$n_control)
}

# --- Finalize and clean up ---------------------------------------------------
merged_df <- merged_df_clean

rm(
  first_treatment,
  merged_df_clean,
  remaining_duplicates,
  pre_2017_summary,
  pre_year_filter,
  post_year_filter,
  pre_duplicate_filter,
  post_duplicate_filter
)

message("\n=== MERGE COMPLETE ===\n")
message("Final dataset: merged_df with ", nrow(merged_df), " observations")
message(
  "  Application years: ",
  paste(sort(unique(merged_df$year)), collapse = ", ")
)

rm(list = setdiff(ls(), c("alum", "applicants", "merged_df")))
