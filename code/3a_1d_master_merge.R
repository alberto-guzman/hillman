# =============================================================================
# Merge Applicants and Alumni
# =============================================================================

# --- Merge applicant and alumni data ---
merged_df <- applicants |>
  left_join(alum, by = c("first_name", "last_name")) |>
  mutate(
    # Convert NA to 0 for all treatment year indicators
    across(
      starts_with("treated_20"),
      ~ ifelse(is.na(.), 0L, as.integer(.))
    ),
    # Also convert treated_ever and treated_before_2017 NA to 0
    treated_ever = ifelse(is.na(treated_ever), 0L, treated_ever),
    treated_before_2017 = ifelse(
      is.na(treated_before_2017),
      0L,
      treated_before_2017
    )
  )

# --- Create treated_in_year for the application year ---
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

# --- Identify first treatment year for each student ---
first_treatment <- merged_df |>
  filter(treated_ever == 1) |>
  group_by(first_name, last_name) |>
  summarise(
    first_treatment_year = min(year[treated_in_year == 1], na.rm = TRUE),
    total_times_treated = sum(treated_in_year == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Handle cases where they were in alumni but never treated in 2017-2023
  mutate(
    first_treatment_year = if_else(
      is.infinite(first_treatment_year),
      NA_real_,
      first_treatment_year
    )
  )

# --- Merge first treatment info back ---
merged_df <- merged_df |>
  left_join(
    first_treatment,
    by = c("first_name", "last_name")
  ) |>
  mutate(
    # Fill in 0 for never-treated students
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

# Remove only year 2020 (COVID)
merged_df <- merged_df |>
  filter(year != 2020)

post_year_filter <- nrow(merged_df)

message("\nRemoved ", pre_year_filter - post_year_filter, " observations")
message("  Year 2020 (COVID)")
message("Remaining: ", post_year_filter, " observations")

# =============================================================================
# FILTER TO ONE OBSERVATION PER STUDENT
# =============================================================================

message("\n=== FILTERING TO ONE OBSERVATION PER STUDENT ===\n")

pre_duplicate_filter <- nrow(merged_df)

merged_df_clean <- merged_df |>
  group_by(first_name, last_name) |>
  filter(
    # Keep if this is their first treatment year
    (treated_ever == 1 & year == first_treatment_year) |
      # OR keep if never treated AND this is their last application year
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

# Treatment by application year
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

# Distribution of times treated
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

# Check students treated before 2017
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

# --- Final dataset ---
merged_df <- merged_df_clean

# --- Clean up ---
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

# --- Clean up the environment ---
rm(list = setdiff(ls(), c("alum", "applicants", "merged_df")))
