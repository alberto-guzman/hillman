# =============================================================================
# Merge Applicants and Alumni
# =============================================================================

# --- Merge applicant and alumni data ---
merged_df <- applicants |>
  left_join(alum, by = c("first_name", "last_name", "gender")) |>
  mutate(
    across(
      starts_with("treated_20"),
      ~ ifelse(is.na(.), 0L, as.integer(.))
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
      TRUE ~ NA_real_
    ),
    # Simplified ever-treated indicator
    treated_ever = if_else(
      rowSums(pick(starts_with("treated_20")), na.rm = TRUE) > 0,
      1L,
      0L
    ),
    # Count how many years treated
    years_treated = rowSums(pick(starts_with("treated_20")), na.rm = TRUE)
  )

# --- Verify the filtering ---
merged_df |>
  group_by(treated_ever, year) |>
  summarise(
    n = n(),
    n_treated_in_year = sum(treated_in_year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = treated_ever,
    values_from = c(n, n_treated_in_year),
    names_glue = "{.value}_treated_ever_{treated_ever}"
  )

# --- Clean up the environment ---
rm(list = setdiff(ls(), c("alum", "applicants", "merged_df")))
