# =============================================================================
# Merge Applicants and Alumni (One Row per Student for College Enrollment Analysis)
# =============================================================================

# --- Merge applicant and alumni data ---
merged_df <- applicants |>
  left_join(alum, by = c("first_name", "last_name", "gender")) |>
  mutate(
    across(
      c(
        "treated_2017",
        "treated_2018",
        "treated_2019",
        "treated_2020",
        "treated_2021",
        "treated_2022",
        "treated_2023"
      ),
      ~ ifelse(is.na(.), 0L, as.integer(.))
    )
  )

# --- Create a treated_in_year variable based on year columns ---
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
    )
  )

# --- Create a simplified ever-treated indicator ---
merged_df <- merged_df |>
  mutate(
    treated_ever = if_else(
      (treated_2017 +
        treated_2018 +
        treated_2019 +
        treated_2020 +
        treated_2021 +
        treated_2022 +
        treated_2023) >
        0,
      1L,
      0L
    )
  )

# --- Keep only the first (earliest) application per student ---
merged_df <- merged_df |>
  arrange(first_name, last_name, year) |>
  group_by(first_name, last_name) |>
  slice(1) |> # keep earliest appearance
  ungroup()

# --- Ensure treatment indicator persists for those who participated later ---
merged_df <- merged_df |>
  mutate(
    treated_ever = if_else(is.na(treated_ever), 0L, treated_ever)
  )

# --- Clean up the environment ---
rm(list = setdiff(ls(), c("alum", "applicants", "merged_df")))
