# =============================================================================
# Merge
# =============================================================================

# Merge the applicant and alumni datasets
merged_df <- applicants |>
  left_join(alum, by = c("first_name", "last_name", "gender")) |>
  mutate(
    across(starts_with("treated_"), ~ ifelse(is.na(.), 0L, as.integer(.)))
  )

# Create a 'treated_in_year' variable based on year columns (keep your case_when)
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

rm(list = setdiff(ls(), c("alum", "applicants", "merged_df")))
