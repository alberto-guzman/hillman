outcomes <- read_dta(here(
  "data/files_for_danielle_nsc",
  "clean_Hillman_PAonly.dta"
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
    ste_mbachdegree_6years_all_nsc
  )

outcomes <- outcomes %>%
  group_by(first_name, last_name) %>%
  slice(1) %>%
  ungroup()


# Merge outcome data with matched_data by first name, last name and, hs_grad_year
merged_df <- matched_data %>%
  left_join(outcomes, by = c("first_name", "last_name"))


rm(list = setdiff(ls(), "merged_df")) # Keep only the applicants data frame in memory
