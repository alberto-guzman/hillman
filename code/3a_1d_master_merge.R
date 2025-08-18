# =============================================================================
# Merge prep: alumni → year-wide treatment flags, then join
# =============================================================================

# Pivot the alum dataset to create separate treatment columns for each year
treated_years <- alum %>%
  select(first_name, last_name, gender, year, treatment) %>%
  distinct() %>%
  pivot_wider(
    names_from = year,
    values_from = treatment,
    names_prefix = "treated_",
    values_fill = 0
  )

# Reorder the columns so treated_* are chronological (ID columns first)
treated_cols <- grep("^treated_\\d{4}$", names(treated_years), value = TRUE)
treated_cols <- treated_cols[order(as.integer(str_extract(
  treated_cols,
  "\\d{4}"
)))]
treated_years <- treated_years %>%
  select(first_name, last_name, gender, all_of(treated_cols))

# Create an "ever treated" variable based on the year-specific treatment columns
treated_years <- treated_years %>%
  mutate(
    treated_ever = if_else(
      (coalesce(treated_2017, 0) +
        coalesce(treated_2018, 0) +
        coalesce(treated_2019, 0) +
        coalesce(treated_2020, 0) +
        coalesce(treated_2021, 0) +
        coalesce(treated_2022, 0) +
        coalesce(treated_2023, 0)) >
        0,
      1L,
      0L
    )
  )

# Create a "treated before 2017" variable
treated_years <- treated_years %>%
  mutate(
    treated_before_2017 = if_else(
      rowSums(
        select(., matches("^treated_200[0-9]$|^treated_201[0-6]$")),
        na.rm = TRUE
      ) >
        0,
      1L,
      0L
    )
  )

# Subset treated years (keep 2017:2023 like your original)
treated_years <- treated_years %>%
  select(
    first_name,
    last_name,
    gender,
    treated_ever,
    treated_before_2017,
    treated_2017:treated_2023
  )

# Frequency table for treated_2017 to treated_2023 (using pivot_* instead of gather/spread)
treated_years %>%
  select(treated_2017:treated_2023) %>%
  pivot_longer(everything(), names_to = "year", values_to = "treated") %>%
  group_by(year, treated) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = treated, values_from = n, values_fill = 0) %>%
  arrange(year) %>%
  print()

# Merge the applicant and treated_years datasets
merged_df <- applicants %>%
  left_join(treated_years, by = c("first_name", "last_name", "gender")) %>%
  mutate(
    across(starts_with("treated_"), ~ ifelse(is.na(.), 0L, as.integer(.)))
  )

# Create a 'treated_in_year' variable based on year columns (keep your case_when)
merged_df <- merged_df %>%
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

# pastTreatments = cumulative prior treatments
merged_df <- merged_df %>%
  arrange(first_name, last_name, gender, year) %>%
  group_by(first_name, last_name, gender) %>%
  mutate(pastTreatments = cumsum(treated_in_year) - treated_in_year) %>%
  ungroup()

# Multi-year treatment check
treated_long <- merged_df %>%
  select(first_name, last_name, gender, starts_with("treated_")) %>%
  pivot_longer(
    cols = starts_with("treated_"),
    names_to = "year_var",
    values_to = "treated"
  ) %>%
  mutate(year = str_extract(year_var, "\\d{4}") %>% as.integer()) %>%
  filter(treated == 1)

multi_treated <- treated_long %>%
  group_by(first_name, last_name, gender) %>%
  summarise(years_treated = n(), .groups = "drop") %>%
  filter(years_treated > 1)

print(multi_treated)


rm(list = setdiff(ls(), c("alum", "applicants", "merged_df")))
