# =============================================================================
# Hillman Summer Alumni
# =============================================================================

# Loan data
alum <- readr::read_csv(
  here::here("data", "Alumni Tracker (Updated 9.13.2023 - SJ) with Charts.csv"),
  col_types = readr::cols(...7 = readr::col_skip())
)

# Clean names
alum <- alum |>
  mutate(
    first_name = First |>
      str_to_lower() |>
      str_replace_all('"(.*?)"|\\((.*?)\\)', " ") |>
      str_replace_all("[^a-z]", " ") |>
      str_squish(),

    last_name = Last |>
      str_to_lower() |>
      str_replace_all('"(.*?)"|\\((.*?)\\)', " ") |>
      str_replace_all("[^a-z]", " ") |>
      str_squish()
  ) |>
  select(-First, -Last)

# Clean gender
alum <- alum |>
  rename(gender = Gender) |>
  mutate(
    gender = if_else(
      str_to_lower(as.character(gender)) %in% c("m", "male"),
      1,
      0
    )
  )

# Create max and min participation year
alum <- alum |>
  mutate(
    year_max = if_else(
      if_all(year1:year4, is.na),
      NA_real_,
      pmax(year1, year2, year3, year4, na.rm = TRUE)
    ),
    year_min = if_else(
      if_all(year1:year4, is.na),
      NA_real_,
      pmin(year1, year2, year3, year4, na.rm = TRUE)
    )
  )

# Pivot to long format
alum_long <- alum |>
  select(first_name, last_name, year1:year4, gender) |>
  pivot_longer(
    cols = starts_with("year"),
    names_to = "year_col",
    values_to = "year"
  ) |>
  transmute(
    first_name,
    last_name,
    year = suppressWarnings(as.integer(year)),
    gender
  ) |>
  drop_na(year)

# De-duplicate
alum_long <- alum_long |>
  distinct(first_name, last_name, year, .keep_all = TRUE)

# Remove a specifically flagged duplicate row
alum_long <- alum_long |> filter(!(first_name == "amanda" & last_name == "lu"))

# Create treatment variable
alum <- alum_long
alum <- alum |>
  mutate(treatment = 1L)


# Pivot the alum dataset to create separate treatment columns for each year
treated_years <- alum |>
  select(first_name, last_name, gender, year, treatment) |>
  distinct() |>
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
treated_years <- treated_years |>
  select(first_name, last_name, gender, all_of(treated_cols))

# Create an "ever treated" variable based on the year-specific treatment columns
treated_years <- treated_years |>
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
treated_years <- treated_years |>
  mutate(
    treated_before_2017 = if_else(
      rowSums(
        select(cur_data(), matches("^treated_200[0-9]$|^treated_201[0-6]$")),
        na.rm = TRUE
      ) >
        0,
      1L,
      0L
    )
  )


# Subset treated years (keep 2017:2023)
treated_years <- treated_years |>
  select(
    first_name,
    last_name,
    gender,
    treated_ever,
    treated_before_2017,
    treated_2017:treated_2023
  )


treated_years |>
  select(treated_2017:treated_2023) |>
  pivot_longer(everything(), names_to = "year", values_to = "treated") |>
  group_by(year, treated) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = treated, values_from = n, values_fill = 0) |>
  arrange(year) |>
  print()


alum <- treated_years


# Keep only alum + applicants in memory
rm(list = setdiff(ls(), c("alum", "applicants")))
