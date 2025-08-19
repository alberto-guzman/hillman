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

# Finalize participants frame
alum <- alum_long
alum <- alum |>
  mutate(treatment = 1L)

# Keep only alum + applicants in memory
rm(list = setdiff(ls(), c("alum", "applicants")))
