# =============================================================================
# Hillman Summer Alumni
# =============================================================================

# Load the data
alum <- readr::read_csv(
  here::here("data", "Alumni Tracker (Updated 9.13.2023 - SJ) with Charts.csv"),
  col_types = readr::cols(...7 = readr::col_skip())
)

# Clean column names
alum <- alum |>
  rename_with(
    ~ str_to_lower(.) |>
      str_replace_all("\\s", "_") |>
      str_replace_all(":", "") |>
      str_replace_all("\\?", "")
  )

# Clean first and last names
alum <- alum |>
  mutate(
    first_name = str_to_lower(first) |> str_remove("\\(.*") |> str_squish(),
    last_name = str_to_lower(last) |> str_remove("\\(.*") |> str_squish()
  ) |>
  select(-first, -last)

# Clean gender (1 = male, 0 = female, else NA)
alum <- alum |>
  mutate(
    gender = case_when(
      tolower(as.character(gender)) %in% c("m", "male") ~ 1,
      tolower(as.character(gender)) %in% c("f", "female") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Create max and min participation year
# (guards against all-NA rows so pmax/pmin don't return Inf/-Inf)
alum <- alum |>
  mutate(
    year_max = if_else(
      if_all(c(year1, year2, year3, year4), is.na),
      NA_real_,
      pmax(year1, year2, year3, year4, na.rm = TRUE)
    ),
    year_min = if_else(
      if_all(c(year1, year2, year3, year4), is.na),
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

# De-duplicate panel keys (first, last, year)
alum_long <- alum_long |>
  distinct(first_name, last_name, year, .keep_all = TRUE)

# Remove a specifically flagged duplicate row (include year if known)
alum_long <- alum_long |> filter(!(first_name == "amanda" & last_name == "lu"))

# Finalize participants frame
alum <- alum_long
alum <- alum |>
  mutate(treatment = 1L)

# Count of unique participants by year
alum |>
  group_by(year) |>
  summarise(
    total_unique_students = n_distinct(paste0(first_name, "|", last_name)),
    .groups = "drop"
  )

# Keep only alum + applicants in memory
rm(list = setdiff(ls(), c("alum", "applicants")))
