# =============================================================================
# 2_1d_alumni_clean.R
#
# Purpose: Clean the Hillman alumni tracker and build a wide treatment
#          indicator dataset (one row per student, one column per program year).
#
# Input:   data/Alumni Tracker (...).csv
# Output:  `alum`   — wide data frame with treated_2017:2023, treated_ever,
#                     and treated_before_2017 indicators
#          `alum_n` — raw alumni counts by year
#          output/n_alumni_by_year.csv
# =============================================================================

# --- Load raw alumni tracker -------------------------------------------------
alum <- readr::read_csv(
  here::here("data", "Alumni Tracker (Updated 9.13.2023 - SJ) with Charts.csv"),
  col_types = readr::cols(...7 = readr::col_skip())
)

# =============================================================================
# STANDARDIZE NAMES
# =============================================================================
# Lowercase, strip nicknames/parentheticals, and collapse whitespace so names
# join cleanly with the applicant data.

alum <- alum |>
  mutate(
    first_name = First |>
      str_to_lower() |>
      str_replace_all('"(.*?)"|\\.\\((.*?)\\)', " ") |>
      str_replace_all("[^a-z]", " ") |>
      str_squish(),
    last_name = Last |>
      str_to_lower() |>
      str_replace_all('"(.*?)"|\\.\\((.*?)\\)', " ") |>
      str_replace_all("[^a-z]", " ") |>
      str_squish()
  ) |>
  select(-First, -Last)

# =============================================================================
# BUILD PARTICIPATION YEAR RANGE
# =============================================================================

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

# =============================================================================
# RESHAPE TO LONG AND DE-DUPLICATE
# =============================================================================

alum_long <- alum |>
  select(first_name, last_name, year1:year4) |>
  pivot_longer(
    cols = starts_with("year"),
    names_to = "year_col",
    values_to = "year"
  ) |>
  transmute(
    first_name,
    last_name,
    year = suppressWarnings(as.integer(year))
  ) |>
  drop_na(year)

# Manually verified duplicate entry
alum_long <- alum_long |>
  filter(!(first_name == "amanda" & last_name == "lu"))

alum_long <- alum_long |>
  distinct(first_name, last_name, year, .keep_all = TRUE)

# =============================================================================
# BUILD WIDE TREATMENT INDICATORS
# =============================================================================

alum <- alum_long |>
  mutate(treatment = 1L)

treated_years <- alum |>
  select(first_name, last_name, year, treatment) |>
  distinct() |>
  pivot_wider(
    names_from = year,
    values_from = treatment,
    names_prefix = "treated_",
    values_fill = 0
  )

treated_cols <- grep("^treated_\\d{4}$", names(treated_years), value = TRUE)
treated_cols <- treated_cols[order(as.integer(str_extract(
  treated_cols,
  "\\d{4}"
)))]

treated_years <- treated_years |>
  select(first_name, last_name, all_of(treated_cols))

# treated_ever: participated in any year
# treated_before_2017: participated before the analytic window (flagged, not dropped here)
treated_years <- treated_years |>
  mutate(
    treated_ever = if_else(
      rowSums(pick(starts_with("treated_")), na.rm = TRUE) > 0,
      1L,
      0L
    )
  )

treated_years <- treated_years |>
  mutate(
    treated_before_2017 = if_else(
      rowSums(
        pick(matches("^treated_200[0-9]$|^treated_201[0-6]$")),
        na.rm = TRUE
      ) >
        0,
      1L,
      0L
    )
  )

# --- Subset to analytic window (2017–2023) -----------------------------------
treated_years <- treated_years |>
  select(
    first_name,
    last_name,
    treated_ever,
    treated_before_2017,
    treated_2017:treated_2023
  )

# =============================================================================
# FINAL COUNTS
# =============================================================================

alum <- treated_years

alum_n <- alum_long |>
  count(year, name = "n_alumni") |>
  mutate(
    cumulative_n = cumsum(n_alumni),
    pct_of_total = round(n_alumni / sum(n_alumni) * 100, 1)
  )

write_csv(alum_n, here("output", "n_alumni_by_year.csv"))

alum_n

rm(list = setdiff(ls(), c("alum", "alum_n", "applicants", "applicant_n")))
