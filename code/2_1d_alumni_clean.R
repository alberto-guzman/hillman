# =============================================================================
# 2_1d_alumni_clean.R
#
# Purpose: Load and clean the Hillman alumni tracker. Outputs one row per
#          student with treatment flags and cleaned names for merging to
#          applicant file in 3a_1d_master_merge.R.
#
# Name cleaning:
#   - Strip quoted/parenthetical nicknames: "ore", (ivy), etc.
#   - Strip suffixes: jr, sr, ii, iii
#   - Strip apostrophes and non-alpha characters (hyphens to spaces)
#   - Known spelling variant: hiruni hewapathirana mayunne to mayadunne
#
# treated_before_2017: created HERE from year columns -- do NOT recreate in 3a
#
# Input:  data/Alumni Tracker (Updated 9.13.2023 - SJ) with Charts.csv
# Output: `alum`   -- one row per student with treatment flags
#         `alum_n` -- alumni counts by participation year
# =============================================================================

library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(stringr)

dir.create(here("output", "counts"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD RAW ALUMNI TRACKER
# =============================================================================

alum_raw <- read_csv(
  here(
    "data", "raw", "alumni",
    "Alumni Tracker (Updated 9.13.2023 - SJ) with Charts.csv"
  ),
  show_col_types = FALSE
) |>
  clean_names()

message("Raw alumni tracker: ", nrow(alum_raw), " rows")

# After clean_names(): First -> first, Last -> last, year1-year4 (no underscore)
# Guard: confirm no year5+ columns exist -- pivot assumes max year4
stopifnot(!any(str_detect(names(alum_raw), "^year5")))

# =============================================================================
# CLEAN NAMES
# =============================================================================
# Legal names used to match against application files, which use
# personal_information_first/last_name for 2022/2023 files.
# Cleaning steps:
#   1. Strip quoted nicknames: "ore", "venice", "max"
#   2. Strip parenthetical nicknames: (ivy), (zahra)
#   3. Strip suffixes: jr, sr, ii, iii
#   4. Strip non-alpha characters (hyphens, apostrophes -> spaces)
#   5. Normalize whitespace

alum <- alum_raw |>
  mutate(
    first_name = first |>
      str_to_lower() |>
      str_remove_all('"[^"]*"') |>
      str_remove_all('\\([^)]*\\)') |>
      str_replace_all("[^a-z]", " ") |>
      str_squish(),
    last_name = last |>
      str_to_lower() |>
      str_remove_all('"[^"]*"') |>
      str_remove_all('\\([^)]*\\)') |>
      str_remove("\\s+(jr|sr|ii|iii)\\.?$") |>
      str_replace_all("[^a-z]", " ") |>
      str_squish()
  ) |>
  mutate(
    last_name = case_when(
      first_name == "hiruni" & str_detect(last_name, "mayunne") ~
        str_replace(last_name, "mayunne", "mayadunne"),
      TRUE ~ last_name
    )
  ) |>
  select(-first, -last)

message(
  "After name cleaning: ",
  n_distinct(alum$first_name),
  " unique first names, ",
  n_distinct(alum$last_name),
  " unique last names"
)

# =============================================================================
# PIVOT TO LONG FORMAT
# =============================================================================
# year columns are year1, year2, year3, year4 (no underscore after clean_names)

alum_long <- alum |>
  pivot_longer(
    cols = matches("^year[0-9]+$"),
    names_to = "participation_number",
    values_to = "year"
  ) |>
  filter(!is.na(year)) |>
  mutate(
    participation_number = as.integer(str_extract(
      participation_number,
      "[0-9]+"
    )),
    year = as.integer(year)
  )

# Manually verified duplicate -- remove one row for amanda lu
alum_long <- alum_long |>
  filter(
    !(first_name == "amanda" & last_name == "lu" & participation_number == 2)
  )

message("alum_long after dedup: ", nrow(alum_long), " rows")

# =============================================================================
# BUILD TREATMENT FLAGS
# =============================================================================
# treated_before_2017: student first participation year is before 2017.
#   Created here -- do NOT recreate in 3a.
# first_treatment_year: earliest participation year.
# total_times_treated: number of distinct participation years.

alum_flags <- alum_long |>
  group_by(first_name, last_name) |>
  summarise(
    first_treatment_year = min(year, na.rm = TRUE),
    total_times_treated = n_distinct(year),
    .groups = "drop"
  ) |>
  mutate(
    treated_ever = 1L,
    treated_before_2017 = if_else(first_treatment_year < 2017, 1L, 0L)
  )

message(
  "treated_before_2017: ",
  sum(alum_flags$treated_before_2017),
  " students"
)

# Build year-specific treatment indicators (2017-2023 only)
treated_years <- alum_long |>
  filter(between(year, 2017L, 2023L)) |>
  distinct(first_name, last_name, year) |>
  mutate(treated_in_year = 1L) |>
  pivot_wider(
    names_from = year,
    values_from = treated_in_year,
    names_prefix = "treated_",
    values_fill = 0L
  )

# Ensure all years 2017-2023 present even if no alumni in that year
for (yr in 2017:2023) {
  col <- paste0("treated_", yr)
  if (!col %in% names(treated_years)) {
    treated_years[[col]] <- 0L
  }
}

# Order year columns
treated_years <- treated_years |>
  select(first_name, last_name, paste0("treated_", 2017:2023))

# Join flags and year indicators back to alum (one row per student)
alum <- alum |>
  select(-matches("^year[0-9]+$")) |>
  left_join(alum_flags, by = c("first_name", "last_name")) |>
  left_join(treated_years, by = c("first_name", "last_name")) |>
  mutate(across(starts_with("treated_"), ~ replace_na(.x, 0L)))

message("alum: ", nrow(alum), " rows x ", ncol(alum), " cols")

# =============================================================================
# FINAL COUNTS
# =============================================================================

alum_n <- alum_long |>
  count(year, name = "n_alumni") |>
  mutate(
    cumulative_n = cumsum(n_alumni),
    pct_of_total = round(n_alumni / sum(n_alumni) * 100, 1)
  ) |>
  arrange(year)

write_csv(alum_n, here("output", "counts", "n_alumni_by_year.csv"))
alum_n

# =============================================================================
# CLEAN UP
# =============================================================================

rm(
  list = setdiff(
    ls(),
    c("alum", "alum_n", "applicants", "applicant_n")
  )
)
