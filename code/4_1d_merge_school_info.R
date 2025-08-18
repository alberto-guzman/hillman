# =============================================================================
# School-level college enrollment (2-yr post-grad) and merge by school name
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(here)
library(tidyr)

# -----------------------------------------------------------------------------
# Helper: normalize school names for safer joins
# -----------------------------------------------------------------------------
normalize_school <- function(x) {
  x |>
    tolower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish() |>
    # common suffix noise
    str_replace_all("\\b(high school|hs|school district|sd|district)\\b", "") |>
    str_squish()
}

# -----------------------------------------------------------------------------
# List .xlsx files (skip temp and specific year bundle)
# -----------------------------------------------------------------------------
files <- list.files(
  here("data/pa_stats"),
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)
files <- files[!grepl("~\\$", basename(files))] # drop Excel temp files
files <- files[!grepl("20172018", basename(files))] # exclude that year
files <- files[file.info(files)$size > 0] # drop zero-byte files

# -----------------------------------------------------------------------------
# Reader for the 3rd sheet; picks school name + “two-year higher ed enrollment”
# -----------------------------------------------------------------------------
read_one <- function(path) {
  sheets <- readxl::excel_sheets(path)
  if (length(sheets) < 3) {
    warning("Skipping (less than 3 sheets): ", basename(path))
    return(tibble())
  }

  df <- readxl::read_excel(path, sheet = 3)

  # school name column guess
  nm <- tolower(names(df))
  name_idx <- which(nm %in% c("name", "school", "school_name", "schl"))
  if (length(name_idx) == 0) {
    name_idx <- which(str_detect(nm, "\\bname\\b|school"))
  }
  if (length(name_idx) == 0) {
    warning("Skipping (no school name column): ", basename(path))
    return(tibble())
  }
  name_col <- names(df)[name_idx[1]]

  # two-year postsecondary enrollment column guess
  target_regex <- regex(
    "percent.*graduates.*two\\s*years.*enrolled.*(institution|inst).*higher.*education.*all\\s*student",
    ignore_case = TRUE
  )
  match_cols <- names(df)[str_detect(names(df), target_regex)]
  if (length(match_cols) == 0) {
    warning("Skipping (no Higher Ed enrollment column): ", basename(path))
    return(tibble())
  }
  enroll_col <- match_cols[1]

  # extract year from filename (first 4-digit token), fallback NA
  file_year <- str_extract(basename(path), "\\b(20\\d{2})\\b") |> as.integer()

  out <- df |>
    select(
      school_name = all_of(name_col),
      college_enrollment_pct = all_of(enroll_col)
    ) |>
    mutate(
      # percent clean-up
      college_enrollment_pct = as.character(college_enrollment_pct),
      college_enrollment_pct = str_replace_all(college_enrollment_pct, "%", ""),
      college_enrollment_pct = suppressWarnings(as.numeric(
        college_enrollment_pct
      )),
      college_enrollment_pct = ifelse(
        !is.na(college_enrollment_pct) & college_enrollment_pct <= 1,
        college_enrollment_pct * 100,
        college_enrollment_pct
      ),
      source_file = basename(path),
      source_year = file_year
    ) |>
    filter(!is.na(school_name), !is.na(college_enrollment_pct))
}

# -----------------------------------------------------------------------------
# Read all files and bind
# -----------------------------------------------------------------------------
school_enroll_raw <- map(files, ~ try(read_one(.x), silent = TRUE)) |>
  keep(~ inherits(.x, "data.frame") && nrow(.x) > 0) |>
  bind_rows()

# If multiple rows per school across files/years, keep the latest year with data
school_enroll <- school_enroll_raw |>
  mutate(school_key = normalize_school(school_name)) |>
  arrange(school_key, desc(source_year), desc(college_enrollment_pct)) |>
  group_by(school_key) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(
    school_key,
    college_enrollment_pct,
    source_year,
    source_file
  )
