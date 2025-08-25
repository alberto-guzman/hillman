# =============================================================================
# Merge School Info
# =============================================================================
library(tidyverse)
library(readxl)
library(janitor)
library(here)

# --- Start with PA schools
merged_df_pa <- merged_clean %>%
  mutate(
    hs_name_clean = str_to_lower(str_trim(high_school)),
    state = str_to_lower(str_trim(state))
  ) %>%
  filter(state %in% c("pa", "pennsylvania", "pa`"))

# Crosswalk: keep AUN + cleaned school names
crosswalk <- read_dta(here("data", "high_school_match.dta")) %>%
  select(hs_name_clean, pa_state_name, aun) %>%
  distinct(hs_name_clean, pa_state_name, .keep_all = TRUE) %>%
  rename(pa_state_name_cw = pa_state_name)

# Merge crosswalk onto base
merged_df_pa <- merged_df_pa %>%
  left_join(crosswalk, by = "hs_name_clean")

# =============================================================================
# Extract 2-year Postsecondary Enrollment (lagged) from Excel files
# =============================================================================

# Function to grab reported year from filename
extract_endyear <- function(path) {
  yrs <- stringr::str_extract_all(basename(path), "20\\d{2}")[[1]]
  if (length(yrs)) as.integer(tail(yrs, 1)) else NA_integer_
}

# Simple percent parser → 0–100
parse_pct <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  x <- ifelse(stringr::str_detect(x, "suppress|not apply|n/?a|^-$"), NA, x)
  out <- readr::parse_number(x)
  out <- ifelse(!is.na(out) & out <= 1, out * 100, out)
  ifelse(!is.na(out) & out >= 0 & out <= 100, out, NA_real_)
}

# Reader: sheet 3, pull only the 2yr higher ed enrollment (All Students)
read_sheet3_highered <- function(path) {
  if (length(readxl::excel_sheets(path)) < 3) {
    return(tibble())
  }

  df <- readxl::read_excel(path, sheet = 3) %>%
    janitor::clean_names()

  aun_col <- names(df)[stringr::str_detect(names(df), "\\baun\\b")]
  target_col <- names(df)[
    stringr::str_detect(
      names(df),
      stringr::regex(
        "two.*year.*prior.*enrolled.*higher.*education.*all_?student",
        ignore_case = TRUE
      )
    )
  ]

  if (!length(aun_col) || !length(target_col)) {
    return(tibble())
  }

  year_reported <- extract_endyear(path)

  df %>%
    dplyr::select(
      AUN = dplyr::all_of(aun_col),
      higher_ed_enrollment_2yr = dplyr::all_of(target_col)
    ) %>%
    dplyr::mutate(
      higher_ed_enrollment_2yr = parse_pct(higher_ed_enrollment_2yr),
      grad_year = year_reported - 2L # TRUE graduation cohort year
    ) %>%
    dplyr::filter(
      !is.na(AUN),
      !is.na(grad_year),
      !is.na(higher_ed_enrollment_2yr)
    )
}

# Collect files
files <- list.files(
  here::here("data/pa_stats"),
  pattern = "\\.xlsx$",
  full.names = TRUE
) %>%
  discard(~ stringr::str_detect(basename(.x), "^~\\$")) %>%
  keep(~ file.info(.x)$size > 0)

# Apply reader → keep only AUN + grad_year + value; unique (AUN, grad_year)
lagged_vars <- files %>%
  purrr::map_dfr(read_sheet3_highered) %>%
  dplyr::mutate(AUN = as.numeric(AUN)) %>%
  dplyr::filter(!is.na(AUN), !is.na(grad_year)) %>%
  dplyr::distinct(AUN, grad_year, .keep_all = TRUE) %>%
  dplyr::select(AUN, grad_year, higher_ed_enrollment_2yr)

# =============================================================================
# Merge lagged covariate into base dataframe
# =============================================================================
merged_df_pa_covars <- merged_df_pa %>%
  mutate(aun = as.numeric(aun)) %>%
  left_join(lagged_vars, by = c("aun" = "AUN", "year" = "grad_year"))

rm(list = setdiff(ls(), c("merged_df_pa", "merged_df_pa_covars")))
