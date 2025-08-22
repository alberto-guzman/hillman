# =============================================================================
# Merge School Info
# =============================================================================

# Subset to PA schools
merged_df_pa <- merged_clean |>
  mutate(
    hs_name_clean = str_to_lower(str_trim(high_school)),
    state = str_to_lower(str_trim(state))
  ) |>
  filter(state %in% c("pa", "pennsylvania", "pa`"))

# Read crosswalk and pre-rename to avoid .x/.y suffixes on join
crosswalk <- read_dta(here("data", "high_school_match.dta")) |>
  select(hs_name_clean, pa_state_name, aun) |>
  distinct(hs_name_clean, pa_state_name, .keep_all = TRUE) |>
  rename(pa_state_name_cw = pa_state_name)

# Join on the *cleaned* key and apply crosswalk only for Pennsylvania
merged_df_pa <- merged_df_pa |>
  left_join(crosswalk, by = "hs_name_clean")

rm(list = setdiff(ls(), c("merged_df_pa")))

# ────────────────────────────── Setup ──────────────────────────────
library(tidyverse)
library(readxl)
library(janitor)
library(stringi)
library(stringr)
library(readr)
library(fs)
library(here)
library(writexl)

# Toggle subgroup capture (FALSE = keep All Students only)
include_subgroups <- FALSE

# Directory with the Excel files
data_dir <- here("data/pa_stats")

# ─────────────────────────── Helpers ───────────────────────────────

# Normalize school names to a stable key (useful elsewhere if you join)
normalize_school <- function(x) {
  x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]+", " ") %>%
    str_replace_all("\\b(high|hs|school|sec)\\b", " ") %>%
    str_squish()
}

# Clean common suppression/NA tokens before numeric parse
clean_pct_token <- function(x) {
  y <- as.character(x) %>% str_trim() %>% str_to_lower()
  y <- str_replace_all(y, "[\u2013\u2014]", "-")  # en/em dashes → "-"
  ifelse(
    str_detect(y, "suppress|suppressed|does not apply|not apply|not available|n/?a|^-$|^--$|^—$|^–$|^\\.$|^$"),
    NA_character_,
    y
  )
}

# Parse a percentage that might be "85%", "<5%", or "0.87"
parse_pct_0to100 <- function(x) {
  y <- clean_pct_token(x)
  out <- readr::parse_number(y, na = c("", "na", "n/a"))
  out <- ifelse(!is.na(out) & out <= 1, out * 100, out)   # 0–1 → 0–100
  ifelse(!is.na(out) & out >= 0 & out <= 100, out, NA_real_)
}

# Parse a relaxed numeric (counts or already 0–100 rates)
parse_number_relaxed <- function(x) {
  y <- clean_pct_token(x)
  readr::parse_number(y, na = c("", "na", "n/a"))
}

# Extract the *ending* year from filenames like "..._20232024.xlsx" → 2024
extract_endyear <- function(path) {
  yrs <- str_extract_all(basename(path), "20\\d{2}")[[1]]
  if (length(yrs)) as.integer(tail(yrs, 1)) else NA_integer_
}

# Find likely school-name column after clean_names()
guess_name_col <- function(nm) {
  idx <- which(str_detect(nm, "\\b(name|school|schl|school_name)\\b"))
  if (length(idx)) nm[idx[1]] else NA_character_
}

# Columns that signal college access / enrollment covariates (post-clean_names)
access_terms <- regex(
  paste(
    c(
      "higher.*education",                 # enrolled in higher ed (2 yrs prior)
      "enlisted.*military",                # enlisted in the military (2 yrs prior)
      "entered.*workforce",                # entered PA workforce (2 yrs prior)
      "post.*secondary.*transition",       # school/military/work composite (not 2yr)
      "dual.*enroll",                      # dual enrollment
      "advanced.*placement|\\bap\\b",      # AP
      "international.*baccalaureate|\\bib\\b", # IB
      "rigorous.*courses",                 # rigorous coursework
      "industry.*(standards|competency)",  # industry competency/standards
      "nocti|nims",                        # NOCTI / NIMS
      "credential",                        # industry-recognized credentials
      "work.*based.*learning",             # WBL
      "\\bcte\\b|career.*technical",       # CTE
      "graduation.*(4|5).*year.*cohort"    # grad rates (useful covariates)
    ),
    collapse = "|"
  ),
  ignore_case = TRUE
)

# Keep only All Students unless subgroups requested
is_all_student <- function(colname) {
  str_detect(colname, "all_?student$") |
    !str_detect(
      colname,
      "americanindian|alaska|asian|hawaiian|pacific|black|hispanic|white|2or_more|two_or_more|economically_disadvantaged|english_learner|studentswithdisabilities|combined_ethnicity"
    )
}

# Heuristic: does column name look like a percent?
is_percent_col <- function(colname) {
  str_detect(colname, "^percent|_percent|percent_")
}

# Nice snake_case fallback for names
sanitize_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")
}

# Human-friendly metric names
# NOTE: “two years prior” measures are flagged with *_2yr
friendly_var <- function(x) {
  x <- str_to_lower(x)
  case_when(
    # two-year prior trio
    str_detect(x, "two.*year.*prior.*higher.*education") ~ "higher_ed_enrollment_2yr",
    str_detect(x, "two.*year.*prior.*enlisted.*military") ~ "military_enlistment_2yr",
    str_detect(x, "two.*year.*prior.*entered.*workforce") ~ "workforce_entry_2yr",

    # not-lagged composites / participation
    str_detect(x, "post.*secondary.*transition.*school.*military.*work") ~ "postsecondary_transition_rate",
    str_detect(x, "dual.*enroll") ~ "dual_enrollment_rate",
    str_detect(x, "advanced.*placement|\\bap\\b") & str_detect(x, "percent") ~ "ap_participation_rate",
    str_detect(x, "international.*baccalaureate|\\bib\\b") & str_detect(x, "percent") ~ "ib_participation_rate",
    str_detect(x, "rigorous.*courses") ~ "rigorous_course_rate",
    str_detect(x, "industry.*(standards|competency)") & str_detect(x, "percent.*advanced") ~ "industry_competency_advanced_rate",
    str_detect(x, "industry.*(standards|competency)") & str_detect(x, "percent") ~ "industry_competency_rate",
    str_detect(x, "nocti|nims") & str_detect(x, "competent|advanced|scoring") ~ "nocti_nims_competent_or_advanced_rate",
    str_detect(x, "credential") & str_detect(x, "percent") ~ "industry_credentials_rate",
    str_detect(x, "work.*based.*learning") & str_detect(x, "percent") ~ "work_based_learning_rate",
    str_detect(x, "\\bcte\\b|career.*technical") & str_detect(x, "percent") ~ "cte_participation_rate",
    str_detect(x, "graduation.*4.*year.*cohort") & str_detect(x, "percent") ~ "grad_rate_4yr",
    str_detect(x, "graduation.*5.*year.*cohort") & str_detect(x, "percent") ~ "grad_rate_5yr",

    TRUE ~ sanitize_name(x)
  )
}

# ───────────────────────── Extractor (Sheet 3) ───────────────────────

read_sheet3_access <- function(path, include_subgroups = FALSE, sheet = 3) {
  sheets <- readxl::excel_sheets(path)
  if (length(sheets) < sheet) {
    warning("Skipping (not enough sheets): ", basename(path)); return(tibble())
  }

  df <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal") %>% clean_names()
  if (!nrow(df)) {
    warning("Skipping (empty sheet): ", basename(path)); return(tibble())
  }

  nm <- names(df)
  name_col <- guess_name_col(nm)
  aun_col  <- nm[str_detect(nm, "\\baun\\b")]

  if (is.na(name_col))  { warning("Skipping (no school name column): ", basename(path)); return(tibble()) }
  if (!length(aun_col)) { warning("Skipping (no AUN column): ", basename(path)); return(tibble()) }

  # Candidate columns
  access_cols <- nm[str_detect(nm, access_terms)]
  if (!include_subgroups) access_cols <- access_cols[map_lgl(access_cols, is_all_student)]
  access_cols <- access_cols[!str_detect(access_cols, "statewide|goal|annual_progress|essagoal|annualprogress|state_goal")]

  if (!length(access_cols)) {
    warning("No access-related columns found in: ", basename(path)); return(tibble())
  }

  end_year <- extract_endyear(path)

  df %>%
    select(
      AUN = all_of(aun_col),
      school_name = all_of(name_col),
      all_of(access_cols)
    ) %>%
    pivot_longer(-c(AUN, school_name), names_to = "variable", values_to = "raw_value") %>%
    mutate(
      measure_type = if_else(is_percent_col(variable), "percent", "count"),
      value = if_else(
        measure_type == "percent",
        parse_pct_0to100(raw_value),
        parse_number_relaxed(raw_value)
      ),
      source_file = basename(path),
      source_year = end_year
    ) %>%
    filter(!is.na(AUN), !is.na(school_name), !is.na(value))
}

# ─────────────────────────── File list ──────────────────────────────

files <- dir_ls(data_dir, regexp = "\\.xlsx$", type = "file") %>%
  as.character() %>%
  discard(~ str_detect(basename(.x), "^~\\$")) %>%  # drop Excel temp files
  keep(~ file_info(.x)$size > 0)                    # drop zero-byte

# ───────────────────── Read all + bind (safe) ───────────────────────

safe_read_access <- purrr::possibly(read_sheet3_access, otherwise = tibble(), quiet = FALSE)

access_raw <- map(
  files,
  ~ safe_read_access(.x, include_subgroups = include_subgroups, sheet = 3)
) %>%
  keep(~ nrow(.x) > 0) %>%
  bind_rows()

# access_raw: AUN, school_name, variable, raw_value, measure_type, value, source_file, source_year

# ─────────────── Prepare tidy with friendly names ───────────────
access_tidy <- access_raw %>%
  transmute(
    AUN,
    school_name,
    year_reported = source_year,
    var  = friendly_var(variable),
    value
  ) %>%
  arrange(AUN, year_reported, var, desc(value)) %>%
  group_by(AUN, school_name, year_reported, var) %>%
  slice_head(n = 1) %>%    # keep a single best row if duplicates
  ungroup()

# ─────────────── Split into lagged vs non-lagged panels ───────────────

# Lagged (all *_2yr) → adjust year = reported - 2
lagged_vars <- access_tidy %>%
  filter(str_detect(var, "_2yr$")) %>%
  mutate(year = year_reported - 2L) %>%
  select(AUN, school_name, year, var, value) %>%
  pivot_wider(names_from = var, values_from = value, values_fn = dplyr::first) %>%
  relocate(AUN, school_name, year) %>%
  arrange(AUN, year)

# Non-lagged → keep reported year
nonlagged_vars <- access_tidy %>%
  filter(!str_detect(var, "_2yr$")) %>%
  transmute(AUN, school_name, year = year_reported, var, value) %>%
  pivot_wider(names_from = var, values_from = value, values_fn = dplyr::first) %>%
  relocate(AUN, school_name, year) %>%
  arrange(AUN, year)

# ─────────────── Write to Excel (two sheets) ───────────────
write_xlsx(
  list(
    lagged    = lagged_vars,
    nonlagged = nonlagged_vars
  ),
  "school_access_covariates.xlsx"
)

# Result:
#   - sheet "lagged": one row per AUN×year (year adjusted back 2), columns = *_2yr metrics
#   - sheet "nonlagged": one row per AUN×year (reported year), columns = other metrics
