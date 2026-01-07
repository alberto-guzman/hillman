# =============================================================================
# Merge School Info (with normalization + diagnostics)
# =============================================================================

# --- School name normalizer
normalize_hs <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_squish() |>
    stringr::str_replace_all(
      "\\b(highschool|high school|hs|shs|senior high)\\b",
      "hs"
    ) |>
    stringr::str_replace_all("\\b(jr|jr\\.|junior)\\b", "j") |>
    stringr::str_replace_all("\\bsr\\.?\\b", "s") |>
    stringr::str_replace_all("\\bcharter school\\b", "cs")
}

# --- Start with PA schools, apply normalization
merged_df_pa <- merged_clean |>
  mutate(
    hs_name_clean = normalize_hs(high_school),
    state = str_to_lower(str_trim(state))
  ) |>
  filter(state %in% c("pa", "pennsylvania"))

# --- Crosswalk: normalize school names too
# aun public and private
# school number is just public schools
crosswalk <- read_dta(here("data", "high_school_match.dta")) |>
  mutate(hs_name_clean = normalize_hs(hs_name_clean)) |>
  select(hs_name_clean, pa_state_name, aun) |>
  distinct(hs_name_clean, pa_state_name, .keep_all = TRUE) |>
  rename(pa_state_name_cw = pa_state_name)

# --- Check for crosswalk duplicates
crosswalk_dupes <- crosswalk |>
  group_by(hs_name_clean) |>
  filter(n() > 1) |>
  arrange(hs_name_clean)

if (nrow(crosswalk_dupes) > 0) {
  warning(
    "Crosswalk has ",
    nrow(crosswalk_dupes),
    " duplicate normalized school names"
  )
  print(crosswalk_dupes)
}

# --- Merge crosswalk onto base
merged_df_pa <- merged_df_pa |>
  left_join(crosswalk, by = "hs_name_clean")

# =============================================================================
# Extract 2-year Postsecondary Enrollment (lagged) from Excel files (robust)
# =============================================================================
extract_endyear <- function(path) {
  yrs <- stringr::str_extract_all(basename(path), "20\\d{2}")[[1]]
  if (length(yrs)) as.integer(tail(yrs, 1)) else NA_integer_
}

parse_pct <- function(x) {
  x0 <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  # treat many variants as missing
  missing_pattern <- "^(suppress|suppressed|not apply|not applicable|n/?a|na|^-$|^-+$|\\s*$)"
  x0[stringr::str_detect(x0, missing_pattern)] <- NA_character_
  out <- readr::parse_number(x0)
  # if percentages were given as proportions (<=1), convert to percent
  out <- ifelse(!is.na(out) & out <= 1, out * 100, out)
  # keep only 0-100 range
  out <- ifelse(!is.na(out) & out >= 0 & out <= 100, out, NA_real_)
  out
}

read_sheet3_highered <- function(path) {
  sheets <- readxl::excel_sheets(path)
  if (length(sheets) < 3) {
    message("Skipping (less than 3 sheets): ", basename(path))
    return(tibble())
  }

  df <- readxl::read_excel(path, sheet = 3) |> janitor::clean_names()
  nm <- names(df)

  # --- AUN column: choose first sensible match
  aun_candidates <- nm[stringr::str_detect(nm, "\\baun\\b")]
  if (!length(aun_candidates)) {
    message("No AUN column found in: ", basename(path))
    return(tibble())
  }
  aun_col <- aun_candidates[1]

  # --- Prioritized patterns to find the most likely enrollment column ---
  patterns <- c(
    # first: explicit long-form like 'percentofgraduates...enrolled...higher_education'
    "percent.*gradu.*two.*year.*enroll.*higher.*education",
    "percentof.*graduates.*two.*year.*enroll.*higher",
    "percent.*two.*year.*enroll.*higher.*education",
    # more generic variations
    "two[_ ]?year.*enroll.*higher",
    "enrolled.*in.*institution.*higher",
    "post.?secondary.*enroll",
    "percentofgraduates.*enrolled",
    "percent.*enroll.*higher",
    # fallback: any 'enroll' + 'higher' / 'institution' combination
    "enroll.*(higher|institution|post)",
    "enrolled.*higher"
  )

  chosen <- NULL
  matches <- character(0)
  for (pat in patterns) {
    m <- nm[stringr::str_detect(nm, stringr::regex(pat, ignore_case = TRUE))]
    if (length(m) > 0) {
      matches <- m
      chosen <- m[1]
      message(
        "File: ",
        basename(path),
        " -> matched pattern '",
        pat,
        "' selecting column '",
        chosen,
        "'"
      )
      break
    }
  }

  # if nothing matched, give a diagnostic and return empty
  if (is.null(chosen)) {
    message(
      "No enrollment column matched in file: ",
      basename(path),
      " — available cols (head): ",
      paste(head(nm, 20), collapse = ", ")
    )
    return(tibble())
  }

  # if multiple candidates matched for the chosen pattern, warn and pick first
  if (length(matches) > 1) {
    message(
      "Multiple candidate columns matched in ",
      basename(path),
      ". Choosing first: ",
      chosen,
      " (candidates: ",
      paste(matches, collapse = ", "),
      ")"
    )
  }

  # safe select / parse / return with tryCatch so a file can't abort the entire map_dfr
  out <- tryCatch(
    {
      df2 <- df |>
        dplyr::select(
          AUN = dplyr::all_of(aun_col),
          higher_ed_enrollment_2yr = dplyr::all_of(chosen)
        ) |>
        dplyr::mutate(
          higher_ed_enrollment_2yr = parse_pct(higher_ed_enrollment_2yr),
          grad_year = extract_endyear(path) - 2L
        ) |>
        dplyr::filter(
          !is.na(AUN),
          !is.na(grad_year),
          !is.na(higher_ed_enrollment_2yr)
        ) |>
        dplyr::mutate(AUN = suppressWarnings(as.numeric(AUN))) |>
        dplyr::select(AUN, grad_year, higher_ed_enrollment_2yr)

      df2
    },
    error = function(e) {
      message(
        "Failed to parse file: ",
        basename(path),
        " -> ",
        conditionMessage(e)
      )
      tibble()
    }
  )

  out
}

# --- Collect Excel files (same as before)
files <- list.files(
  here::here("data/pa_stats"),
  pattern = "\\.xlsx$",
  full.names = TRUE
) |>
  purrr::discard(~ stringr::str_detect(basename(.x), "^~\\$")) |>
  purrr::keep(~ file.info(.x)$size > 0)

# Map and combine; empty tibbles from bad files are fine
lagged_vars <- files |>
  purrr::map_dfr(read_sheet3_highered) |>
  dplyr::mutate(AUN = as.numeric(AUN)) |>
  dplyr::filter(!is.na(AUN), !is.na(grad_year)) |>
  dplyr::distinct(AUN, grad_year, .keep_all = TRUE) |>
  dplyr::select(AUN, grad_year, higher_ed_enrollment_2yr)


merged_df_pa_covars <- merged_df_pa |>
  mutate(
    aun = as.numeric(aun),
    year = as.integer(year)
  ) |>
  left_join(lagged_vars, by = c("aun" = "AUN", "year" = "grad_year"))

# =============================================================================
# Validation Diagnostics
# =============================================================================

# How many PA schools total?
n_pa_schools <- merged_df_pa |>
  distinct(hs_name_clean) |>
  nrow()

# How many matched to crosswalk (got AUN)?
n_with_aun <- merged_df_pa_covars |>
  filter(!is.na(aun)) |>
  distinct(hs_name_clean) |>
  nrow()

# How many got enrollment data?
n_with_enrollment <- merged_df_pa_covars |>
  filter(!is.na(higher_ed_enrollment_2yr)) |>
  distinct(hs_name_clean) |>
  nrow()

message("\n=== School Merge Diagnostics ===")
message("PA Schools: ", n_pa_schools)
message(
  "Matched to AUN: ",
  n_with_aun,
  " (",
  round(100 * n_with_aun / n_pa_schools, 1),
  "%)"
)
message(
  "With enrollment data: ",
  n_with_enrollment,
  " (",
  round(100 * n_with_enrollment / n_pa_schools, 1),
  "%)"
)

# Check individual records
message("\nRecords by enrollment data availability:")
merged_df_pa_covars |>
  mutate(has_enrollment = !is.na(higher_ed_enrollment_2yr)) |>
  group_by(year, has_enrollment) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(
    names_from = has_enrollment,
    values_from = n,
    names_prefix = "has_enrollment_"
  ) |>
  print()

rm(list = setdiff(ls(), c("merged_df_pa", "merged_df_pa_covars")))

# Write out CSV
write_csv(merged_df_pa_covars, here("data", "merged_df_pa_covars.csv"))
