# =============================================================================
# Standardized variables
# =============================================================================

merged_clean <- merged_df |>
  mutate(
    # --- GPA: numeric, 0 becomes NA
    gpa = suppressWarnings(as.numeric(gpa)) |> na_if(0),

    # --- PSAT Math: numeric, 0 becomes NA
    psat_math = suppressWarnings(as.numeric(psat_math)) |> na_if(0),

    # --- Stipend: yes/no → 1/0
    stipend = case_when(
      str_detect(
        stipend,
        regex("\\b(yes|stipend eligible|yes student)\\b", ignore_case = TRUE)
      ) ~
        1L,
      TRUE ~ 0L
    ),

    # --- Household size: cap 1–11
    house_size = suppressWarnings(as.integer(house_size)),
    house_size = if_else(between(house_size, 1L, 11L), house_size, NA_integer_),

    # --- First-gen: yes/no → 1/0
    first_gen = case_when(
      str_detect(tolower(as.character(first_gen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(first_gen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Fix 2023 reverse-coded first-gen
    first_gen = if_else(
      year == 2023 & !is.na(first_gen),
      1L - first_gen, # Flip 0→1 and 1→0
      first_gen
    ),

    # --- Race / identity flags -----------------------------------------------
    racially_marginalized = case_when(
      str_detect(
        tolower(self_identity),
        "black|african|latino|latinx|hispanic|native|indian|pacific"
      ) ~
        1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),
    bi_multi_racial = case_when(
      str_detect(tolower(self_identity), "multi|bi-?racial|two or more") ~ 1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),

    # --- Geography flags ------------------------------------------------------
    urban = if_else(
      str_detect(tolower(geographic_location), "urban"),
      1L,
      0L,
      missing = NA_integer_
    ),
    suburban = if_else(
      str_detect(tolower(geographic_location), "suburban"),
      1L,
      0L,
      missing = NA_integer_
    ),
    rural = if_else(
      str_detect(tolower(geographic_location), "rural"),
      1L,
      0L,
      missing = NA_integer_
    ),

    # --- Disability: yes/no → 1/0
    disability = case_when(
      str_detect(
        documented_disability,
        regex("^\\s*no\\b", ignore_case = TRUE)
      ) ~
        0L,
      !is.na(documented_disability) ~ 1L,
      TRUE ~ NA_integer_
    ),

    # --- Negative school environment flag
    neg_school = case_when(
      str_detect(school_impact, regex("^\\s*no\\b", ignore_case = TRUE)) ~ 0L,
      !is.na(school_impact) ~ 1L,
      TRUE ~ NA_integer_
    ),

    # --- Citizenship: yes/no → 1/0
    us_citizen = case_when(
      str_detect(tolower(as.character(american_citizen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(american_citizen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# =============================================================================
# MERGE NSC OUTCOME DATA
# =============================================================================

library(haven)

# Load NSC outcome data
outcomes <- read_dta(here(
  "data/files_for_danielle_nsc",
  "clean_Hillman_PAonly.dta"
)) |>
  janitor::clean_names() |>
  rename(
    first_name = firstname,
    last_name = lastname
  ) |>
  select(
    first_name,
    last_name,
    seamless_enroll,
    seamless_enroll_stem,
    enrolled_ever_nsc,
    enrolled_ever_stem,
    degree_ever_nsc,
    degree_ever_stem_nsc,
    degree_6years_all_nsc,
    bachdegree_6years_all_nsc,
    ste_mbachdegree_6years_all_nsc
  )

message("Loaded NSC outcome data: ", nrow(outcomes), " records")

# Check for duplicates (without hs_grad_year since we're not using it)
outcomes_dupes <- outcomes %>%
  group_by(first_name, last_name) %>%
  filter(n() > 1) %>%
  ungroup()

message(
  "Found ",
  nrow(outcomes_dupes),
  " duplicate outcome records (",
  round(100 * nrow(outcomes_dupes) / nrow(outcomes), 1),
  "%)"
)

# Remove duplicates - keep first occurrence
outcomes <- outcomes %>%
  group_by(first_name, last_name) %>%
  slice(1) %>%
  ungroup()

message("After de-duplication: ", nrow(outcomes), " unique outcome records")

# Merge outcomes with cleaned data (NO hs_grad_year constraint)
merged_clean <- merged_clean |>
  left_join(
    outcomes,
    by = c("first_name", "last_name")
  )

# Code NSC non-matches as 0 (assume not enrolled)
merged_clean <- merged_clean %>%
  mutate(across(
    c(
      seamless_enroll,
      seamless_enroll_stem,
      enrolled_ever_nsc,
      enrolled_ever_stem,
      degree_ever_nsc,
      degree_ever_stem_nsc,
      degree_6years_all_nsc,
      bachdegree_6years_all_nsc,
      ste_mbachdegree_6years_all_nsc
    ),
    ~ if_else(is.na(.), 0, .)
  ))

# =============================================================================
# VALIDATE MERGE
# =============================================================================

message("\n=== NSC Outcome Merge Validation ===")
message("Total students: ", nrow(merged_clean))

# Overall match rates
n_with_nsc <- sum(merged_clean$enrolled_ever_nsc > 0 | 
                  !is.na(outcomes$first_name[match(
                    paste(merged_clean$first_name, merged_clean$last_name),
                    paste(outcomes$first_name, outcomes$last_name)
                  )]))

message("With NSC data: ", n_with_nsc, " (", 
        round(100 * n_with_nsc / nrow(merged_clean), 1), "%)")
message("Coded as non-enrolled: ", nrow(merged_clean) - n_with_nsc, " (",
        round(100 * (nrow(merged_clean) - n_with_nsc) / nrow(merged_clean), 1), "%)")

# Check balance of non-matches by treatment status (OVERALL)
message("\n=== NSC Match Rates by Treatment Status (OVERALL) ===")
merge_check_overall <- merged_clean %>%
  mutate(has_nsc_data = first_name %in% outcomes$first_name & 
                        last_name %in% outcomes$last_name) %>%
  group_by(treated_in_year) %>%
  summarise(
    n = n(),
    with_nsc = sum(has_nsc_data),
    pct_with_nsc = round(100 * with_nsc / n, 1),
    .groups = "drop"
  )

print(merge_check_overall)

# Check balance of non-matches by treatment status and YEAR
message("\n=== NSC Match Rates by Treatment Status and Year ===")
merge_check_year <- merged_clean %>%
  mutate(has_nsc_data = first_name %in% outcomes$first_name & 
                        last_name %in% outcomes$last_name) %>%
  group_by(year, treated_in_year) %>%
  summarise(
    n = n(),
    with_nsc = sum(has_nsc_data),
    pct_with_nsc = round(100 * with_nsc / n, 1),
    .groups = "drop"
  )

print(merge_check_year)

# Select final variables (including outcomes)
merged_clean <- merged_clean |>
  select(
    first_name,
    last_name,
    year,
    hs_grad_year,
    treated_ever,
    treated_in_year,
    years_treated,
    high_school,
    state,
    gender,
    grade,
    gpa,
    psat_math,
    stipend,
    house_size,
    racially_marginalized,
    bi_multi_racial,
    urban,
    suburban,
    rural,
    disability,
    neg_school,
    us_citizen,
    first_gen,
    # NSC outcomes
    seamless_enroll,
    seamless_enroll_stem,
    enrolled_ever_nsc,
    enrolled_ever_stem,
    degree_ever_nsc,
    degree_ever_stem_nsc,
    degree_6years_all_nsc,
    bachdegree_6years_all_nsc,
    ste_mbachdegree_6years_all_nsc
  )

message("\n=== Cleaning Complete ===")
message("Dataset includes ", nrow(merged_clean), " students with covariates and outcomes")

rm(list = setdiff(ls(), c("alum", "applicants", "merged_clean", "outcomes")))