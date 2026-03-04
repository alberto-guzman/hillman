# =============================================================================
# 4_1d_merge_school_info.R
#
# Purpose: Normalize high school names, link students to PA school AUN codes
#          via a name crosswalk, and merge school-level covariates for the
#          PA public school subsample.
#
# Input:   `merged_clean` — analysis-ready student data (from script 3b)
#          data/high_school_match.dta — HS name → AUN crosswalk
#          data/SchoolFastFacts_20242025.xlsx — PA school-level covariates
# Output:  `merged_df_all` — all-states dataset with normalized school names
#                             and AUN where matched
#          `merged_df_pa`  — PA public school students with school covariates
#          data/merged_all_states.csv
#          data/merged_df_pa_public.csv
# =============================================================================

# --- School name normalizer --------------------------------------------------
# Strips punctuation, lowercases, and standardizes common suffixes
# (e.g. "High School" → "hs") to improve fuzzy matching with the crosswalk.
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

# =============================================================================
# BUILD ALL-STATES DATASET
# =============================================================================
# Apply name normalization to the full dataset. Non-PA students will receive
# NA for AUN after the crosswalk merge below.

message("\n=== Creating Overall Dataset (All States) ===\n")

merged_df_all <- merged_clean |>
  mutate(
    hs_name_clean = normalize_hs(high_school),
    state = str_to_lower(str_trim(state))
  )

message("Total students (all states): ", nrow(merged_df_all))
message(
  "Total unique schools (all states): ",
  n_distinct(merged_df_all$hs_name_clean, na.rm = TRUE)
)

message("\nStudents by state:")
merged_df_all |>
  count(state, sort = TRUE) |>
  print(n = 20)

# =============================================================================
# MERGE SCHOOL → AUN CROSSWALK
# =============================================================================
# Join the HS name crosswalk to add AUN codes. Duplicate crosswalk entries
# are resolved (one known manual fix for Pine Richland HS).

message("\n=== Loading Crosswalk and Adding AUN ===\n")

crosswalk <- read_dta(here("data", "high_school_match.dta")) |>
  mutate(hs_name_clean = normalize_hs(hs_name_clean)) |>
  select(hs_name_clean, pa_state_name, aun) |>
  distinct(hs_name_clean, pa_state_name, .keep_all = TRUE) |>
  rename(pa_state_name_cw = pa_state_name)

message("Crosswalk loaded: ", nrow(crosswalk), " schools")

crosswalk <- crosswalk |>
  filter(
    !(hs_name_clean == "pine richland hs" & pa_state_name_cw == "Richland HS")
  )

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
} else {
  message("No duplicate school names in crosswalk")
}

merged_df_all <- merged_df_all |>
  left_join(crosswalk, by = "hs_name_clean")

message("\nAUN added to overall dataset")
message("  Students with AUN: ", sum(!is.na(merged_df_all$aun)))
message("  Students without AUN: ", sum(is.na(merged_df_all$aun)))

# =============================================================================
# BUILD PA PUBLIC SCHOOL DATASET
# =============================================================================
# Filter to PA students, then merge PA school-level covariates from the
# School Fast Facts file. Retain only students matched to a public school
# (i.e., with a valid AUN and non-missing enrollment data).

message("\n=== Creating PA-Only Dataset ===\n")

merged_df_pa <- merged_df_all |>
  filter(state %in% c("pa", "pennsylvania"))

message("PA students: ", nrow(merged_df_pa))
message(
  "PA unique schools: ",
  n_distinct(merged_df_pa$hs_name_clean, na.rm = TRUE)
)

# --- Load PA school-level covariates -----------------------------------------
message("\n=== Loading and Merging School-Level Data ===\n")

school_facts <- readxl::read_excel(
  "/Users/albertoguzman-alvarez/Projects/inProgress/2020_hillman/data/SchoolFastFacts_20242025.xlsx"
) |>
  janitor::clean_names() |>
  # Ensure AUN is numeric to match merged_df_pa
  mutate(aun = as.numeric(aun)) |>
  # Keep only desired variables and rename
  select(
    aun,
    school_title_i = title_i_school,
    school_enrollment = enrollment,
    school_pct_econ_disadvantaged = economically_disadvantaged,
    school_pct_english_learner = english_learner,
    school_pct_special_ed = special_education,
    school_pct_white = white
  ) |>
  group_by(aun) |>
  slice(1) |>
  ungroup()

message(
  "School Fast Facts loaded and de-duplicated: ",
  nrow(school_facts),
  " unique schools"
)

merged_df_pa <- merged_df_pa |>
  left_join(school_facts, by = "aun") |>
  filter(!is.na(aun)) |>
  filter(!is.na(school_enrollment))

message(
  "\nPA public school students with school covariates: ",
  nrow(merged_df_pa)
)
message("  Treated: ", sum(merged_df_pa$treated_in_year == 1))
message("  Control: ", sum(merged_df_pa$treated_in_year == 0))
message("  Unique schools: ", n_distinct(merged_df_pa$aun))

# =============================================================================
# VALIDATION DIAGNOSTICS
# =============================================================================

message("\n=== PA School Merge Diagnostics ===")

n_pa_schools <- merged_df_pa |> distinct(hs_name_clean) |> nrow()
n_with_aun <- merged_df_pa |>
  filter(!is.na(aun)) |>
  distinct(hs_name_clean) |>
  nrow()

message("PA Public Schools: ", n_pa_schools)
message(
  "Matched to AUN: ",
  n_with_aun,
  " (",
  round(100 * n_with_aun / n_pa_schools, 1),
  "%)"
)

message("\nStudent-level match (public schools only):")
message("  All students have AUN: ", sum(!is.na(merged_df_pa$aun)))
message(
  "  All students have school data: ",
  sum(!is.na(merged_df_pa$school_enrollment))
)

message("\nPA public school students by application year:")
merged_df_pa |>
  group_by(year) |>
  summarise(
    n = n(),
    n_treated = sum(treated_in_year == 1),
    pct_treated = round(100 * n_treated / n, 1),
    .groups = "drop"
  ) |>
  print()

message("\nSchool characteristics (averages):")
merged_df_pa |>
  summarise(
    avg_enrollment = round(mean(school_enrollment, na.rm = TRUE)),
    avg_pct_econ_disadv = round(
      mean(school_pct_econ_disadvantaged, na.rm = TRUE),
      1
    ),
    avg_pct_english_learner = round(
      mean(school_pct_english_learner, na.rm = TRUE),
      1
    ),
    avg_pct_special_ed = round(mean(school_pct_special_ed, na.rm = TRUE), 1),
    avg_pct_white = round(mean(school_pct_white, na.rm = TRUE), 1),
    n_title_i = sum(school_title_i == "Yes", na.rm = TRUE)
  ) |>
  glimpse()

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

message("\n=== Saving Datasets ===\n")

write_csv(merged_df_all, here("data", "merged_all_states.csv"))
message(
  "Saved: data/merged_all_states.csv (",
  nrow(merged_df_all),
  " students)"
)

write_csv(merged_df_pa, here("data", "merged_df_pa_public.csv"))
message(
  "Saved: data/merged_df_pa_public.csv (",
  nrow(merged_df_pa),
  " students - public schools only)"
)

# --- Final summary and clean up ----------------------------------------------
rm(
  list = setdiff(
    ls(),
    c("merged_df_all", "merged_df_pa", "crosswalk", "school_facts")
  )
)

message("\n=== School Merge Complete ===")
message("Final datasets in memory:")
message("  merged_df_all: ", nrow(merged_df_all), " students (all states)")
message(
  "    - PA students with AUN: ",
  sum(
    !is.na(merged_df_all$aun) &
      merged_df_all$state %in% c("pa", "pennsylvania")
  )
)
message("    - Non-PA students (AUN = NA): ", sum(is.na(merged_df_all$aun)))
message(
  "  merged_df_pa: ",
  nrow(merged_df_pa),
  " students (PA public schools only)"
)
message("    - All have AUN and school covariates")
message("    - Unique schools: ", n_distinct(merged_df_pa$aun))

rm(
  list = setdiff(
    ls(),
    c("merged_df_all", "merged_df_pa")
  )
)
