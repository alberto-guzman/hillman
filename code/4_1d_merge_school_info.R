# =============================================================================
# Merge School Info - Add normalized school names and AUN
# =============================================================================

library(tidyverse)
library(haven)
library(here)
library(readxl)
library(janitor)

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

# =============================================================================
# Create OVERALL file (all states) with normalized school names
# =============================================================================

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

# State distribution
message("\nStudents by state:")
merged_df_all |>
  count(state, sort = TRUE) |>
  print(n = 20)

# =============================================================================
# Load crosswalk and merge with ALL students
# =============================================================================

message("\n=== Loading Crosswalk and Adding AUN ===\n")

# Load crosswalk
crosswalk <- read_dta(here("data", "high_school_match.dta")) |>
  mutate(hs_name_clean = normalize_hs(hs_name_clean)) |>
  select(hs_name_clean, pa_state_name, aun) |>
  distinct(hs_name_clean, pa_state_name, .keep_all = TRUE) |>
  rename(pa_state_name_cw = pa_state_name)

message("Crosswalk loaded: ", nrow(crosswalk), " schools")

# Remove the Richland HS duplicate
crosswalk <- crosswalk |>
  filter(
    !(hs_name_clean == "pine richland hs" & pa_state_name_cw == "Richland HS")
  )

# Check for crosswalk duplicates
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

# Merge crosswalk onto ALL students (not just PA)
# Non-PA students will have NA for aun (which is correct)
merged_df_all <- merged_df_all |>
  left_join(crosswalk, by = "hs_name_clean")

message("\nAUN added to overall dataset")
message("  Students with AUN: ", sum(!is.na(merged_df_all$aun)))
message("  Students without AUN: ", sum(is.na(merged_df_all$aun)))

# =============================================================================
# Create PA-only file (now already has AUN from merged_df_all)
# =============================================================================

message("\n=== Creating PA-Only Dataset ===\n")

# Filter to PA schools
merged_df_pa <- merged_df_all |>
  filter(state %in% c("pa", "pennsylvania"))

message("PA students: ", nrow(merged_df_pa))
message(
  "PA unique schools: ",
  n_distinct(merged_df_pa$hs_name_clean, na.rm = TRUE)
)

# =============================================================================
# Merge PA School Level Vars
# =============================================================================

message("\n=== Loading and Merging School-Level Data ===\n")

# Load school-level data
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
  # De-duplicate: keep only first row per AUN
  group_by(aun) |>
  slice(1) |>
  ungroup()

message("School Fast Facts loaded and de-duplicated: ", 
        nrow(school_facts), " unique schools")

# Merge school facts onto PA students and filter to public schools only
merged_df_pa <- merged_df_pa |>
  left_join(school_facts, by = "aun") |>
  filter(!is.na(aun)) |>  # Keep only students with AUN
  filter(!is.na(school_enrollment))  # Keep only public schools with school data

message("\nPA public school students with school covariates: ", nrow(merged_df_pa))
message("  Treated: ", sum(merged_df_pa$treated_in_year == 1))
message("  Control: ", sum(merged_df_pa$treated_in_year == 0))
message("  Unique schools: ", n_distinct(merged_df_pa$aun))

# =============================================================================
# Validation Diagnostics
# =============================================================================

message("\n=== PA School Merge Diagnostics ===")

# How many PA schools total?
n_pa_schools <- merged_df_pa |>
  distinct(hs_name_clean) |>
  nrow()

# How many matched to crosswalk (got AUN)?
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

# Check student-level matches
message("\nStudent-level match (public schools only):")
message("  All students have AUN: ", sum(!is.na(merged_df_pa$aun)))
message("  All students have school data: ", sum(!is.na(merged_df_pa$school_enrollment)))

# By year
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

# School characteristics summary
message("\nSchool characteristics (averages):")
merged_df_pa |>
  summarise(
    avg_enrollment = round(mean(school_enrollment, na.rm = TRUE)),
    avg_pct_econ_disadv = round(mean(school_pct_econ_disadvantaged, na.rm = TRUE), 1),
    avg_pct_english_learner = round(mean(school_pct_english_learner, na.rm = TRUE), 1),
    avg_pct_special_ed = round(mean(school_pct_special_ed, na.rm = TRUE), 1),
    avg_pct_white = round(mean(school_pct_white, na.rm = TRUE), 1),
    n_title_i = sum(school_title_i == "Yes", na.rm = TRUE)
  ) |>
  glimpse()

# =============================================================================
# Save datasets
# =============================================================================

message("\n=== Saving Datasets ===\n")

# Save overall file (all states) - includes aun
write_csv(merged_df_all, here("data", "merged_all_states.csv"))
message(
  "Saved: data/merged_all_states.csv (",
  nrow(merged_df_all),
  " students)"
)

# Save PA public schools file (with school covariates)
write_csv(merged_df_pa, here("data", "merged_df_pa_public.csv"))
message("Saved: data/merged_df_pa_public.csv (", 
        nrow(merged_df_pa), 
        " students - public schools only)")

# Clean up
rm(list = setdiff(ls(), c("merged_df_all", "merged_df_pa", "crosswalk", "school_facts")))

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
message("  merged_df_pa: ", nrow(merged_df_pa), " students (PA public schools only)")
message("    - All have AUN and school covariates")
message("    - Unique schools: ", n_distinct(merged_df_pa$aun))