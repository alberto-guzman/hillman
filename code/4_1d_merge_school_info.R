# =============================================================================
# 4_1d_merge_school_info.R
#
# Purpose: Normalize high school names, link students to PA school AUN codes
#          via a name crosswalk, and merge school-level covariates for the
#          PA public school subsample.
#
# Input:   `merged_clean` — analysis-ready student data (from script 3b)
#          data/processed/high_school_match.dta — HS name → AUN crosswalk
#          data/raw/SchoolFastFacts_20242025.xlsx — PA school-level covariates
# Output:  `merged_df_all`   — all-states dataset with normalized school names
#                               and AUN where matched
#          `merged_df_pa`    — PA public school students with school covariates
#          `merged_df_all_n` — applicant and alumni counts by year (all states)
#          `merged_df_pa_n`  — applicant and alumni counts by year (PA public)
#          data/processed/merged_all_states.csv
#          data/processed/merged_df_pa_public.csv
#          output/counts/n_merged_all_by_year.csv
#          output/counts/n_merged_pa_by_year.csv
# =============================================================================

dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "counts"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# HELPER FUNCTION
# =============================================================================
# Strips punctuation, lowercases, and standardizes common suffixes
# (e.g., "High School" → "hs") to improve matching with the crosswalk.

normalize_hs <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish() |>
    str_replace_all(
      "\\b(highschool|high school|hs|shs|senior high)\\b",
      "hs"
    ) |>
    str_replace_all("\\b(jr|jr\\.|junior)\\b", "j") |>
    str_replace_all("\\bsr\\.?\\b", "s") |>
    str_replace_all("\\bcharter school\\b", "cs")
}

# =============================================================================
# BUILD ALL-STATES DATASET
# =============================================================================

merged_df_all <- merged_clean |>
  mutate(
    hs_name_clean = normalize_hs(high_school),
    state = str_to_lower(str_trim(state)),
    # Normalize state abbreviations to full lowercase names
    state = case_match(
      state,
      "al" ~ "alabama",
      "ak" ~ "alaska",
      "az" ~ "arizona",
      "ar" ~ "arkansas",
      "ca" ~ "california",
      "co" ~ "colorado",
      "ct" ~ "connecticut",
      "de" ~ "delaware",
      "fl" ~ "florida",
      "ga" ~ "georgia",
      "hi" ~ "hawaii",
      "id" ~ "idaho",
      "il" ~ "illinois",
      "in" ~ "indiana",
      "ia" ~ "iowa",
      "ks" ~ "kansas",
      "ky" ~ "kentucky",
      "la" ~ "louisiana",
      "me" ~ "maine",
      "md" ~ "maryland",
      "ma" ~ "massachusetts",
      "mi" ~ "michigan",
      "mn" ~ "minnesota",
      "ms" ~ "mississippi",
      "mo" ~ "missouri",
      "mt" ~ "montana",
      "ne" ~ "nebraska",
      "nv" ~ "nevada",
      "nh" ~ "new hampshire",
      "nj" ~ "new jersey",
      "nm" ~ "new mexico",
      "ny" ~ "new york",
      "nc" ~ "north carolina",
      "nd" ~ "north dakota",
      "oh" ~ "ohio",
      "ok" ~ "oklahoma",
      "or" ~ "oregon",
      "pa" ~ "pennsylvania",
      "ri" ~ "rhode island",
      "sc" ~ "south carolina",
      "sd" ~ "south dakota",
      "tn" ~ "tennessee",
      "tx" ~ "texas",
      "ut" ~ "utah",
      "vt" ~ "vermont",
      "va" ~ "virginia",
      "wa" ~ "washington",
      "wv" ~ "west virginia",
      "wi" ~ "wisconsin",
      "wy" ~ "wyoming",
      "dc" ~ "district of columbia",
      .default = state
    )
  )

message("Total students (all states): ", nrow(merged_df_all))
message(
  "Unique schools (all states): ",
  n_distinct(merged_df_all$hs_name_clean, na.rm = TRUE)
)

# =============================================================================
# MERGE SCHOOL → AUN CROSSWALK
# =============================================================================

# schoolnumber (crosswalk) = schl (SchoolFastFacts) — school-level within district.
# aun alone is district-level; join on aun + schoolnumber to get the right school.
crosswalk <- read_dta(here("data", "processed", "high_school_match.dta")) |>
  mutate(hs_name_clean = normalize_hs(hs_name_clean)) |>
  select(hs_name_clean, pa_state_name, aun, schoolnumber) |>
  distinct(hs_name_clean, pa_state_name, .keep_all = TRUE) |>
  rename(pa_state_name_cw = pa_state_name)

message("Crosswalk loaded: ", nrow(crosswalk), " schools")

# Manually verified: pine richland hs maps to two crosswalk entries; drop the wrong one
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

message("Students with AUN: ", sum(!is.na(merged_df_all$aun)))
message("Students without AUN: ", sum(is.na(merged_df_all$aun)))

# =============================================================================
# BUILD PA PUBLIC SCHOOL DATASET
# =============================================================================
# Filter to PA students, merge school-level covariates, then retain only
# students matched to a public school (valid AUN + non-missing enrollment).

merged_df_pa <- merged_df_all |>
  filter(state == "pennsylvania")

message("PA students: ", nrow(merged_df_pa))

# schl is a unique school-level identifier within each district (aun).
# Join on aun + schl = schoolnumber to get the specific high school's stats,
# not an arbitrary school from the same district.
school_facts <- readxl::read_excel(here(
  "data", "raw",
  "SchoolFastFacts_20242025.xlsx"
)) |>
  janitor::clean_names() |>
  mutate(
    aun  = as.numeric(aun),
    schl = as.numeric(schl)
  ) |>
  select(
    aun,
    schl,
    school_title_i = title_i_school,
    school_enrollment = enrollment,
    school_pct_econ_disadvantaged = economically_disadvantaged,
    school_pct_english_learner = english_learner,
    school_pct_special_ed = special_education,
    school_pct_white = white
  ) |>
  mutate(school_title_i = as.integer(school_title_i == "Yes"))

message("School Fast Facts loaded: ", nrow(school_facts), " schools")

merged_df_pa <- merged_df_pa |>
  left_join(school_facts, by = c("aun", "schoolnumber" = "schl")) |>
  filter(!is.na(aun), !is.na(school_enrollment))

# Two-step PA attrition diagnostic
n_pa_raw <- merged_df_all |> filter(state == "pennsylvania") |> nrow()
n_pa_after_aun <- merged_df_all |>
  filter(state == "pennsylvania", !is.na(aun)) |>
  nrow()
n_pa_final <- nrow(merged_df_pa)

message("PA attrition breakdown:")
message("  PA students before filter:     ", n_pa_raw)
message(
  "  After dropping no-AUN:         ",
  n_pa_after_aun,
  "  (-",
  n_pa_raw - n_pa_after_aun,
  ")"
)
message(
  "  After dropping no school data: ",
  n_pa_final,
  "  (-",
  n_pa_after_aun - n_pa_final,
  ")"
)

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

write_csv(merged_df_all, here("data", "processed", "merged_all_states.csv"))
write_csv(merged_df_pa, here("data", "processed", "merged_df_pa_public.csv"))

message(
  "Saved: data/processed/merged_all_states.csv (",
  nrow(merged_df_all),
  " students)"
)
message(
  "Saved: data/processed/merged_df_pa_public.csv (",
  nrow(merged_df_pa),
  " students)"
)

# =============================================================================
# FINAL COUNTS
# =============================================================================

merged_df_all_n <- merged_df_all |>
  count(year, name = "n_total") |>
  left_join(
    merged_df_all |>
      filter(treated_in_year == 1) |>
      count(year, name = "n_alumni"),
    by = "year"
  ) |>
  mutate(
    n_alumni = replace_na(n_alumni, 0L),
    n_applicants = n_total - n_alumni,
    pct_alumni = round(n_alumni / n_total * 100, 1)
  ) |>
  select(year, n_applicants, n_alumni, n_total, pct_alumni)

merged_df_pa_n <- merged_df_pa |>
  count(year, name = "n_total") |>
  left_join(
    merged_df_pa |>
      filter(treated_in_year == 1) |>
      count(year, name = "n_alumni"),
    by = "year"
  ) |>
  mutate(
    n_alumni = replace_na(n_alumni, 0L),
    n_applicants = n_total - n_alumni,
    pct_alumni = round(n_alumni / n_total * 100, 1)
  ) |>
  select(year, n_applicants, n_alumni, n_total, pct_alumni)

write_csv(merged_df_all_n, here("output", "counts", "n_merged_all_by_year.csv"))
write_csv(merged_df_pa_n, here("output", "counts", "n_merged_pa_by_year.csv"))

merged_df_all_n
merged_df_pa_n

# --- Final clean up ----------------------------------------------
rm(
  list = setdiff(
    ls(),
    c("merged_df_all", "merged_df_pa", "merged_df_all_n", "merged_df_pa_n")
  )
)
