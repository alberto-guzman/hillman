# =============================================================================
# 3b_1d_merge_clean.R
#
# Purpose: Standardize covariates in the merged applicant/alumni dataset,
#          merge NSC college outcome data, and restrict to the analytic sample.
#
# Outcome strategy:
#   ITT enrollment outcomes  — all students in analytic sample; students with
#     no NSC record OR enroll_seamless == 0 both coded as non-enrolled (0).
#     Cohorts: 2018–2023.
#   NSC-conditional outcomes — students with an NSC record only (has_nsc_record
#     == 1). Post-enrollment outcomes (persistence, degree) further conditioned
#     on enroll_ever == 1. Non-attainers recoded NA → 0 within sample.
#     Cohorts: 2018–2022 (insufficient follow-up for 2023 degree outcomes).
#
# Naming scheme:
#   Prefix:  enroll_    enrollment outcomes
#            reten_     retention (same institution)
#            pers_      persistence (any institution)
#            deg_any_   any degree attainment
#            deg_bach_  bachelor's degree attainment
#            inst_      institution characteristics
#   Suffix:  _itt       intent-to-treat (NA → 0 for all students)
#            _stem      STEM-specific version
#            _Xy        within X years of HS graduation
#            _ever      any time observed
#
# Policy-relevant outcomes:
#   ITT:  enroll_seamless_itt, enroll_ever_itt,
#         enroll_seamless_stem_itt, enroll_ever_stem_itt
#   NSC:  enroll_seamless, enroll_ever, enroll_delayed, enroll_firsttime_fulltime,
#         enroll_seamless_stem, enroll_ever_stem,
#         inst_public4yr_entry, inst_private4yr_entry, inst_4yr_entry,
#         inst_instate_entry,
#         reten_1y, pers_1y, pers_1y_stem,
#         deg_any_ever, deg_bach_ever,
#         deg_any_6y, deg_bach_4y, deg_bach_6y
#
# NOTE: deg_any_4y dropped — confirmed identical to deg_any_6y in raw NSC file
#       (degree_4years_all_nsc == degree_6years_all_nsc for all cohorts).
#       deg_bach_4y retained — distinct from deg_bach_6y for 2018 cohort.
#       reten_1y/pers_1y restricted to cohorts <= 2021 — 2022 cohort has no
#       follow-up year in the NSC file; all zeros would reflect missing data,
#       not true non-retention.
#       pers_1y_stem: persistence in a STEM major at any institution next fall.
#         NOT conditional on STEM entry — any fall entrant coded 0/1.
#         See 4c PSE persistence.do for coding details.
#
# Input:   `merged_df` — one row per student (from script 3a)
#          data/files_for_danielle_nsc/clean_Hillman_2017_2025.dta — NSC data
# Output:  `merged_clean`   — analysis-ready dataset
#          `merged_clean_n` — applicant and alumni counts by year
#          output/n_merged_clean_by_year.csv
# =============================================================================

# =============================================================================
# STANDARDIZE COVARIATES
# =============================================================================
merged_clean <- merged_df |>
  mutate(
    gpa = na_if(gpa, 0),
    psat_math = na_if(psat_math, 0),

    stipend = case_when(
      str_detect(
        stipend,
        regex("\\b(yes|stipend eligible|yes student)\\b", ignore_case = TRUE)
      ) ~ 1L,
      TRUE ~ 0L
    ),

    house_size = suppressWarnings(as.integer(house_size)),
    house_size = if_else(between(house_size, 1L, 11L), house_size, NA_integer_),

    first_gen = case_when(
      str_detect(tolower(as.character(first_gen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(first_gen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    ),

    racially_marginalized = case_when(
      str_detect(
        tolower(self_identity),
        "black|african|latino|latinx|hispanic|native|indian|pacific"
      ) ~ 1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),
    bi_multi_racial = case_when(
      str_detect(tolower(self_identity), "multi|bi-?racial|two or more") ~ 1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),

    # geographic_location contains dirty values (race responses entered in wrong
    # field; "Do not wish to answer"; "Rural/SmallTown" variant with no space).
    # Normalize valid values first, then derive mutually exclusive indicators
    # anchored at start of string so "Suburban" cannot match "^urban".
    geographic_location = case_when(
      str_detect(tolower(geographic_location), "^urban") ~ "Urban",
      str_detect(tolower(geographic_location), "^suburban") ~ "Suburban",
      str_detect(tolower(geographic_location), "^rural") ~ "Rural/Small Town",
      TRUE ~ NA_character_ # catches dirty values, "Do not wish to answer", NA
    ),

    urban = if_else(
      geographic_location == "Urban",
      1L,
      0L,
      missing = NA_integer_
    ),
    suburban = if_else(
      geographic_location == "Suburban",
      1L,
      0L,
      missing = NA_integer_
    ),
    rural = if_else(
      geographic_location == "Rural/Small Town",
      1L,
      0L,
      missing = NA_integer_
    ),

    disability = case_when(
      str_detect(
        documented_disability,
        regex("^\\s*no\\b", ignore_case = TRUE)
      ) ~ 0L,
      !is.na(documented_disability) ~ 1L,
      TRUE ~ NA_integer_
    ),
    neg_school = case_when(
      str_detect(school_impact, regex("^\\s*no\\b", ignore_case = TRUE)) ~ 0L,
      !is.na(school_impact) ~ 1L,
      TRUE ~ NA_integer_
    ),

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

dir.create(here("output", "counts"), recursive = TRUE, showWarnings = FALSE)

outcomes <- read_dta(here(
  "data/files_for_danielle_nsc",
  "clean_Hillman_2017_2025.dta"
)) |>
  janitor::clean_names() |>
  rename(
    first_name = firstname,
    last_name = lastname
  ) |>
  select(
    first_name,
    last_name,
    hs_grad_year,
    # enrollment
    seamless_enroll,
    seamless_enroll_stem,
    enrolled_ever_nsc,
    enrolled_ever_stem,
    delayed_enrollment_nsc,
    firsttime_fulltime,
    # institution type at entry
    public4yr_initial,
    private4yr_initial,
    x4yr_initial,
    instate_initial_nsc,
    # retention & persistence
    reten_fall_enter,
    pers_fall_enter,
    pers_fall_enter_stem,
    # degree — ever
    degree_ever_nsc,
    bach_ever_nsc,
    # degree — by year window
    # NOTE: degree_4years_all_nsc confirmed identical to degree_6years_all_nsc
    #       for all cohorts — only the 6yr variable is retained
    degree_6years_all_nsc,
    bachdegree_4years_all_nsc,
    bachdegree_6years_all_nsc
  ) |>
  rename(
    enroll_seamless = seamless_enroll,
    enroll_seamless_stem = seamless_enroll_stem,
    enroll_ever = enrolled_ever_nsc,
    enroll_ever_stem = enrolled_ever_stem,
    enroll_delayed = delayed_enrollment_nsc,
    enroll_firsttime_fulltime = firsttime_fulltime,
    inst_public4yr_entry = public4yr_initial,
    inst_private4yr_entry = private4yr_initial,
    inst_4yr_entry = x4yr_initial,
    inst_instate_entry = instate_initial_nsc,
    reten_1y = reten_fall_enter,
    pers_1y = pers_fall_enter,
    pers_1y_stem = pers_fall_enter_stem,
    deg_any_ever = degree_ever_nsc,
    deg_bach_ever = bach_ever_nsc,
    deg_any_6y = degree_6years_all_nsc,
    deg_bach_4y = bachdegree_4years_all_nsc,
    deg_bach_6y = bachdegree_6years_all_nsc
  )

message("Loaded NSC outcome data: ", nrow(outcomes), " records")

# De-duplicate: keep row with most non-missing outcomes; break ties by
# preferring enroll_seamless == 1
outcomes_dupes <- outcomes |>
  group_by(first_name, last_name, hs_grad_year) |>
  filter(n() > 1) |>
  ungroup()

if (nrow(outcomes_dupes) > 0) {
  message(
    "Found ",
    nrow(outcomes_dupes),
    " duplicate NSC records — deduplicating"
  )
  outcomes <- outcomes |>
    group_by(first_name, last_name, hs_grad_year) |>
    arrange(
      desc(rowSums(!is.na(across(everything())))),
      desc(enroll_seamless),
      .by_group = TRUE
    ) |>
    slice(1) |>
    ungroup()
  message("After de-duplication: ", nrow(outcomes), " unique outcome records")
} else {
  message("No duplicate NSC records found")
}

merged_clean <- merged_clean |>
  left_join(outcomes, by = c("first_name", "last_name", "hs_grad_year")) |>
  mutate(enroll_delayed = as.integer(enroll_delayed))

# =============================================================================
# FLAG NSC MATCH (before dropping unmatched students)
# =============================================================================
# has_nsc_record = 1 for all students returned in the NSC query.
# Students without a record carry NA on all NSC-conditional outcomes.

merged_clean <- merged_clean |>
  mutate(has_nsc_record = if_else(!is.na(enroll_seamless), 1L, 0L))

match_by_treatment <- merged_clean |>
  summarise(
    n = n(),
    n_matched = sum(has_nsc_record),
    pct_matched = round(100 * n_matched / n, 1),
    .by = treated_in_year
  )

message("NSC match rates by treatment status:")
print(match_by_treatment)

# =============================================================================
# ITT ENROLLMENT OUTCOMES (all students, cohorts 2018–2023)
# =============================================================================
# No NSC record AND enroll_seamless == 0 both treated as non-enrolled (0).
# Explicit as.integer() cast — source .dta variables are dbl.
# STEM ITT outcomes follow same logic: no NSC record OR not in STEM = 0.

merged_clean <- merged_clean |>
  mutate(
    enroll_seamless_itt = as.integer(replace_na(
      as.integer(enroll_seamless),
      0L
    )),
    enroll_ever_itt = as.integer(replace_na(as.integer(enroll_ever), 0L)),
    enroll_seamless_stem_itt = as.integer(replace_na(
      as.integer(enroll_seamless_stem),
      0L
    )),
    enroll_ever_stem_itt = as.integer(replace_na(
      as.integer(enroll_ever_stem),
      0L
    ))
  )

# =============================================================================
# FILTER TO ANALYTIC SAMPLE (graduation cohorts 2018–2023)
# =============================================================================
# 2017: only 18 students, predates program — excluded
# 2024/2025: too recent even for enrollment outcomes — excluded
# 2023: included for ITT enrollment; degree/persistence NA by design

pre_filter_n <- nrow(merged_clean)

removal_stats <- merged_clean |>
  summarise(
    missing_grad_year = sum(is.na(hs_grad_year)),
    missing_grade = sum(is.na(grade)),
    invalid_grade = sum(!is.na(grade) & (grade < 9 | grade > 12)),
    outside_range = sum(
      !is.na(hs_grad_year) &
        !is.na(grade) &
        grade >= 9 &
        grade <= 12 &
        (hs_grad_year < 2018 | hs_grad_year > 2023)
    )
  )

merged_clean <- merged_clean |>
  filter(
    !is.na(hs_grad_year),
    !is.na(grade),
    grade >= 9,
    grade <= 12,
    hs_grad_year >= 2018,
    hs_grad_year <= 2023
  )

message(
  "Cohort filter removed ",
  pre_filter_n - nrow(merged_clean),
  " observations:"
)
message("  Missing hs_grad_year: ", removal_stats$missing_grad_year)
message("  Missing grade:        ", removal_stats$missing_grade)
message("  Invalid grade:        ", removal_stats$invalid_grade)
message("  Outside 2018–2023:    ", removal_stats$outside_range)
message("Remaining: ", nrow(merged_clean))

# =============================================================================
# NSC-CONDITIONAL OUTCOMES — recode NA → 0 within sample
# =============================================================================
# Retention/persistence: conditional on enroll_ever == 1, cohorts 2018–2021.
#   2022 cohort excluded — NSC file has no fall 2023 follow-up records so
#   all 2022 reten/pers values would be 0 (missing data, not true dropout).
# pers_1y_stem: same conditioning as pers_1y — any fall entrant, 2018–2021.
#   Measures whether student is in a STEM major at any institution next fall.
#   NOT conditional on having entered as STEM (see header note).
# Degree: conditional on enroll_ever == 1, cohorts 2018–2022.
#   deg_bach_ever: coded only when bach degree record exists in NSC — no 0
#   baseline in Danielle's .do file. Recode NA → 0 for all enrolled students
#   in 2018–2022 to ensure non-bach-graduates are correctly coded 0.
# 2023 cohort intentionally left NA — insufficient follow-up.

merged_clean <- merged_clean |>
  mutate(
    # -- retention & persistence: only enrolled students, 2018–2021 only
    across(
      c(reten_1y, pers_1y, pers_1y_stem),
      ~ if_else(
        as.integer(enroll_ever) == 1L & hs_grad_year <= 2021 & is.na(.x),
        0L,
        as.integer(.x)
      )
    ),
    # -- degree outcomes: only enrolled students, 2018–2022
    across(
      c(deg_any_ever, deg_bach_ever, deg_any_6y, deg_bach_4y, deg_bach_6y),
      ~ if_else(
        as.integer(enroll_ever) == 1L & hs_grad_year <= 2022 & is.na(.x),
        0L,
        as.integer(.x)
      )
    )
  )

message(
  "NSC-conditional recoding complete. ",
  "Enrolled (has_nsc_record == 1 & enroll_ever == 1): ",
  sum(
    merged_clean$has_nsc_record == 1 & merged_clean$enroll_ever == 1,
    na.rm = TRUE
  )
)

# =============================================================================
# SELECT FINAL VARIABLE SET
# =============================================================================

merged_clean <- merged_clean |>
  select(
    # identifiers & admin
    first_name,
    last_name,
    year,
    hs_grad_year,

    # treatment
    treated_ever,
    treated_in_year,
    total_times_treated,
    treated_before_2017,
    first_treatment_year,

    # demographics & covariates
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

    # NSC match flag
    has_nsc_record,

    # -------------------------------------------------------------------------
    # ITT ENROLLMENT OUTCOMES (all students, 2018–2023)
    # NA → 0: non-NSC-matched and non-enrolled both coded 0
    # -------------------------------------------------------------------------
    enroll_seamless_itt,
    enroll_ever_itt,
    enroll_seamless_stem_itt,
    enroll_ever_stem_itt,

    # -------------------------------------------------------------------------
    # NSC-CONDITIONAL OUTCOMES (has_nsc_record == 1)
    # -------------------------------------------------------------------------

    # -- enrollment
    enroll_seamless,
    enroll_seamless_stem,
    enroll_ever,
    enroll_ever_stem,
    enroll_delayed,
    enroll_firsttime_fulltime,

    # -- institution type at entry
    inst_public4yr_entry,
    inst_private4yr_entry,
    inst_4yr_entry,
    inst_instate_entry,

    # -- retention: same institution, year 1 (enroll_ever == 1, 2018–2021)
    reten_1y,

    # -- persistence: any institution, year 1 (enroll_ever == 1, 2018–2021)
    # pers_1y_stem: in a STEM major at any institution next fall
    pers_1y,
    pers_1y_stem,

    # -- degree attainment: ever (enroll_ever == 1, 2018–2022)
    deg_any_ever,
    deg_bach_ever,

    # -- degree attainment: by year window (enroll_ever == 1, 2018–2022)
    # deg_any_4y dropped: confirmed identical to deg_any_6y in raw NSC file
    deg_any_6y,
    deg_bach_4y,
    deg_bach_6y
  )

# =============================================================================
# FINAL COUNTS
# =============================================================================

merged_clean_n <- merged_clean |>
  count(year, name = "n_total") |>
  left_join(
    merged_clean |>
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

write_csv(merged_clean_n, here("output", "counts", "n_merged_clean_by_year.csv"))

merged_clean_n

# =============================================================================
# DATA QUALITY CHECKS
# =============================================================================

# -----------------------------------------------------------------------------
# CHECK 1: RAW NSC — non-seamless students with post-enrollment outcomes
# Expected: small n, all delayed enrollees
# -----------------------------------------------------------------------------
outcomes_check <- read_dta(here(
  "data/files_for_danielle_nsc",
  "clean_Hillman_2017_2025.dta"
)) |>
  janitor::clean_names() |>
  rename(first_name = firstname, last_name = lastname)

nsc_anomalies <- outcomes_check |>
  filter(seamless_enroll == 0) |>
  filter(
    reten_fall_enter == 1 |
      pers_fall_enter == 1 |
      degree_ever_nsc == 1 |
      degree_6years_all_nsc == 1 |
      bach_ever_nsc == 1
  )

message(
  "CHECK 1 — Non-seamless students with post-enrollment outcomes: ",
  nrow(nsc_anomalies),
  " (expect small n of delayed enrollees)"
)
if (nrow(nsc_anomalies) > 0) {
  nsc_anomalies |>
    count(enrolled_ever_nsc, delayed_enrollment_nsc) |>
    print()
}
rm(outcomes_check, nsc_anomalies)

# -----------------------------------------------------------------------------
# CHECK 2: ITT outcomes — no NAs, must be integer
# -----------------------------------------------------------------------------
message(
  "CHECK 2 — ITT enrollment NAs (all should be 0) and types (should be integer):"
)
merged_clean |>
  summarise(across(
    c(
      enroll_seamless_itt,
      enroll_ever_itt,
      enroll_seamless_stem_itt,
      enroll_ever_stem_itt
    ),
    ~ sum(is.na(.x)),
    .names = "{.col}_na"
  )) |>
  print()

message("  Types:")
merged_clean |>
  summarise(across(
    c(
      enroll_seamless_itt,
      enroll_ever_itt,
      enroll_seamless_stem_itt,
      enroll_ever_stem_itt
    ),
    ~ class(.x),
    .names = "{.col}_type"
  )) |>
  print()

# -----------------------------------------------------------------------------
# CHECK 3: NSC-conditional NAs only for has_nsc_record == 0
# and 2023 cohort on degree/persistence
# -----------------------------------------------------------------------------
message("CHECK 3 — NSC-conditional NAs by has_nsc_record:")
merged_clean |>
  summarise(
    .by = has_nsc_record,
    enroll_na = sum(is.na(enroll_seamless)),
    reten_1y_na = sum(is.na(reten_1y)),
    pers_1y_na = sum(is.na(pers_1y)),
    pers_stem_na = sum(is.na(pers_1y_stem)),
    deg_any_na = sum(is.na(deg_any_ever)),
    deg_bach_na = sum(is.na(deg_bach_ever)),
    deg_any_6y_na = sum(is.na(deg_any_6y)),
    deg_bach_6y_na = sum(is.na(deg_bach_6y))
  ) |>
  arrange(has_nsc_record) |>
  print()

# -----------------------------------------------------------------------------
# CHECK 4: Degree/persistence NAs by cohort for enrolled students
# 2018–2022 should be 0 for degree; 2018–2021 for reten/pers; 2023 NA by design
# -----------------------------------------------------------------------------
message("CHECK 4 — Degree/persistence NAs for enrolled students by cohort:")
merged_clean |>
  filter(enroll_ever == 1) |>
  summarise(
    .by = hs_grad_year,
    n = n(),
    deg_any_na = sum(is.na(deg_any_ever)),
    deg_bach_na = sum(is.na(deg_bach_ever)),
    deg_any_6y_na = sum(is.na(deg_any_6y)),
    deg_bach_6y_na = sum(is.na(deg_bach_6y)),
    reten_1y_na = sum(is.na(reten_1y)),
    pers_1y_na = sum(is.na(pers_1y)),
    pers_stem_na = sum(is.na(pers_1y_stem))
  ) |>
  arrange(hs_grad_year) |>
  print()

# -----------------------------------------------------------------------------
# CHECK 5: ITT mean < NSC-conditional mean
# -----------------------------------------------------------------------------
message("CHECK 5 — ITT vs NSC-conditional means (ITT should be lower):")
merged_clean |>
  summarise(
    enroll_seamless_itt = round(mean(enroll_seamless_itt, na.rm = TRUE), 3),
    enroll_seamless_nsc = round(mean(enroll_seamless, na.rm = TRUE), 3),
    enroll_ever_itt = round(mean(enroll_ever_itt, na.rm = TRUE), 3),
    enroll_ever_nsc = round(mean(enroll_ever, na.rm = TRUE), 3),
    enroll_seamless_stem_itt = round(
      mean(enroll_seamless_stem_itt, na.rm = TRUE),
      3
    ),
    enroll_seamless_stem_nsc = round(
      mean(enroll_seamless_stem, na.rm = TRUE),
      3
    ),
    enroll_ever_stem_itt = round(mean(enroll_ever_stem_itt, na.rm = TRUE), 3),
    enroll_ever_stem_nsc = round(mean(enroll_ever_stem, na.rm = TRUE), 3)
  ) |>
  tidyr::pivot_longer(everything(), names_to = "outcome", values_to = "mean") |>
  print()

# -----------------------------------------------------------------------------
# CHECK 6: Logical consistency — enroll_ever >= enroll_seamless (both versions)
# -----------------------------------------------------------------------------
n_inconsistent <- merged_clean |>
  filter(!is.na(enroll_seamless), !is.na(enroll_ever)) |>
  filter(enroll_seamless == 1 & enroll_ever == 0) |>
  nrow()

n_inconsistent_stem <- merged_clean |>
  filter(!is.na(enroll_seamless_stem), !is.na(enroll_ever_stem)) |>
  filter(enroll_seamless_stem == 1 & enroll_ever_stem == 0) |>
  nrow()

message(
  "CHECK 6 — enroll_seamless == 1 but enroll_ever == 0 (should be 0): ",
  n_inconsistent
)
message(
  "CHECK 6 — enroll_seamless_stem == 1 but enroll_ever_stem == 0 (should be 0): ",
  n_inconsistent_stem
)

# -----------------------------------------------------------------------------
# CHECK 7: Sample sizes by outcome set
# -----------------------------------------------------------------------------
message("CHECK 7 — Sample sizes by outcome set:")
tibble(
  outcome_set = c(
    "Full analytic sample",
    "ITT enrollment (2018–2023)",
    "NSC-matched (has_nsc_record == 1)",
    "NSC-enrolled (enroll_ever == 1)",
    "Degree outcomes (enrolled, 2018–2022)",
    "Persistence/retention (enrolled, 2018–2021)"
  ),
  n = c(
    nrow(merged_clean),
    merged_clean |> filter(hs_grad_year <= 2023) |> nrow(),
    merged_clean |> filter(has_nsc_record == 1) |> nrow(),
    merged_clean |> filter(enroll_ever == 1) |> nrow(),
    merged_clean |> filter(enroll_ever == 1, hs_grad_year <= 2022) |> nrow(),
    merged_clean |> filter(enroll_ever == 1, hs_grad_year <= 2021) |> nrow()
  )
) |>
  print()

# -----------------------------------------------------------------------------
# CHECK 8: reten/pers — no non-enrollees coded 0 (should all be NA)
# -----------------------------------------------------------------------------
message(
  "CHECK 8 — reten_1y/pers_1y/pers_1y_stem for non-enrollees (all should be NA):"
)
merged_clean |>
  filter(has_nsc_record == 1, enroll_ever == 0, hs_grad_year <= 2021) |>
  summarise(
    n = n(),
    reten_0 = sum(reten_1y == 0, na.rm = TRUE),
    pers_0 = sum(pers_1y == 0, na.rm = TRUE),
    pers_stem_0 = sum(pers_1y_stem == 0, na.rm = TRUE),
    reten_na = sum(is.na(reten_1y)),
    pers_na = sum(is.na(pers_1y)),
    pers_stem_na = sum(is.na(pers_1y_stem))
  ) |>
  print()

# -----------------------------------------------------------------------------
# CHECK 9: deg_any_6y >= deg_bach_6y (any degree must be >= bach degree)
# -----------------------------------------------------------------------------
message("CHECK 9 — deg_any_6y >= deg_bach_6y for all enrolled students:")
n_violated <- merged_clean |>
  filter(enroll_ever == 1, hs_grad_year <= 2022) |>
  filter(!is.na(deg_any_6y), !is.na(deg_bach_6y)) |>
  filter(deg_any_6y < deg_bach_6y) |>
  nrow()
message("  Violations (should be 0): ", n_violated)

# -----------------------------------------------------------------------------
# CHECK 10: geographic_location — mutually exclusive, sum to 1
# -----------------------------------------------------------------------------
message(
  "CHECK 10 — urban/suburban/rural mutually exclusive (geo_sum should always be 1):"
)
merged_clean |>
  filter(!is.na(urban)) |>
  mutate(geo_sum = urban + suburban + rural) |>
  count(geo_sum) |>
  print()

# -----------------------------------------------------------------------------
# CHECK 11: institution type sanity — pub + priv should equal 4yr
# -----------------------------------------------------------------------------
message("CHECK 11 — Institution type means and pub+priv == 4yr check:")
merged_clean |>
  filter(has_nsc_record == 1) |>
  summarise(
    enroll_delayed_mean = round(mean(enroll_delayed, na.rm = TRUE), 3),
    inst_private4yr_mean = round(mean(inst_private4yr_entry, na.rm = TRUE), 3),
    inst_instate_mean = round(mean(inst_instate_entry, na.rm = TRUE), 3),
    inst_public4yr_mean = round(mean(inst_public4yr_entry, na.rm = TRUE), 3),
    inst_4yr_mean = round(mean(inst_4yr_entry, na.rm = TRUE), 3)
  ) |>
  print()

n_pub_priv_mismatch <- merged_clean |>
  filter(has_nsc_record == 1, !is.na(inst_4yr_entry)) |>
  mutate(pub_priv_sum = inst_public4yr_entry + inst_private4yr_entry) |>
  filter(pub_priv_sum != inst_4yr_entry) |>
  nrow()
message("  pub + priv != inst_4yr_entry (should be 0): ", n_pub_priv_mismatch)

# -----------------------------------------------------------------------------
# CHECK 12: STEM outcomes — means and logical consistency
# enroll_ever_stem <= enroll_ever (can't be in STEM without being enrolled)
# pers_1y_stem <= pers_1y (can't persist in STEM without persisting generally)
# -----------------------------------------------------------------------------
message("CHECK 12 — STEM outcome means and logical consistency:")
merged_clean |>
  filter(has_nsc_record == 1) |>
  summarise(
    enroll_seamless_stem_mean = round(
      mean(enroll_seamless_stem, na.rm = TRUE),
      3
    ),
    enroll_ever_stem_mean = round(mean(enroll_ever_stem, na.rm = TRUE), 3),
    pers_1y_stem_mean = round(mean(pers_1y_stem, na.rm = TRUE), 3),
    pers_1y_mean = round(mean(pers_1y, na.rm = TRUE), 3)
  ) |>
  print()

n_stem_gt_enroll <- merged_clean |>
  filter(!is.na(enroll_ever), !is.na(enroll_ever_stem)) |>
  filter(enroll_ever_stem > enroll_ever) |>
  nrow()

n_stem_pers_gt_pers <- merged_clean |>
  filter(!is.na(pers_1y), !is.na(pers_1y_stem)) |>
  filter(pers_1y_stem > pers_1y) |>
  nrow()

message("  enroll_ever_stem > enroll_ever (should be 0): ", n_stem_gt_enroll)
message("  pers_1y_stem > pers_1y (should be 0): ", n_stem_pers_gt_pers)

# =============================================================================
# CLEAN UP
# =============================================================================

rm(
  list = setdiff(
    ls(),
    c(
      "merged_clean",
      "merged_clean_n",
      "merged_df",
      "merged_n",
      "alum",
      "alum_n",
      "applicants",
      "applicant_n"
    )
  )
)
