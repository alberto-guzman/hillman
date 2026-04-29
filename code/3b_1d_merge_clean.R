# =============================================================================
# 3b_1d_merge_clean.R
#
# Purpose: Standardize covariates in the merged applicant/alumni dataset,
#          merge NSC college outcome data, and restrict to the analytic sample.
#
# Outcome strategy:
#   Analytic cohorts: HS grad years 2018–2021 only. The 2022 and 2023 cohorts
#     are excluded because the NSC query that produced clean_Hillman_2017_2025
#     returned no enrollment records for 2023 grads and only partial coverage
#     for 2022 controls (~54% seamless vs ~80% for prior cohorts), so any
#     reported rates would understate true outcomes.
#   ITT enrollment outcomes  — all students in analytic sample; students with
#     no NSC record OR enroll_seamless == 0 both coded as non-enrolled (0).
#   NSC-conditional outcomes — students with an NSC record only (has_nsc_record
#     == 1). Post-enrollment outcomes (persistence, degree) further conditioned
#     on enroll_ever == 1. Non-attainers recoded NA → 0 within sample.
#     Note: degree-by-year-window outcomes are right-censored for cohorts with
#     less than the full window of follow-up (e.g., 7y bach for 2019–2021
#     grads). Treat these as "earned within observed years," not within the
#     full nominal window.
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
# Reported outcomes (consolidated set used in scripts 7 and 8):
#   Panel A — enroll_seamless, enroll_seamless_stem
#             (raw NSC outcomes; conditioned on has_nsc_record == 1)
#   Panel B — inst_4yr_entry, inst_2yr_entry
#             (conditioned on has_nsc_record == 1)
#   Panel C — pers_1y, pers_1y_stem, deg_bach_6y, deg_any_stem_6y
#             (conditioned on enroll_ever == 1)
#
# Other NSC outcomes (enroll_ever, enroll_delayed, enroll_firsttime_fulltime,
# inst_public4yr_entry, inst_private4yr_entry, inst_instate_entry, reten_1y,
# deg_any_ever, deg_bach_ever, deg_any_6y) are carried in merged_clean for
# diagnostics and sensitivity work but are not included in the published
# panels. ITT versions (enroll_*_itt) are also retained for reference.
#
# NOTE: deg_any_4y dropped — identical to deg_any_6y in the raw NSC file
#       (degree_4years_all_nsc == degree_6years_all_nsc for all cohorts).
#       deg_bach_4y dropped — identical to deg_bach_6y in the matched 2018–2021
#       sample (no bachelor's earned in years 5-7).
#       deg_bach_6y is actually a 7-year window: Danielle's loop range is 0/6
#       (inclusive of year 0). Original variable name retained; script 8's
#       table header reads "within 7 years".
#       pers_1y_stem: persistence in a STEM major at any institution next fall;
#       NOT conditional on STEM entry. See 4c PSE persistence.do.
#
# Input:   `merged_df` — one row per student (from script 3a)
#          data/files_for_danielle_nsc/clean_Hillman_2017_2025.dta — NSC data
# Output:  `merged_clean`   — analysis-ready dataset
#          `merged_clean_n` — applicant and alumni counts by year
#          output/counts/n_merged_clean_by_year.csv
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(haven)
library(janitor)
library(here)

dir.create(here("output", "counts"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# STANDARDIZE COVARIATES
# =============================================================================
merged_clean <- merged_df |>
  mutate(
    # Stipend: NA → 0 (not eligible) per data dictionary. Keyword set covers
    # all observed positive-response values (Y/1/Yes/Eligible/Stipend Eligible/
    # Yes Student); anything else (incl. "No" and NA) → 0.
    stipend = case_when(
      str_detect(
        stipend,
        regex("\\b(yes|y|1|eligible|stipend eligible|yes student)\\b",
              ignore_case = TRUE)
      ) ~ 1L,
      TRUE ~ 0L
    ),

    first_gen = case_when(
      str_detect(tolower(as.character(first_gen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(first_gen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Racially marginalized: Black, African, Latino/Hispanic, Native American /
    # American Indian, Pacific Islander. "indian" alone is intentionally
    # excluded — that keyword would also match Asian Indian, who are not
    # classified as marginalized under the framework used in this study.
    racially_marginalized = case_when(
      str_detect(
        tolower(self_identity),
        "black|african|latino|latinx|hispanic|native|american indian|pacific"
      ) ~ 1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),
    # bi_multi_racial only catches literal "multi", "bi-racial", "biracial",
    # or "two or more". Students who write multiple races separated by
    # commas/slashes (e.g., "Black/Hispanic") are NOT detected here — this
    # is a known limitation of free-text race fields and is consistent with
    # how the EEPA paper classifies these responses.
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
      str_detect(tolower(geographic_location), "^\\s*urban") ~ "Urban",
      str_detect(tolower(geographic_location), "^\\s*suburban") ~ "Suburban",
      str_detect(tolower(geographic_location), "^\\s*rural") ~ "Rural/Small Town",
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

    # disability / neg_school logic:
    #   "Do not wish to answer", "N/A", "Not applicable" → NA (informative
    #     non-response; ~3-5% of applicants — coding as 1 would inflate the
    #     "has disability / negative school env" rate).
    #   "No...", "None..." → 0
    #   any other non-NA, non-decline → 1 (free-text describing the condition)
    disability = case_when(
      str_detect(
        documented_disability,
        regex("^\\s*(do not wish|n/?a|not applicable)", ignore_case = TRUE)
      ) ~ NA_integer_,
      str_detect(
        documented_disability,
        regex("^\\s*(no|none)\\b", ignore_case = TRUE)
      ) ~ 0L,
      !is.na(documented_disability) ~ 1L,
      TRUE ~ NA_integer_
    ),
    neg_school = case_when(
      str_detect(
        school_impact,
        regex("^\\s*(do not wish|n/?a|not applicable)", ignore_case = TRUE)
      ) ~ NA_integer_,
      str_detect(
        school_impact,
        regex("^\\s*(no|none)\\b", ignore_case = TRUE)
      ) ~ 0L,
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
# Load once into `outcomes_raw` (preserved for CHECK 1, which references the
# pre-rename column names) and produce the analysis-ready `outcomes` from it.
# All integer-coded outcome columns are cast to plain integer at load so the
# rest of the pipeline does not need inline `as.integer()` calls.

outcomes_raw <- read_dta(here(
  "data/files_for_danielle_nsc",
  "clean_Hillman_2017_2025.dta"
)) |>
  clean_names() |>
  rename(
    first_name = firstname,
    last_name = lastname
  )

outcomes <- outcomes_raw |>
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
    inst_sector_initial_nsc,    # raw 1–9 sector code, used to derive inst_2yr_entry
    instate_initial_nsc,
    # retention & persistence
    reten_fall_enter,
    pers_fall_enter,
    pers_fall_enter_stem,
    # degree — ever
    degree_ever_nsc,
    bach_ever_nsc,
    # degree — by year window
    # NOTE 1: degree_4years_all_nsc confirmed identical to degree_6years_all_nsc
    #         for all cohorts — only the 6yr variable is retained.
    # NOTE 2: Danielle's loop range in 4b PSE degree attainment new.do uses 0/6
    #         (inclusive of year 0), so bachdegree_6years_all_nsc is actually
    #         "Bachelor's within 7 years". The original variable name is
    #         preserved; script 8's table header reflects the actual span.
    #         The 5-year window (bachdegree_4years_all_nsc) was identical to
    #         the 7-year window in the matched sample for current cohorts and
    #         was dropped from the consolidated outcome set.
    degree_6years_all_nsc,
    bachdegree_6years_all_nsc,
    # STEM degree (any) within 6 years post-HS — janitor mangles "STEM" to "ste_m"
    ste_mdegree_in6_grad
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
    deg_bach_6y = bachdegree_6years_all_nsc,
    deg_any_stem_6y = ste_mdegree_in6_grad
  ) |>
  mutate(
    across(
      -c(first_name, last_name, hs_grad_year, inst_sector_initial_nsc),
      as.integer
    ),
    # 2-year college entry = sectors 4 (Pub 2yr), 5 (PrNP 2yr), 6 (PrFP 2yr).
    # Never-enrolled students have inst_sector_initial_nsc = NA; %in% returns
    # FALSE, so they are coded 0 — matching how inst_4yr_entry treats them
    # (Stata's `==` returns 0 when comparing to missing).
    # Cast to integer first because haven::read_dta() can return labelled
    # types where %in% would compare against labels rather than codes.
    inst_2yr_entry = as.integer(
      as.integer(inst_sector_initial_nsc) %in% c(4L, 5L, 6L)
    )
  ) |>
  select(-inst_sector_initial_nsc)

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

# Join on names only and use the .dta's hs_grad_year as authoritative when
# present. Our hs_grad_year is computed from grade at application time
# (year + 12 - grade); Danielle's is sourced from registrar/NSC records
# and reflects actual graduation, accounting for held-back/skipped grades.
# Joining on the 3-key (first_name, last_name, hs_grad_year) silently
# drops 3+ in-sample students whose grade-derived hs_grad_year disagrees.
if (any(duplicated(outcomes[, c("first_name", "last_name")]))) {
  stop(
    "Duplicate (first_name, last_name) pairs remain in NSC outcomes after ",
    "deduplication — investigate the dedup step before joining."
  )
}

merged_clean <- merged_clean |>
  left_join(
    outcomes |> rename(hs_grad_year_nsc = hs_grad_year),
    by = c("first_name", "last_name"),
    relationship = "many-to-one"
  ) |>
  mutate(hs_grad_year = coalesce(hs_grad_year_nsc, hs_grad_year)) |>
  select(-hs_grad_year_nsc)

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
# ITT ENROLLMENT OUTCOMES (all students in analytic sample)
# =============================================================================
# No NSC record AND enroll_seamless == 0 both treated as non-enrolled (0).
# STEM ITT outcomes follow same logic: no NSC record OR not in STEM = 0.
# These are kept for reference; the published panels use raw NSC outcomes
# conditioned on has_nsc_record == 1 (per PI input — students missing from
# NSC went to college per Hillman records, just at non-reporting institutions).

merged_clean <- merged_clean |>
  mutate(
    enroll_seamless_itt      = replace_na(enroll_seamless,      0L),
    enroll_ever_itt          = replace_na(enroll_ever,          0L),
    enroll_seamless_stem_itt = replace_na(enroll_seamless_stem, 0L),
    enroll_ever_stem_itt     = replace_na(enroll_ever_stem,     0L)
  )

# =============================================================================
# FILTER TO ANALYTIC SAMPLE (graduation cohorts 2018–2021)
# =============================================================================
# 2017: n=18 students; seamless-STEM rate is 22% vs 41–52% for 2018–2021,
#       likely small-N noise or measurement degradation in older NSC vintages.
#       Excluded out of conservatism on a headline outcome.
# 2022: NSC query has only ~57% control seamless rate (vs ~80% prior cohorts);
#       partial NSC coverage — excluded to avoid biased denominators.
# 2023+: NSC query returned 0% enrollment — no data, excluded.

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
        (hs_grad_year < 2018 | hs_grad_year > 2021)
    )
  )

merged_clean <- merged_clean |>
  filter(
    !is.na(hs_grad_year),
    !is.na(grade),
    grade >= 9,
    grade <= 12,
    hs_grad_year >= 2018,
    hs_grad_year <= 2021
  )

message(
  "Cohort filter removed ",
  pre_filter_n - nrow(merged_clean),
  " observations:"
)
message("  Missing hs_grad_year: ", removal_stats$missing_grad_year)
message("  Missing grade:        ", removal_stats$missing_grade)
message("  Invalid grade:        ", removal_stats$invalid_grade)
message("  Outside 2018–2021:    ", removal_stats$outside_range)
message("Remaining: ", nrow(merged_clean))

# =============================================================================
# NSC-CONDITIONAL OUTCOMES — recode NA → 0 within sample
# =============================================================================
# Retention/persistence and degree outcomes are conditional on enroll_ever == 1
# (you cannot persist into year 2 or earn a degree if you never started). The
# analytic sample is already capped at HS-grad cohorts 2018–2021, so no
# additional cohort gating is needed here.
#
# pers_1y_stem: any fall entrant — measures whether the student is in a STEM
#   major at any institution next fall. NOT conditional on entering as STEM.
# Degree windows: deg_bach_6y is right-censored for cohorts younger than the
#   nominal 7 years (e.g., 2021 grads have only 4 years observed). Treat as
#   "earned within observed years" rather than imputing NA.
# Danielle's .do file does NOT set a 0 baseline for bach_ever / degree-by-year
#   variables — only positive cases are recorded. We recode NA → 0 here for
#   enrolled students so non-graduates are correctly coded 0 within the sample.

merged_clean <- merged_clean |>
  mutate(
    # -- retention/persistence and degree outcomes are only meaningful for
    #    enrolled students. Force NA for non-enrolled rows so any stale zeros
    #    from the raw NSC file cannot leak into the analytic data.
    #    All cohorts in the analytic sample (2018–2021) have ≥ 1 year of
    #    fall-after follow-up, so no further cohort gating is needed for
    #    persistence. Degree outcomes are right-censored for cohorts younger
    #    than the nominal window (e.g., 2021 grads have only 4 years of
    #    follow-up for the 7-year bachelor's window) — treat as observed-
    #    within-window rather than imputing NA.
    across(
      c(reten_1y, pers_1y, pers_1y_stem,
        deg_any_ever, deg_bach_ever, deg_any_6y,
        deg_bach_6y, deg_any_stem_6y),
      ~ case_when(
        is.na(enroll_ever) | enroll_ever != 1L ~ NA_integer_,
        is.na(.x)                              ~ 0L,
        TRUE                                   ~ .x
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

# Invariant: enroll_ever == 1 implies has_nsc_record == 1.
# Downstream Panel C analysis (script 7) reads from the unfiltered matched
# sample and conditions only on enroll_ever == 1 — relying on this invariant
# rather than re-asserting has_nsc_record. Guard it here so any future change
# to the NSC join or recoding logic surfaces immediately.
stopifnot(
  all(merged_clean$has_nsc_record == 1 |
        is.na(merged_clean$enroll_ever) |
        merged_clean$enroll_ever != 1L)
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
    # ITT ENROLLMENT OUTCOMES (all students in analytic sample, 2018–2021)
    # NA → 0: non-NSC-matched and non-enrolled both coded 0.
    # Retained for reference; published panels use raw NSC outcomes.
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
    inst_2yr_entry,
    inst_instate_entry,

    # -- retention: same institution, year 1 (enroll_ever == 1)
    reten_1y,

    # -- persistence: any institution, year 1 (enroll_ever == 1)
    # pers_1y_stem: in a STEM major at any institution next fall
    pers_1y,
    pers_1y_stem,

    # -- degree attainment: ever (enroll_ever == 1)
    deg_any_ever,
    deg_bach_ever,

    # -- degree attainment: by year window (enroll_ever == 1)
    # deg_any_4y dropped: identical to deg_any_6y in raw NSC file.
    # deg_bach_4y dropped: identical to deg_bach_6y in matched sample for
    # current cohorts (no bachelor's earned in years 5-7).
    # NOTE: deg_bach_6y is actually a 7-year window (Danielle's loop range is
    # 0/6, inclusive of year 0). Original variable name retained; script 8's
    # table header reads "within 7 years".
    deg_any_6y,
    deg_bach_6y,
    deg_any_stem_6y
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
# Expected: small n, all delayed enrollees. Uses outcomes_raw (pre-rename).
# -----------------------------------------------------------------------------
nsc_anomalies <- outcomes_raw |>
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
rm(nsc_anomalies)

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
# CHECK 3: NSC-conditional NAs only for has_nsc_record == 0 and for
# students with enroll_ever != 1 (post-enrollment outcomes are NA there).
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
# CHECK 4: Degree/persistence NAs by cohort for enrolled students.
# Analytic sample is 2018–2021; all rows here should have non-NA outcomes
# (enroll_ever == 1 ensures the conditional-NA recoding fired).
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
    "Full analytic sample (2018–2021)",
    "NSC-matched (has_nsc_record == 1)",
    "NSC-enrolled (enroll_ever == 1)"
  ),
  n = c(
    nrow(merged_clean),
    merged_clean |> filter(has_nsc_record == 1) |> nrow(),
    merged_clean |> filter(enroll_ever == 1) |> nrow()
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
  filter(has_nsc_record == 1, enroll_ever == 0) |>
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
  filter(enroll_ever == 1) |>
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
