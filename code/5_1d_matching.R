# =============================================================================
# 5_1d_matching.R
#
# Purpose: Propensity score matching (PSM) for two samples:
#            1. All states (merged_df_all)
#            2. PA public schools (merged_df_pa) — adds school-level covariates
#
# Method:  Nearest-neighbor matching with replacement (caliper = 0.25 SD),
#          logistic propensity score, exact match on application year.
#          Missing covariates are imputed to 0; missing indicators are added.
#
# Input:   `merged_df_all` — all-states cleaned dataset (from script 4)
#          `merged_df_pa`  — PA public school dataset (from script 4)
# Output:  data/matched/matched_all_states_year_only.rds
#          data/matched/matched_pa_year_only.rds
#          data/matched/matchit_object_all_states.rds
#          data/matched/matchit_object_pa.rds
#          data/matched/matching_data_all_states.rds
#          data/matched/matching_data_pa.rds
# =============================================================================

library(dplyr)
library(tidyr)
library(MatchIt)
library(cobalt)
library(here)

# =============================================================================
# COVARIATES
# =============================================================================
# `base_covariates` are shared by both samples. The all-states PS adds
# `pa_state` (selection into Hillman is geographically concentrated in PA);
# the PA PS adds school-level covariates instead. `pa_state` would be a
# constant in the PA sample so it's excluded there.
# house_size included as SES proxy alongside stipend.
# gender cast to integer here — sourced as dbl from applicant clean script.

base_covariates <- c(
  "gender",
  "gpa",
  "psat_math",
  "stipend",
  "house_size",
  "racially_marginalized",
  "bi_multi_racial",
  "urban",
  "suburban",
  "rural",
  "disability",
  "neg_school",
  "us_citizen"
  # first_gen excluded: question wasn't on the 2017 or 2018 application
  # forms, so it's 100% missing for those cohorts and the missing-indicator
  # method produces first_gen_miss == 1[year %in% c(2017, 2018)] — perfectly
  # collinear with year FE in the outcome model and aliased in the PS logit.
  # Among 2019–2021 cohorts where it was measured, only 1 of 86 students
  # is first-generation, so within-sample variation is also negligible.
)

all_states_extra_covariates <- c("pa_state")

pa_extra_covariates <- c(
  "school_enrollment",
  "school_pct_econ_disadvantaged",
  "school_pct_english_learner",
  "school_pct_special_ed",
  "school_pct_white"
  # school_title_i excluded — high collinearity with school_pct_econ_disadvantaged
)

# Per-sample full covariate vectors used by the PS model and balance tables.
all_states_covariates <- c(base_covariates, all_states_extra_covariates)
pa_covariates         <- c(base_covariates, pa_extra_covariates)

# =============================================================================
# HELPER: PREPARE MATCHING DATA
# =============================================================================
# Applies exclusions (before imputation), casts gender to integer, adds grade
# dummies, builds _miss indicators, then imputes remaining NAs to 0.
#
# Exclusions applied on true values before imputation:
#   - treated_before_2017 == 1: would contaminate control group
#   - us_citizen == 0 (confirmed non-citizen): NA citizenship retained and
#     handled via us_citizen_miss in the PS model
#   - year == 2022: redundant under script 3b's hs_grad_year ≤ 2021 cap
#     (a 2022 applicant graduates ≥ 2022 and is already filtered upstream);
#     kept defensively here in case the upstream cap is loosened.
#   - missing treated_in_year: data anomaly

prepare_matching_data <- function(data, covariates, exclude_years = NULL) {
  data <- data |>
    mutate(
      gender = as.integer(gender),
      grade_9 = if_else(grade == 9, 1L, 0L),
      grade_10 = if_else(grade == 10, 1L, 0L),
      grade_11 = if_else(grade == 11, 1L, 0L),
      grade_12 = if_else(grade == 12, 1L, 0L),
      # PA-residence indicator — selection into Hillman is geographically
      # concentrated in PA. Only used in the all-states PS model; constant
      # within the PA sample so excluded from pa_covariates.
      pa_state = if_else(state == "pennsylvania", 1L, 0L, missing = NA_integer_)
    )

  data <- data |>
    filter(
      !is.na(treated_in_year),
      !year %in% c(2022, exclude_years),
      us_citizen != 0 | is.na(us_citizen),
      treated_before_2017 == 0
    )

  data <- data |>
    mutate(
      across(
        all_of(covariates),
        list(miss = ~ if_else(is.na(.), 1L, 0L)),
        .names = "{col}_miss"
      )
    ) |>
    mutate(across(all_of(covariates), ~ replace_na(., 0)))

  data
}

# =============================================================================
# HELPER: RUN MATCHING
# =============================================================================
# A _miss indicator is included in the PS formula whenever the underlying
# covariate has any missingness in the matching sample. Indicators with 0%
# missingness are dropped because an all-zero column is collinear with the
# intercept and adds no information.

run_matching <- function(data, covariates) {
  miss_counts <- vapply(
    paste0(covariates, "_miss"),
    function(v) sum(data[[v]]),
    integer(1)
  )
  miss_vars <- paste0(covariates[miss_counts > 0], "_miss")

  # grade_11 (modal grade, ~60% of pool) is the reference category — including
  # all four grade dummies plus the implicit intercept makes the design matrix
  # rank-deficient and glm() drops one arbitrarily.
  message(
    "  PS model: ",
    length(covariates) + length(miss_vars) + 3,
    " terms (3 grade dummies + ",
    length(covariates),
    " covars + ",
    length(miss_vars),
    " _miss)"
  )
  if (length(miss_vars) < length(covariates)) {
    dropped <- setdiff(paste0(covariates, "_miss"), miss_vars)
    message(
      "  Dropped _miss indicators (no missingness): ",
      paste(sub("_miss$", "", dropped), collapse = ", ")
    )
  }

  ps_formula <- reformulate(
    termlabels = c(
      "grade_9",
      "grade_10",
      "grade_12",
      covariates,
      miss_vars
    ),
    response = "treated_in_year"
  )

  matchit(
    ps_formula,
    data = data,
    method = "nearest",
    exact = ~year,
    distance = "glm",
    caliper = 0.25,         # 0.25 pooled SDs of the propensity score
                            # (Austin 2011, Stuart 2010 standard)
    caliper.d = "pooled SD",
    replace = TRUE
  )
}

# =============================================================================
# HELPER: COMMON-SUPPORT DIAGNOSTIC
# =============================================================================
# Reports the propensity score distribution by treatment status and any units
# that fall outside the overlap region. Reviewers expect this for any PSM paper.

report_common_support <- function(m_out, sample_label) {
  ps <- m_out$distance
  trt <- m_out$treat == 1

  ps_t <- ps[trt]
  ps_c <- ps[!trt]

  message(
    "\n  Common support (", sample_label, "):",
    "\n    Treated PS range:  [", sprintf("%.3f, %.3f", min(ps_t), max(ps_t)), "]",
    "\n    Control PS range:  [", sprintf("%.3f, %.3f", min(ps_c), max(ps_c)), "]",
    "\n    Treated below min(control PS): ", sum(ps_t < min(ps_c)),
    "\n    Treated above max(control PS): ", sum(ps_t > max(ps_c)),
    "\n    Control below min(treated PS): ", sum(ps_c < min(ps_t)),
    "\n    Control above max(treated PS): ", sum(ps_c > max(ps_t))
  )

  # m_out$caliper stores the absolute caliper width (caliper * sd(ps) already
  # applied). Recover the standardized value as caliper_abs / sd(ps).
  caliper_abs <- as.numeric(m_out$caliper)
  caliper_std <- caliper_abs / sd(ps)
  message(
    "    Caliper (", sprintf("%.2f", caliper_std), " SD = ",
    sprintf("%.3f", caliper_abs),
    " on PS scale) eliminated ",
    sum(m_out$weights == 0 & trt),
    " treated unit(s)"
  )
}

# =============================================================================
# 1. ALL-STATES MATCH
# =============================================================================

message("\n=== ALL-STATES MATCHING ===\n")

# Reproducibility: nearest-neighbor matching with replacement is mostly
# deterministic, but ties in propensity score are broken by row order. A seed
# locks the matched set against any future upstream reordering.
set.seed(20260428)

# All-states exclusions:
#   2021 — only 1 treated student appears in the analytic sample (after the
#          hs_grad ≤ 2021 cap, only application-year 2021 grade-12 students
#          survive, and the alumni tracker has just 1 such treated student).
#          The resulting n=2 matched cell (1T/1C) breaks HC3 SEs in the
#          outcome model (leverage = 1) and contributes ~0 information to the
#          ATT. Excluded for analytic stability.
matching_data_all <- prepare_matching_data(
  merged_df_all,
  all_states_covariates,
  exclude_years = 2021
)

message(
  "Matching sample: ",
  nrow(matching_data_all),
  " students (",
  sum(matching_data_all$treated_in_year == 1),
  " treated / ",
  sum(matching_data_all$treated_in_year == 0),
  " control)"
)

m.out_all <- run_matching(matching_data_all, all_states_covariates)

report_common_support(m.out_all, "All states")

summary(m.out_all)
bal.tab(m.out_all, un = TRUE, thresholds = c(m = 0.1))
bal.tab(m.out_all, cluster = "year", un = TRUE, thresholds = c(m = 0.1))

matched_data_all <- match.data(m.out_all)

message(
  "Matched sample (all states): ",
  nrow(matched_data_all),
  " students (",
  sum(matched_data_all$treated_in_year == 1),
  " treated / ",
  sum(matched_data_all$treated_in_year == 0),
  " control)"
)

# =============================================================================
# 2. PA PUBLIC SCHOOL MATCH
# =============================================================================
# Adds school-level covariates (enrollment, % econ. disadvantaged, etc.) to
# the PS model. school_title_i excluded due to high collinearity with
# school_pct_econ_disadvantaged — its information is largely captured there.

message("\n=== PA PUBLIC SCHOOL MATCHING ===\n")

set.seed(20260428)

# PA exclusions:
#   2021 — 0 PA treated students with HS grad <= 2021 (after the cohort cap
#          in script 3b, application year 2021 corresponds to HS-grad-2021
#          seniors, of whom none in PA were treated). Exact-on-year matching
#          fails without both treatment levels in every year cluster.
#   2023 — only 1 treated student, insufficient for matching.
matching_data_pa <- prepare_matching_data(
  merged_df_pa,
  pa_covariates,
  exclude_years = c(2021, 2023)
)

message(
  "Matching sample (PA): ",
  nrow(matching_data_pa),
  " students (",
  sum(matching_data_pa$treated_in_year == 1),
  " treated / ",
  sum(matching_data_pa$treated_in_year == 0),
  " control)"
)

m.out_pa <- run_matching(matching_data_pa, pa_covariates)

report_common_support(m.out_pa, "PA public schools")

summary(m.out_pa)
bal.tab(m.out_pa, un = TRUE, thresholds = c(m = 0.1))
bal.tab(m.out_pa, cluster = "year", un = TRUE, thresholds = c(m = 0.1))

matched_data_pa <- match.data(m.out_pa)

message(
  "Matched sample (PA): ",
  nrow(matched_data_pa),
  " students (",
  sum(matched_data_pa$treated_in_year == 1),
  " treated / ",
  sum(matched_data_pa$treated_in_year == 0),
  " control)"
)

# =============================================================================
# 3. SAVE OUTPUTS
# =============================================================================

dir.create(here("data", "matched"), recursive = TRUE, showWarnings = FALSE)

# Matched datasets (used by scripts 7 and 8)
saveRDS(matched_data_all, here("data", "matched", "matched_all_states_year_only.rds"))
saveRDS(matched_data_pa,  here("data", "matched", "matched_pa_year_only.rds"))

# MatchIt objects (used by script 8 for balance tables + Love plot)
saveRDS(m.out_all, here("data", "matched", "matchit_object_all_states.rds"))
saveRDS(m.out_pa,  here("data", "matched", "matchit_object_pa.rds"))

# Pre-match analytic samples (used by script 8 for descriptive table + A2)
saveRDS(matching_data_all, here("data", "matched", "matching_data_all_states.rds"))
saveRDS(matching_data_pa,  here("data", "matched", "matching_data_pa.rds"))

message("\n=== Matching complete ===")
message("Saved: matched_all_states_year_only.rds (", nrow(matched_data_all), " rows)")
message("Saved: matched_pa_year_only.rds (", nrow(matched_data_pa), " rows)")
message("Saved: matchit_object_all_states.rds")
message("Saved: matchit_object_pa.rds")
message("Saved: matching_data_all_states.rds (", nrow(matching_data_all), " rows)")
message("Saved: matching_data_pa.rds (", nrow(matching_data_pa), " rows)")
# =============================================================================
