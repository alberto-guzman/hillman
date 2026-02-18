# OUTCOME ANALYSIS: Treatment Effects Following MatchIt Best Practices
# =============================================================================
# Reference: https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
# Estimand: ATT via linear probability model on matched data
# Model: lm() with matching weights
# SE approach: cluster-robust by subclass (matched pair) via avg_slopes()
#
# Sample splits:
#   - Enrollment outcomes: full matched sample (all cohorts)
#   - Degree outcomes: restricted to students with non-NA outcome (i.e.,
#     sufficient follow-up time to have completed a degree). NAs in the
#     degree variables already encode censoring from the data build step.
# =============================================================================

library(tidyverse)
library(marginaleffects)
library(sandwich)
library(lmtest)
library(here)

# =============================================================================
# 1. LOAD MATCHED DATASET
# =============================================================================

matched <- readRDS(here("data", "matched_all_states_year_only.rds"))

message(
  "Matched data: ",
  nrow(matched),
  " obs | ",
  sum(matched$treated_in_year == 1),
  " treated / ",
  sum(matched$treated_in_year == 0),
  " control"
)

# =============================================================================
# 2. DEFINE OUTCOMES AND COVARIATES
# =============================================================================

# Enrollment outcomes: no censoring concern, run on full sample
enrollment_outcomes <- c(
  "seamless_enroll",
  "seamless_enroll_stem",
  "enrolled_ever_nsc",
  "enrolled_ever_stem"
)

# Degree outcomes: restricted to students with sufficient follow-up (non-NA)
degree_outcomes <- c(
  "degree_ever_nsc",
  "degree_ever_stem_nsc"
)

outcome_labels <- c(
  seamless_enroll = "Seamless enrollment (any)",
  seamless_enroll_stem = "Seamless STEM enrollment",
  enrolled_ever_nsc = "Ever enrolled (any)",
  enrolled_ever_stem = "Ever enrolled STEM",
  degree_ever_nsc = "Ever earned degree (any)",
  degree_ever_stem_nsc = "Ever earned STEM degree"
)

# Covariates for doubly robust adjustment.
# Excluded:
#   - house_size, us_citizen: excluded from PS model for consistency
#   - first_gen: all zeros in matched data (constant)
#   - first_gen_miss: all ones in matched data (constant)
#   - All other _miss indicators that are all-zero in matched data:
#     gender_miss, stipend_miss, racially_marginalized_miss,
#     bi_multi_racial_miss, urban_miss, suburban_miss, rural_miss,
#     disability_miss, us_citizen_miss
# Retained _miss indicators with actual variation:
#   gpa_miss, psat_math_miss, neg_school_miss
covars <- c(
  "gender",
  "grade",
  "gpa",
  "psat_math",
  "stipend",
  "racially_marginalized",
  "bi_multi_racial",
  "urban",
  "suburban",
  "rural",
  "disability",
  "neg_school",
  "gpa_miss",
  "psat_math_miss",
  "neg_school_miss"
)


# =============================================================================
# 3. HELPER: FIT LPM + EXTRACT ATT VIA avg_slopes()
# =============================================================================

# Linear probability model (lm) with matching weights.
# MatchIt recommendation for 1:m matching with replacement:
#   - Use weights from matched data
#   - avg_slopes() with vcov = ~subclass gives cluster-robust SEs by matched
#     pair and correctly targets the ATT for the treated subsample

fit_att <- function(data, outcome_var, predictors) {
  formula_str <- paste0(
    outcome_var,
    " ~ treated_in_year + ",
    paste(predictors, collapse = " + "),
    " + factor(year)" # cohort FE; matches exact = ~year from matchit
  )

  fit <- lm(
    as.formula(formula_str),
    data = data,
    weights = weights
  )

  # avg_slopes: ATT restricted to treated units, cluster-robust SE by subclass
  treated_data <- subset(data, treated_in_year == 1)

  att <- avg_slopes(
    fit,
    variables = "treated_in_year",
    vcov = sandwich::vcovCL(fit, cluster = data$subclass),
    newdata = treated_data,
    wts = "weights"
  )

  # Weighted means
  wt_mean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)

  ctrl_mean <- wt_mean(
    data[[outcome_var]][data$treated_in_year == 0],
    data$weights[data$treated_in_year == 0]
  )
  trt_mean <- wt_mean(
    data[[outcome_var]][data$treated_in_year == 1],
    data$weights[data$treated_in_year == 1]
  )

  tibble(
    outcome = outcome_var,
    label = outcome_labels[outcome_var],
    sample = "full",
    n_obs = nrow(data),
    n_treated = sum(data$treated_in_year == 1),
    n_control = sum(data$treated_in_year == 0),
    ctrl_mean = ctrl_mean,
    trt_mean = trt_mean,
    raw_diff_pp = (trt_mean - ctrl_mean) * 100,
    att_pp = att$estimate * 100,
    se_pp = att$std.error * 100,
    pval = att$p.value,
    conf_lo_pp = att$conf.low * 100,
    conf_hi_pp = att$conf.high * 100
  )
}

# =============================================================================
# 4. ATT ESTIMATES
# =============================================================================

# --- Enrollment: full matched sample -----------------------------------------
message("\n--- Enrollment outcomes (full sample) ---")

results_enrollment <- enrollment_outcomes |>
  map(\(out) {
    message("  Fitting: ", out)
    fit_att(matched, out, covars)
  }) |>
  list_rbind()

# --- Degree: restrict to students with sufficient follow-up ------------------
# NAs in degree variables encode censoring (not enough time to earn a degree).
# We subset to non-NA for each outcome separately so each model uses as many
# observations as possible.
message("\n--- Degree outcomes (non-censored subsample) ---")

results_degree <- degree_outcomes |>
  map(\(out) {
    message("  Fitting: ", out)
    data_sub <- matched |> filter(!is.na(.data[[out]]))
    message(
      "    Sample after removing censored: ",
      nrow(data_sub),
      " obs (",
      sum(data_sub$treated_in_year == 1),
      " treated / ",
      sum(data_sub$treated_in_year == 0),
      " control)"
    )
    fit_att(data_sub, out, covars) |>
      mutate(sample = "degree_eligible")
  }) |>
  list_rbind()

# --- Combine -----------------------------------------------------------------
results <- bind_rows(results_enrollment, results_degree)

# =============================================================================
# 5. INSPECT RESULTS
# =============================================================================

results |>
  select(
    label,
    sample,
    n_treated,
    n_control,
    ctrl_mean,
    trt_mean,
    att_pp,
    se_pp,
    pval
  )

# =============================================================================
# 6. SAVE
# =============================================================================

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}

saveRDS(results, here("output", "att_results_all_states_year_only.rds"))
