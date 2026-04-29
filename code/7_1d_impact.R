# =============================================================================
# 7_1d_impact.R
#
# Purpose: Estimate ATT (average treatment effect on the treated) of the
#          Hillman summer program on college outcomes using doubly-robust
#          linear probability models with matching weights.
#
# Reference: https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
#
# Estimand: ATT, recovered via g-computation (average marginal effects over
#           treated units) using marginaleffects::avg_comparisons() with HC3
#           robust SEs. HC3 is the recommended SE for matching with
#           replacement (Hill & Reiter 2006; MatchIt vignette 2026).
#           NOTE — caliper drops some treated units, so the estimand is
#           technically the average treatment in the matched (caliper-passing)
#           sample (ATM); we interpret it as ATT for the matched cohort.
#
# Outcome panels (consolidated, policy-relevant set of 8):
#   All panels condition on has_nsc_record == 1 — i.e., students with an NSC
#   query record. Per PI input, students not appearing in NSC went to college
#   per Hillman's own records but their institution did not report to NSC, so
#   we cannot reliably infer enrollment for them. Conditioning on has_nsc_record
#   restricts to the population for whom NSC actually tracked outcomes.
#
#   Panel A — Enrollment (NSC-matched students)
#             enroll_seamless, enroll_seamless_stem
#   Panel B — Institution at entry (NSC-matched students)
#             inst_4yr_entry, inst_2yr_entry
#   Panel C — Persistence + degree (NSC-matched and enrolled, enroll_ever == 1)
#             pers_1y, pers_1y_stem,
#             deg_bach_6y (=7y window), deg_any_stem_6y
#
# Year-window naming caveat: deg_bach_6y is stored under the original variable
# name but Danielle's loop range (0/6, inclusive of year 0) makes it a 7-year
# window. The 5-year version (deg_bach_4y) was identical to the 7-year version
# in the matched sample and was dropped from the consolidated set. Table
# headers in script 8 read "within 7 years".
#
# Analytic sample is restricted to HS grad cohorts 2018–2021 in script 3b
# (2022 partial NSC coverage; 2023+ no NSC enrollment data). Per-panel cohort
# filters are therefore not needed here. Degree outcomes are right-censored
# for cohorts younger than the full nominal window — interpret as observed
# rates within the available follow-up.
#
# Samples:
#   All states:      matched_all_states_year_only.rds
#   PA public schools: matched_pa_year_only.rds
#
# Output:  output/att_results_all_states.rds
#          output/att_results_pa.rds
#          output/att_results_het.rds
# Tables and figures are produced by 8_1d_tables_figures.R.
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(marginaleffects)
library(here)

# =============================================================================
# 1. LOAD MATCHED DATASETS
# =============================================================================

matched_all <- readRDS(here("data", "matched", "matched_all_states_year_only.rds"))
matched_pa  <- readRDS(here("data", "matched", "matched_pa_year_only.rds"))

message(
  "All-states: ",
  nrow(matched_all),
  " obs | ",
  sum(matched_all$treated_in_year == 1),
  " treated / ",
  sum(matched_all$treated_in_year == 0),
  " control"
)
message(
  "PA public:  ",
  nrow(matched_pa),
  " obs | ",
  sum(matched_pa$treated_in_year == 1),
  " treated / ",
  sum(matched_pa$treated_in_year == 0),
  " control"
)


# =============================================================================
# 2. DEFINE OUTCOMES, COVARIATES, AND LABELS
# =============================================================================

# ---------------------------------------------------------------------------
# Outcome panels
# ---------------------------------------------------------------------------

# Panel A: Enrollment — NSC-matched students only
panel_a_outcomes <- c(
  "enroll_seamless",
  "enroll_seamless_stem"
)

# Panel B: Institution at entry — has_nsc_record == 1
panel_b_outcomes <- c(
  "inst_4yr_entry",
  "inst_2yr_entry"
)

# Panel C: Persistence + degree — enroll_ever == 1; cohort restrictions apply
panel_c_outcomes <- c(
  "pers_1y",
  "pers_1y_stem",
  "deg_bach_6y",      # actually 7-year window (see header note)
  "deg_any_stem_6y"
)

# ---------------------------------------------------------------------------
# Labels — year-window labels reflect Danielle's actual loop ranges, not
# the original variable names (see header).
# ---------------------------------------------------------------------------

outcome_labels <- c(
  # Panel A — Enrollment (NSC-matched)
  enroll_seamless      = "Seamless enrollment",
  enroll_seamless_stem = "Seamless STEM enrollment",
  # Panel B — Institution at entry
  inst_4yr_entry = "Initial enrollment: any 4-year",
  inst_2yr_entry = "Initial enrollment: any 2-year",
  # Panel C — Persistence + degree
  pers_1y         = "Persisted to 2nd year (any inst.)",
  pers_1y_stem    = "Persisted in STEM to 2nd year",
  deg_bach_6y     = "Bachelor's degree within 7 years",
  deg_any_stem_6y = "Any STEM degree within 6 years"
)

# ---------------------------------------------------------------------------
# Covariates — must match PS model exactly (same covars + _miss indicators)
# Grade dummies used (consistent with PS model); raw grade dropped
# ---------------------------------------------------------------------------

base_covars <- c(
  "gender",
  # Grade dummies: grade_11 is the implicit reference (modal grade, ~60% of
  # sample). Including all four with the intercept makes the design matrix
  # rank-deficient and lm() silently drops one. Consistent with script 5's
  # PS formula.
  "grade_9",
  "grade_10",
  "grade_12",
  "gpa",
  "psat_math",
  "stipend",
  "house_size",
  "racially_marginalized",
  "bi_multi_racial",
  # urban/suburban/rural are mutually exclusive after geo-NA imputation.
  # `suburban` is the implicit reference (modal category) — including all
  # three plus the geo-missing indicator and the intercept introduces a
  # linear identity (urban + rural + urban_miss + intercept).
  "urban",
  "rural",
  "disability",
  "neg_school",
  "us_citizen",
  # first_gen excluded: question wasn't on the 2017 or 2018 application
  # forms, so first_gen_miss == 1[year %in% c(2017, 2018)] is perfectly
  # collinear with year FE. See script 5 base_covariates note.
  # missing indicators — only included where missingness exists in matched
  # sample AND the column is not redundant.
  # Excluded as constant: gender_miss, stipend_miss, us_citizen_miss.
  # Excluded as redundant duplicates: suburban_miss, rural_miss
  # (all three geo _miss columns are derived from the same field and are
  # therefore identical; urban_miss alone covers the geographic missingness).
  # Excluded as collinear with year FE: first_gen_miss (see note above).
  "gpa_miss",
  "psat_math_miss",
  "house_size_miss",
  "racially_marginalized_miss",
  "bi_multi_racial_miss",
  "urban_miss",
  "disability_miss",
  "neg_school_miss"
)

pa_covars <- c(
  base_covars,
  "school_enrollment",
  "school_pct_econ_disadvantaged",
  "school_pct_english_learner",
  "school_pct_special_ed",
  "school_pct_white"
)


# =============================================================================
# 3. HELPER: FIT LPM + EXTRACT ATT VIA G-COMPUTATION
# =============================================================================
# Fits a weighted LPM via lm() (the LPM choice keeps the marginal RD as the
# natural ATT scale for binary outcomes). The ATT is recovered via
# marginaleffects::avg_comparisons() with HC3 robust SEs and newdata
# restricted to treated units — i.e., g-computation averaged over the treated.
# Year FEs included. Constant predictors in a subsample are dropped
# automatically to avoid rank-deficient models (common in small subgroup
# analyses). HC3 is the recommended SE for matching with replacement
# (Hill & Reiter 2006; MatchIt vignette).

fit_att <- function(data, outcome_var, predictors) {
  stopifnot("weights" %in% names(data))
  data <- droplevels(data)

  # Drop predictors that are constant or missing in this subsample
  predictors <- predictors[sapply(predictors, \(p) {
    col <- data[[p]]
    !is.null(col) && length(unique(col[!is.na(col)])) > 1
  })]

  formula_str <- paste0(
    outcome_var,
    " ~ treated_in_year + ",
    paste(predictors, collapse = " + "),
    " + factor(year)"
  )

  fit <- lm(as.formula(formula_str), data = data, weights = weights)

  # G-computation: average marginal effect of treated_in_year (0 → 1) over
  # treated units (newdata = subset of treated → ATT). HC3 robust SEs.
  ac <- avg_comparisons(
    fit,
    variables = "treated_in_year",
    vcov      = "HC3",
    newdata   = subset(data, treated_in_year == 1)
  )

  wt_mean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)

  tibble(
    outcome = outcome_var,
    label = outcome_labels[outcome_var],
    n_obs = nrow(data),
    n_treated = sum(data$treated_in_year == 1),
    n_control = sum(data$treated_in_year == 0),
    ctrl_mean = wt_mean(
      data[[outcome_var]][data$treated_in_year == 0],
      data$weights[data$treated_in_year == 0]
    ),
    trt_mean = wt_mean(
      data[[outcome_var]][data$treated_in_year == 1],
      data$weights[data$treated_in_year == 1]
    ),
    att     = ac$estimate,
    se      = ac$std.error,
    pval    = ac$p.value,
    conf_lo = ac$conf.low,
    conf_hi = ac$conf.high
  )
}

# =============================================================================
# 4. HELPER: RUN ALL OUTCOME PANELS FOR ONE SAMPLE
# =============================================================================

run_all_outcomes <- function(matched, covars, sample_label) {
  matched_nsc <- matched |> filter(has_nsc_record == 1)
  message(
    "\n--- ",
    sample_label,
    ": NSC-matched subsample = ",
    nrow(matched_nsc),
    " (",
    sum(matched_nsc$treated_in_year == 1),
    " treated / ",
    sum(matched_nsc$treated_in_year == 0),
    " control) ---"
  )

  # Panel A: Enrollment — NSC-matched
  message("\n--- ", sample_label, ": Panel A — enrollment ---")
  results_a <- map(panel_a_outcomes, \(out) {
    message("  Fitting: ", out)
    fit_att(matched_nsc, out, covars)
  }) |>
    list_rbind() |>
    mutate(panel = "A")

  # Panel B: Institution at entry — NSC-matched
  message("\n--- ", sample_label, ": Panel B — institution at entry ---")
  results_b <- map(panel_b_outcomes, \(out) {
    message("  Fitting: ", out)
    fit_att(matched_nsc, out, covars)
  }) |>
    list_rbind() |>
    mutate(panel = "B")

  # Panel C: Persistence + degree — enroll_ever == 1.
  # Analytic sample is already restricted to 2018–2021 in script 3b; no
  # per-outcome cohort filters needed here.
  message("\n--- ", sample_label, ": Panel C — persistence + degree ---")
  results_c <- map(panel_c_outcomes, \(out) {
    message("  Fitting: ", out)

    data_sub <- matched |>
      filter(enroll_ever == 1, !is.na(.data[[out]]))

    message(
      "    n = ",
      nrow(data_sub),
      " (",
      sum(data_sub$treated_in_year == 1),
      " treated / ",
      sum(data_sub$treated_in_year == 0),
      " control)"
    )
    fit_att(data_sub, out, covars)
  }) |>
    list_rbind() |>
    mutate(panel = "C")

  bind_rows(results_a, results_b, results_c) |>
    mutate(subsample = sample_label)
}

# =============================================================================
# 5. RUN OUTCOME MODELS
# =============================================================================

results_all <- run_all_outcomes(matched_all, base_covars, "All States")
results_pa  <- run_all_outcomes(matched_pa, pa_covars, "PA Public Schools")

# Quick inspect
results_all |>
  select(panel, label, n_treated, n_control, ctrl_mean, trt_mean, att, se, pval)

results_pa |>
  select(panel, label, n_treated, n_control, ctrl_mean, trt_mean, att, se, pval)

# =============================================================================
# 6. HETEROGENEITY ANALYSIS
# =============================================================================
# Re-run fit_att() within subgroups for two focal STEM outcomes:
#   - enroll_seamless_stem (STEM enrollment, Panel A; has_nsc_record == 1)
#   - pers_1y_stem         (STEM persistence, Panel C; enroll_ever == 1)
#
# Subgroups: racially_marginalized, gender, urban/rural
# Results saved to RDS; plots produced by 8_1d_tables_figures.R.

het_outcomes <- c("enroll_seamless_stem", "pers_1y_stem")

het_labels <- c(
  enroll_seamless_stem = "Seamless STEM Enrollment",
  pers_1y_stem         = "Persisted in STEM to 2nd Year"
)

run_het <- function(matched, covars, sample_label) {
  subgroups <- list(
    list(
      name = "racially_marginalized",
      var = "racially_marginalized",
      levels = list(
        "Racially marginalized" = 1,
        "Not racially marginalized" = 0
      )
    ),
    # first_gen subgroup removed: only 1 of 237 matched students is coded
    # first-generation (the question wasn't on 2017-2018 forms; values
    # imputed to 0). Subgroup ATT would be n=1 — not meaningful.
    list(
      name = "gender",
      var = "gender",
      levels = list("Female" = 0, "Male" = 1)
    ),
    list(
      name = "geography",
      var = NULL,
      levels = list("Urban" = "urban", "Rural" = "rural")
    )
  )

  map(subgroups, \(sg) {
    map(names(sg$levels), \(level_label) {
      map(het_outcomes, \(out) {
        data_sub <- if (sg$name == "geography") {
          geo_var <- sg$levels[[level_label]]
          matched |> filter(.data[[geo_var]] == 1)
        } else {
          matched |> filter(.data[[sg$var]] == sg$levels[[level_label]])
        }

        # Conditioning matches the main analysis: enroll_seamless_stem is
        # NSC-conditional (has_nsc_record == 1); pers_1y_stem is conditional
        # on enrollment (enroll_ever == 1, which implies has_nsc_record == 1).
        if (out == "enroll_seamless_stem") {
          data_sub <- data_sub |>
            filter(has_nsc_record == 1, !is.na(.data[[out]]))
        } else if (out == "pers_1y_stem") {
          data_sub <- data_sub |>
            filter(enroll_ever == 1, !is.na(.data[[out]]))
        }

        # Require at least 15 treated obs
        if (sum(data_sub$treated_in_year == 1, na.rm = TRUE) < 15) {
          return(NULL)
        }

        fit_att(data_sub, out, covars) |>
          mutate(
            subgroup = sg$name,
            subgroup_label = level_label,
            subsample = sample_label
          )
      }) |>
        compact() |>
        list_rbind()
    }) |>
      compact() |>
      list_rbind()
  }) |>
    compact() |>
    list_rbind()
}

results_het_all <- run_het(matched_all, base_covars, "All States")
results_het_pa  <- run_het(matched_pa, pa_covars, "PA Public Schools")

results_het <- bind_rows(results_het_all, results_het_pa) |>
  mutate(
    outcome_label = het_labels[outcome],
    subgroup_label = factor(
      subgroup_label,
      levels = c(
        "Racially marginalized",
        "Not racially marginalized",
        "Female",
        "Male",
        "Urban",
        "Rural"
      )
    ),
    subsample = factor(subsample, levels = c("All States", "PA Public Schools"))
  )

# =============================================================================
# 7. SAVE RESULTS
# =============================================================================

dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

saveRDS(results_all, here("output", "att_results_all_states.rds"))
saveRDS(results_pa,  here("output", "att_results_pa.rds"))
saveRDS(results_het, here("output", "att_results_het.rds"))

message("\n=== Impact analysis complete ===")
message(
  "Saved: att_results_all_states.rds / att_results_pa.rds / ",
  "att_results_het.rds"
)
