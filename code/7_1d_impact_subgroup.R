# =============================================================================
# 7_1d_impact_subgroup.R
#
# Purpose: Subgroup ATT estimates for the Hillman summer program. Repeats the
#          main outcome models from 7_1d_impact.R within eight pre-specified
#          subgroups defined by student characteristics.
#
# Subgroups:
#   stipend              — stipend-eligible students
#   racially_marginalized — Black, Latino, Native American, Pacific Islander
#   urban / suburban / rural — geographic location
#   disability           — documented disability
#   neg_school           — negative school environment
#   first_gen            — first-generation college student
#
# NOTE on statistical power: rural (n_treated = 11), disability (17),
#   neg_school (16), and first_gen (8) fall below the minimum threshold of
#   20 treated students and are reported descriptively only. Estimates for
#   these groups are suppressed to avoid spurious inference.
#
# Method: Same doubly-robust LPM + matching weights + cluster-robust SEs as
#   the main analysis (see 7_1d_impact.R). Subgroup samples are subsets of
#   the already-matched datasets — no re-matching is performed.
#
# Input:   data/matched_all_states_year_only.rds
#          data/matched_pa_year_only.rds
#          (also sources fit_att() and helpers from 7_1d_impact.R)
#
# Output:  output/att_subgroup_all_states.rds
#          output/att_subgroup_pa.rds
# Tables and figures are produced by 8_1d_tables_figures.R.
# =============================================================================

library(dplyr)
library(purrr)
library(tidyr)
library(here)

# =============================================================================
# 1. LOAD MATCHED DATA AND SHARED OBJECTS
# =============================================================================
# Source the main impact script to load matched datasets, fit_att(),
# outcome vectors, labels, and covariate lists.

source(here("code", "7_1d_impact.R"))

# =============================================================================
# 2. SUBGROUP DEFINITIONS
# =============================================================================

subgroups <- list(
  list(var = "stipend", label = "Stipend Eligible"),
  list(var = "racially_marginalized", label = "Racially Marginalized"),
  list(var = "urban", label = "Urban"),
  list(var = "suburban", label = "Suburban"),
  list(var = "rural", label = "Rural"),
  list(var = "disability", label = "Documented Disability"),
  list(var = "neg_school", label = "Negative School Environment"),
  list(var = "first_gen", label = "First-Generation")
)

# Minimum treated count to attempt inference. Groups below this threshold
# are included in the sample size summary but excluded from the ATT table.
MIN_TREATED <- 20

# panel_a/b/c_outcomes and fit_att() are inherited from 7_1d_impact.R via source().

# =============================================================================
# 3. HELPER: RUN OUTCOMES FOR ONE SUBGROUP × ONE SAMPLE
# =============================================================================
# Mirrors run_all_outcomes() from the main script but for a single subgroup
# subset. Degree outcomes are skipped for small subgroups (< MIN_TREATED)
# to avoid rank-deficient models.

run_subgroup <- function(matched, covars, sg_var, sg_label, sample_label) {
  data_sg <- matched |>
    filter(!is.na(.data[[sg_var]]), .data[[sg_var]] == 1)

  n_trt <- sum(data_sg$treated_in_year == 1)
  n_ctl <- sum(data_sg$treated_in_year == 0)

  message(
    "\n  ",
    sg_label,
    " (",
    sample_label,
    "): ",
    n_trt,
    " treated / ",
    n_ctl,
    " control"
  )

  # Return sample size row only if below threshold
  if (n_trt < MIN_TREATED) {
    message(
      "    -> below minimum threshold (",
      MIN_TREATED,
      " treated) — skipping inference"
    )
    return(tibble(
      subgroup = sg_label,
      subsample = sample_label,
      outcome = NA_character_,
      label = NA_character_,
      sample = NA_character_,
      n_obs = nrow(data_sg),
      n_treated = n_trt,
      n_control = n_ctl,
      ctrl_mean = NA_real_,
      trt_mean = NA_real_,
      att = NA_real_,
      se = NA_real_,
      pval = NA_real_,
      conf_lo = NA_real_,
      conf_hi = NA_real_,
      suppressed = TRUE
    ))
  }

  # NSC-matched subsample — basis for Panels A and B (per main analysis,
  # conditioning on has_nsc_record == 1 since students missing from NSC have
  # unknown enrollment status, not confirmed non-enrollment).
  data_nsc <- data_sg |> filter(has_nsc_record == 1)

  # (a) Panel A: enrollment — NSC-matched
  res_enroll <- map(panel_a_outcomes, \(out) {
    fit_att(data_nsc, out, covars)
  }) |>
    list_rbind() |>
    mutate(sample = "nsc_matched")

  # (b) Panel B: institution at entry — NSC-matched
  res_enroll_nsc <- map(panel_b_outcomes, \(out) {
    fit_att(data_nsc, out, covars)
  }) |>
    list_rbind() |>
    mutate(sample = "nsc_matched")

  # (c) Degree / persistence — non-censored subsample.
  # Analytic sample is restricted to 2018–2021 cohorts in script 3b, so no
  # additional per-outcome cohort filtering is needed here.
  res_degree <- map(panel_c_outcomes, \(out) {
    data_sub <- data_sg |> filter(!is.na(.data[[out]]))
    if (sum(data_sub$treated_in_year == 1) < MIN_TREATED) {
      return(NULL)
    }
    fit_att(data_sub, out, covars) |> mutate(sample = "degree_eligible")
  }) |>
    compact() |>
    list_rbind()

  bind_rows(res_enroll, res_enroll_nsc, res_degree) |>
    mutate(subgroup = sg_label, subsample = sample_label, suppressed = FALSE)
}

# =============================================================================
# 4. RUN ALL SUBGROUPS
# =============================================================================

message("\n=== ALL STATES SUBGROUP ANALYSIS ===")

results_subgroup_all <- map(subgroups, \(sg) {
  run_subgroup(matched_all, base_covars, sg$var, sg$label, "All States")
}) |> list_rbind()

message("\n=== PA PUBLIC SCHOOLS SUBGROUP ANALYSIS ===")

results_subgroup_pa <- map(subgroups, \(sg) {
  run_subgroup(matched_pa, pa_covars, sg$var, sg$label, "PA Public Schools")
}) |> list_rbind()

# =============================================================================
# 5. SAVE RESULTS
# =============================================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

saveRDS(results_subgroup_all, here("output", "att_subgroup_all_states.rds"))
saveRDS(results_subgroup_pa, here("output", "att_subgroup_pa.rds"))

message("\nSaved: att_subgroup_all_states.rds")
message("Saved: att_subgroup_pa.rds")
message("\n=== Subgroup analysis complete ===")
