# =============================================================================
# 7_1d_impact.R
#
# Purpose: Estimate ATT (average treatment effect on the treated) of the
#          Hillman summer program on college outcomes. Follows MatchIt best
#          practices using doubly robust linear probability models with
#          matching weights and cluster-robust standard errors.
#
# Reference: https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
#
# Estimand: ATT via lm() with matching weights; avg_slopes() with SEs
#           clustered by matched subclass (pair).
#
# Sample splits:
#   - Enrollment outcomes: full matched sample (all cohorts)
#   - Degree / retention outcomes: restricted to non-censored students
#     (NAs in outcome variables encode insufficient follow-up time)
#
# Samples:
#   - All states: matched_all_states_year_only.rds
#   - PA public schools: matched_pa_year_only.rds — adds school-level
#     covariates to the outcome model
#
# Output:  output/att_results_all_states_year_only.rds
#          output/att_results_pa_year_only.rds
# =============================================================================

library(dplyr)
library(purrr)
library(marginaleffects)
library(sandwich)
library(lmtest)
library(here)

# =============================================================================
# 1. LOAD MATCHED DATASETS
# =============================================================================

matched_all <- readRDS(here("data", "matched_all_states_year_only.rds"))
matched_pa <- readRDS(here("data", "matched_pa_year_only.rds"))

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
# 2. DEFINE OUTCOMES AND COVARIATES
# =============================================================================
# Three outcome groups:
#   (a) enrollment_outcomes  — full matched sample; non-NSC-matches coded 0
#   (b) enrollment_outcomes  — NSC-matched subsample only (robustness check)
#   (c) degree_outcomes      — non-censored subsample (NAs = insufficient
#                              follow-up, not missing data)
#
# Running (a) and (b) side-by-side lets us assess whether the coding-as-0
# assumption for non-NSC-matches materially affects enrollment ATT estimates.

enrollment_outcomes <- c(
  "seamless_enroll",
  "seamless_enroll_stem",
  "enrolled_ever_nsc",
  "enrolled_ever_stem",
  "x4yr_initial",
  "public4yr_initial",
  "firsttime_fulltime"
)

degree_outcomes <- c(
  "reten_fall_enter2",
  "degree_6years_all_nsc",
  "bachdegree_6years_all_nsc",
  "ste_mdegree_in6_grad"
)

outcome_labels <- c(
  seamless_enroll = "Seamless enrollment (any)",
  seamless_enroll_stem = "Seamless STEM enrollment",
  enrolled_ever_nsc = "Ever enrolled (any)",
  enrolled_ever_stem = "Ever enrolled STEM",
  x4yr_initial = "Initial enrollment: any 4-year",
  public4yr_initial = "Initial enrollment: public 4-year",
  firsttime_fulltime = "First-time full-time enrollment",
  reten_fall_enter2 = "Retained into 2nd year",
  degree_6years_all_nsc = "Any degree within 6 years",
  bachdegree_6years_all_nsc = "Bachelor's degree within 6 years",
  ste_mdegree_in6_grad = "STEM degree within 6 years"
)

# Base adjustment covariates (all-states outcome model).
base_covars <- c(
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
  "first_gen",
  "gpa_miss",
  "psat_math_miss",
  "neg_school_miss",
  "first_gen_miss",
  "racially_marginalized_miss",
  "bi_multi_racial_miss",
  "urban_miss",
  "suburban_miss",
  "rural_miss",
  "disability_miss"
)

# PA adds school-level covariates — especially important for school_enrollment
# and psat_math which were imbalanced post-match (see balance table).
pa_covars <- c(
  base_covars,
  "school_enrollment",
  "school_pct_econ_disadvantaged",
  "school_pct_english_learner",
  "school_pct_special_ed",
  "school_pct_white"
)

# =============================================================================
# 2b. ADD NSC MATCH FLAG
# =============================================================================
# A student is considered NSC-matched if they have any non-NA degree/
# persistence outcome OR a positive enrollment outcome. This is more robust
# than checking seamless_enroll alone since some students enroll but are
# coded 0 for seamless (e.g. delayed enrollment).
# Must be called after degree_outcomes is defined.

add_nsc_flag <- function(data) {
  data |>
    mutate(
      nsc_matched = if_else(
        rowSums(across(all_of(degree_outcomes), ~ !is.na(.))) > 0 |
          enrolled_ever_nsc == 1 |
          seamless_enroll == 1,
        TRUE,
        FALSE
      )
    )
}

matched_all <- add_nsc_flag(matched_all)
matched_pa <- add_nsc_flag(matched_pa)

message("\nNSC match rates:")
message(
  "  All-states — treated: ",
  round(
    100 * mean(matched_all$nsc_matched[matched_all$treated_in_year == 1]),
    1
  ),
  "% | control: ",
  round(
    100 * mean(matched_all$nsc_matched[matched_all$treated_in_year == 0]),
    1
  ),
  "%"
)
message(
  "  PA — treated: ",
  round(100 * mean(matched_pa$nsc_matched[matched_pa$treated_in_year == 1]), 1),
  "% | control: ",
  round(100 * mean(matched_pa$nsc_matched[matched_pa$treated_in_year == 0]), 1),
  "%"
)


# =============================================================================
# 3. HELPER: FIT LPM + EXTRACT ATT VIA avg_slopes()
# =============================================================================
# For each outcome, fit a weighted LPM with cohort FEs, then use avg_slopes()
# to estimate the ATT restricted to treated units with cluster-robust SEs.
# Constant _miss predictors in a given subsample are dropped automatically.
#
# NOTE on clustering: subclass is not passed as a model predictor so it is
# absent from the model frame. vcovCL must receive the cluster vector
# directly from the data rather than via the formula interface (~subclass).

fit_att <- function(data, outcome_var, predictors) {
  # Drop unused factor levels (important for subsetted data e.g. GPA bins)
  data <- droplevels(data)

  # Drop predictors that are constant in this subsample
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

  treated_data <- subset(data, treated_in_year == 1)

  # Extract subclass from the rows actually used by lm() (na.omit may drop
  # rows, so align via rownames of the model frame).
  # Fall back to HC3 heteroskedasticity-robust SEs if subclass is missing
  # or has only one level (can happen in small GPA-bin subsamples).
  model_rows <- as.integer(rownames(model.frame(fit)))
  cluster_vec <- data$subclass[model_rows]

  vcov_mat <- tryCatch(
    sandwich::vcovCL(fit, cluster = cluster_vec),
    error = function(e) sandwich::vcovHC(fit, type = "HC3")
  )

  att <- avg_slopes(
    fit,
    variables = "treated_in_year",
    vcov = vcov_mat,
    newdata = treated_data,
    wts = "weights"
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
    att_pp = att$estimate * 100,
    se_pp = att$std.error * 100,
    pval = att$p.value,
    conf_lo_pp = att$conf.low * 100,
    conf_hi_pp = att$conf.high * 100
  )
}

# =============================================================================
# 4. HELPER: RUN ALL OUTCOMES FOR ONE SAMPLE
# =============================================================================

run_all_outcomes <- function(matched, covars, sample_label) {
  # (a) Enrollment outcomes — full matched sample (non-NSC-matches coded 0)
  message(
    "\n--- ",
    sample_label,
    ": enrollment outcomes (full sample, non-matches = 0) ---"
  )

  results_enrollment <- map(enrollment_outcomes, \(out) {
    message("  Fitting: ", out)
    fit_att(matched, out, covars)
  }) |>
    list_rbind() |>
    mutate(sample = "full")

  # (b) Enrollment outcomes — NSC-matched students only (robustness check)
  # Compares against (a) to assess sensitivity to the coding-as-0 assumption.
  message(
    "\n--- ",
    sample_label,
    ": enrollment outcomes (NSC-matched subsample) ---"
  )

  matched_nsc <- matched |> filter(nsc_matched)
  message(
    "  NSC-matched subsample: ",
    nrow(matched_nsc),
    " (",
    sum(matched_nsc$treated_in_year == 1),
    " treated / ",
    sum(matched_nsc$treated_in_year == 0),
    " control)"
  )

  results_enrollment_nsc <- map(enrollment_outcomes, \(out) {
    message("  Fitting: ", out)
    fit_att(matched_nsc, out, covars)
  }) |>
    list_rbind() |>
    mutate(sample = "nsc_matched")

  # (c) Degree / persistence outcomes — non-censored subsample per outcome
  message(
    "\n--- ",
    sample_label,
    ": degree/persistence outcomes (non-censored subsample) ---"
  )

  results_degree <- map(degree_outcomes, \(out) {
    message("  Fitting: ", out)
    data_sub <- matched |> filter(!is.na(.data[[out]]))
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
    mutate(sample = "degree_eligible")

  bind_rows(results_enrollment, results_enrollment_nsc, results_degree) |>
    mutate(subsample = sample_label)
}

# =============================================================================
# 5. RUN OUTCOME MODELS
# =============================================================================

results_all <- run_all_outcomes(matched_all, base_covars, "All States")
results_pa <- run_all_outcomes(matched_pa, pa_covars, "PA Public Schools")

# =============================================================================
# 6. INSPECT RESULTS
# =============================================================================

results_all |>
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

results_pa |>
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
# 7. ENSURE OUTPUT DIRECTORY EXISTS
# =============================================================================

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}

# =============================================================================
# 8. TREATMENT EFFECT TABLES
# =============================================================================
# Publication-ready gt tables formatted to match the balance tables.
# Each table has three panels:
#   Panel A — Enrollment outcomes (full matched sample; non-NSC-matches = 0)
#   Panel B — Enrollment outcomes (NSC-matched subsample; robustness check)
#   Panel C — Degree and persistence outcomes (non-censored subsample)
#
# Columns: outcome label | control mean | treated mean | ATT (pp) | SE | 95% CI | p-value

make_att_table <- function(
  results,
  title_subtitle,
  n_treated_full,
  n_control_full
) {
  fmt_pval <- function(p) {
    case_when(
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  }

  fmt_ci <- function(lo, hi) {
    paste0("[", sprintf("%.2f", lo), ", ", sprintf("%.2f", hi), "]")
  }

  sig_stars <- function(p) {
    case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.1 ~ "*",
      TRUE ~ ""
    )
  }

  prep_panel <- function(data) {
    data |>
      mutate(
        ctrl_mean_fmt = sprintf("%.3f", ctrl_mean),
        trt_mean_fmt = sprintf("%.3f", trt_mean),
        att_fmt = paste0(sprintf("%.3f", att_pp / 100), sig_stars(pval)),
        se_fmt = paste0("(", sprintf("%.3f", se_pp / 100), ")"),
        ci_fmt = fmt_ci(conf_lo_pp / 100, conf_hi_pp / 100),
        pval_fmt = fmt_pval(pval)
      ) |>
      select(
        label,
        n_obs,
        n_treated,
        ctrl_mean_fmt,
        trt_mean_fmt,
        att_fmt,
        se_fmt,
        ci_fmt,
        pval_fmt
      )
  }

  panel_a <- results |> filter(sample == "full") |> prep_panel()
  panel_b <- results |> filter(sample == "nsc_matched") |> prep_panel()
  panel_c <- results |> filter(sample == "degree_eligible") |> prep_panel()

  # Add panel labels as stub rows
  spacer <- tibble(
    label = NA_character_,
    n_obs = NA_integer_,
    n_treated = NA_integer_,
    ctrl_mean_fmt = NA,
    trt_mean_fmt = NA,
    att_fmt = NA,
    se_fmt = NA,
    ci_fmt = NA,
    pval_fmt = NA
  )

  tbl <- bind_rows(
    panel_a |> mutate(panel = "A"),
    panel_b |> mutate(panel = "B"),
    panel_c |> mutate(panel = "C")
  )

  # Row indices for panel boundaries
  na_rows_a <- nrow(panel_a)
  na_rows_b <- nrow(panel_a) + nrow(panel_b)
  last_row <- nrow(tbl)

  tbl |>
    gt(rowname_col = "label") |>
    tab_header(
      title = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    # Panel labels
    tab_row_group(
      label = md(
        "**Panel C: Degree and Persistence Outcomes** *(non-censored subsample)*"
      ),
      rows = panel == "C"
    ) |>
    tab_row_group(
      label = md(
        "**Panel B: Enrollment Outcomes** *(NSC-matched students only)*"
      ),
      rows = panel == "B"
    ) |>
    tab_row_group(
      label = md("**Panel A: Enrollment Outcomes** *(full matched sample)*"),
      rows = panel == "A"
    ) |>
    cols_hide(panel) |>
    # Column labels
    cols_label(
      n_obs = "N",
      n_treated = "Treated",
      ctrl_mean_fmt = "Control",
      trt_mean_fmt = "Treated",
      att_fmt = "ATT",
      se_fmt = "(SE)",
      ci_fmt = "95% CI",
      pval_fmt = "p-value"
    ) |>
    tab_spanner(label = "Sample Size", columns = c(n_obs, n_treated)) |>
    tab_spanner(
      label = "Mean Outcome",
      columns = c(ctrl_mean_fmt, trt_mean_fmt)
    ) |>
    tab_spanner(
      label = "Treatment Effect",
      columns = c(att_fmt, se_fmt, ci_fmt, pval_fmt)
    ) |>
    # Alignment
    cols_align(align = "left", columns = label) |>
    cols_align(
      align = "center",
      columns = c(
        n_obs,
        n_treated,
        ctrl_mean_fmt,
        trt_mean_fmt,
        att_fmt,
        se_fmt,
        ci_fmt,
        pval_fmt
      )
    ) |>
    # Borders
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = cells_body(rows = last_row)
    ) |>
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_row_groups()
    ) |>
    # Notes
    tab_source_note(
      source_note = md(paste0(
        "*Notes:* ATT = average treatment effect on the treated, estimated via a ",
        "linear probability model (LPM) with matching weights and cohort fixed effects. ",
        "Standard errors (in parentheses) are clustered by matched subclass. ",
        "95% confidence intervals are reported in brackets. ",
        "All outcomes are binary (0/1); ATT and means are reported as proportions. ",
        "\\* p < 0.10, \\*\\* p < 0.05, \\*\\*\\* p < 0.01."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "Panel A includes all matched students (N = ",
        n_treated_full,
        " treated, ",
        n_control_full,
        " control); students with no NSC record are coded as not enrolled. ",
        "Panel B restricts to students found in the NSC as a robustness check. ",
        "Panel C restricts to students with sufficient follow-up time (non-missing outcome); ",
        "sample size varies by outcome."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "Propensity scores estimated via logistic regression with nearest-neighbor matching ",
        "with replacement, caliper = 0.5 SD, exact match on application year. ",
        "Year 2020 (COVID disruption) and year 2022 (insufficient follow-up) excluded."
      ))
    ) |>
    tab_options(
      table.font.size = px(11),
      heading.title.font.size = px(13),
      heading.subtitle.font.size = px(11),
      heading.align = "left",
      column_labels.font.weight = "bold",
      column_labels.border.top.color = "black",
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(1),
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(2),
      row_group.border.top.color = "black",
      row_group.border.top.width = px(1),
      row_group.border.bottom.color = "gray80",
      row_group.border.bottom.width = px(1),
      row_group.background.color = "gray97",
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      source_notes.font.size = px(10),
      data_row.padding = px(3)
    )
}

att_gt_all <- make_att_table(
  results = results_all,
  title_subtitle = c(
    "Table 2: Effects of the Hillman Summer Program on College Outcomes",
    "All States — Propensity Score Matched Sample"
  ),
  n_treated_full = sum(matched_all$treated_in_year == 1),
  n_control_full = sum(matched_all$treated_in_year == 0)
)

att_gt_pa <- make_att_table(
  results = results_pa,
  title_subtitle = c(
    "Table 3: Effects of the Hillman Summer Program on College Outcomes",
    "PA Public Schools — Propensity Score Matched Sample"
  ),
  n_treated_full = sum(matched_pa$treated_in_year == 1),
  n_control_full = sum(matched_pa$treated_in_year == 0)
)

att_gt_all
att_gt_pa

# =============================================================================
# 9. DESCRIPTIVE TABLES BY COHORT
# =============================================================================
# Weighted means by year and treatment status for six focal outcomes.
# No inference — purely descriptive. Censored cells (both groups = 0 due to
# insufficient follow-up) are shown as "—".

focal_outcomes <- c(
  "seamless_enroll",
  "seamless_enroll_stem",
  "x4yr_initial",
  "public4yr_initial",
  "reten_fall_enter2",
  "degree_6years_all_nsc"
)

focal_labels <- c(
  seamless_enroll = "Seamless enrollment (any)",
  seamless_enroll_stem = "Seamless STEM enrollment",
  x4yr_initial = "Initial enrollment: any 4-year",
  public4yr_initial = "Initial enrollment: public 4-year",
  reten_fall_enter2 = "Retained into 2nd year",
  degree_6years_all_nsc = "Any degree within 6 years"
)

make_year_table <- function(matched, title_subtitle) {
  years <- sort(unique(matched$year))

  tbl <- matched |>
    group_by(year, treated_in_year) |>
    summarise(
      n = n(),
      across(all_of(focal_outcomes), ~ weighted.mean(., weights, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(group = if_else(treated_in_year == 1, "trt", "ctrl")) |>
    select(-treated_in_year) |>
    pivot_longer(
      all_of(focal_outcomes),
      names_to = "outcome",
      values_to = "mean"
    ) |>
    pivot_wider(names_from = group, values_from = c(mean, n)) |>
    mutate(
      censored = (mean_trt == 0 & mean_ctrl == 0),
      mean_ctrl = if_else(censored, NA_real_, mean_ctrl),
      mean_trt = if_else(censored, NA_real_, mean_trt),
      diff = mean_trt - mean_ctrl,
      outcome_label = focal_labels[outcome],
      ctrl_fmt = if_else(is.na(mean_ctrl), "—", sprintf("%.3f", mean_ctrl)),
      trt_fmt = if_else(is.na(mean_trt), "—", sprintf("%.3f", mean_trt)),
      diff_fmt = case_when(
        is.na(diff) ~ "—",
        diff >= 0 ~ paste0("+", sprintf("%.3f", diff)),
        TRUE ~ sprintf("%.3f", diff)
      ),
      year = as.character(year)
    ) |>
    select(outcome_label, year, n_trt, n_ctrl, ctrl_fmt, trt_fmt, diff_fmt) |>
    arrange(factor(outcome_label, levels = focal_labels), year) |>
    pivot_wider(
      names_from = year,
      values_from = c(n_trt, n_ctrl, ctrl_fmt, trt_fmt, diff_fmt),
      names_glue = "{year}_{.value}"
    )

  ordered_cols <- c(
    "outcome_label",
    as.vector(outer(
      as.character(years),
      c("n_trt", "n_ctrl", "ctrl_fmt", "trt_fmt", "diff_fmt"),
      paste,
      sep = "_"
    ))
  )
  tbl <- tbl |> select(all_of(ordered_cols))

  n_rows <- nrow(tbl)

  label_list <- setNames(
    rep(list("NT", "NC", "Control", "Treated", "Diff."), length(years)),
    as.vector(outer(
      as.character(years),
      c("n_trt", "n_ctrl", "ctrl_fmt", "trt_fmt", "diff_fmt"),
      paste,
      sep = "_"
    ))
  )

  gt_tbl <- tbl |>
    gt(rowname_col = "outcome_label") |>
    tab_header(
      title = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    cols_label(.list = label_list)

  for (yr in as.character(years)) {
    gt_tbl <- gt_tbl |>
      tab_spanner(
        label = md(paste0("**", yr, "**")),
        columns = starts_with(paste0(yr, "_"))
      )
  }

  gt_tbl |>
    cols_align(align = "left", columns = 1) |>
    cols_align(align = "center", columns = -1) |>
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = cells_body(rows = n_rows)
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "*Notes:* Means are weighted by matching weights. ",
        "NT = treated students; NC = matched controls. ",
        "Diff. = treated minus control mean (descriptive only — no inference). ",
        "Cells marked '—' indicate outcomes not yet observed due to insufficient ",
        "follow-up time for that cohort. ",
        "Year 2020 excluded (COVID disruption); year 2022 excluded (insufficient follow-up)."
      ))
    ) |>
    tab_options(
      table.font.size = px(11),
      heading.title.font.size = px(13),
      heading.subtitle.font.size = px(11),
      heading.align = "left",
      column_labels.font.weight = "bold",
      column_labels.border.top.color = "black",
      column_labels.border.top.width = px(2),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(1),
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(2),
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      source_notes.font.size = px(10),
      data_row.padding = px(3)
    )
}

desc_gt_all <- make_year_table(
  matched_all,
  c(
    "Descriptive Outcome Means by Cohort and Treatment Status",
    "All States — Matched Sample"
  )
)

desc_gt_pa <- make_year_table(
  matched_pa,
  c(
    "Descriptive Outcome Means by Cohort and Treatment Status",
    "PA Public Schools — Matched Sample"
  )
)

gtsave(desc_gt_all, here("output", "desc_by_year_all_states.html"))
gtsave(desc_gt_all, here("output", "desc_by_year_all_states.tex"))
gtsave(desc_gt_pa, here("output", "desc_by_year_pa.html"))
gtsave(desc_gt_pa, here("output", "desc_by_year_pa.tex"))

# =============================================================================
# 10. SAVE ALL TABLES
# =============================================================================

gtsave(att_gt_all, here("output", "att_table_all_states.html"))
gtsave(att_gt_all, here("output", "att_table_all_states.tex"))
gtsave(att_gt_pa, here("output", "att_table_pa.html"))
gtsave(att_gt_pa, here("output", "att_table_pa.tex"))

message("Saved: att_table_all_states.html/.tex")
message("Saved: att_table_pa.html/.tex")

# =============================================================================
# 11. SAVE RESULTS RDS
# =============================================================================

saveRDS(results_all, here("output", "att_results_all_states_year_only.rds"))
saveRDS(results_pa, here("output", "att_results_pa_year_only.rds"))

message("\nSaved: att_results_all_states_year_only.rds")
message("Saved: att_results_pa_year_only.rds")

# =============================================================================
# 11. HETEROGENEITY BY GPA BIN
# =============================================================================
# Re-run fit_att() separately within each GPA tertile for two focal outcomes:
# seamless STEM enrollment and 2nd-year retention. Small within-bin samples
# produce wide CIs; patterns should be interpreted cautiously.
# Results are visualised with a faceted dot-and-whisker plot.

gpa_bin_levels <- c("3.0 – 3.4", "3.5 – 3.9", "4.0+")

het_outcomes <- c("seamless_enroll_stem", "reten_fall_enter2")

het_labels <- c(
  seamless_enroll_stem = "Seamless STEM Enrollment",
  reten_fall_enter2 = "Retained into 2nd Year"
)

run_gpa_het <- function(matched, covars, sample_label) {
  expand.grid(
    gpa_bin = gpa_bin_levels,
    outcome = het_outcomes,
    stringsAsFactors = FALSE
  ) |>
    purrr::pmap(\(gpa_bin, outcome) {
      data_sub <- matched |>
        mutate(
          gpa_bin = case_when(
            is.na(gpa) ~ NA_character_,
            gpa < 3.0 ~ NA_character_, # too few obs to estimate
            gpa >= 3.0 & gpa < 3.5 ~ "3.0 – 3.4",
            gpa >= 3.5 & gpa < 4.0 ~ "3.5 – 3.9",
            gpa >= 4.0 ~ "4.0+"
          )
        ) |>
        filter(gpa_bin == !!gpa_bin)

      # Require at least 20 treated obs AND total obs > number of predictors
      # to avoid rank-deficient models (e.g. PA 4.0+ bin with 17 treated)
      if (sum(data_sub$treated_in_year == 1) < 20) {
        return(NULL)
      }
      if (nrow(data_sub) < length(covars) + 5) {
        return(NULL)
      }

      fit_att(data_sub, outcome, covars) |>
        mutate(gpa_bin = gpa_bin, subsample = sample_label)
    }) |>
    purrr::compact() |>
    purrr::list_rbind() |>
    mutate(
      gpa_bin = factor(gpa_bin, levels = gpa_bin_levels),
      outcome_label = het_labels[outcome]
    )
}

results_gpa_all <- run_gpa_het(matched_all, base_covars, "All States")
results_gpa_pa <- run_gpa_het(matched_pa, pa_covars, "PA Public Schools")

results_gpa_het <- bind_rows(results_gpa_all, results_gpa_pa)

# Dot-and-whisker plot of ATT ± 95% CI by GPA bin, faceted by outcome and sample
results_gpa_het |>
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = c("Seamless STEM Enrollment", "Retained into 2nd Year")
    ),
    subsample = factor(subsample, levels = c("All States", "PA Public Schools"))
  ) |>
  ggplot(aes(x = gpa_bin, y = att_pp)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(ymin = conf_lo_pp, ymax = conf_hi_pp),
    width = 0.12,
    color = "#2c6e9e",
    linewidth = 0.9
  ) +
  geom_point(size = 4, color = "#f28e2b") +
  facet_grid(subsample ~ outcome_label) +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x), " pp"),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Program Effects on STEM Enrollment and Retention by High School GPA",
    subtitle = "Average treatment effect on the treated (ATT) with 95% confidence intervals",
    x = "High School GPA",
    y = "Treatment effect (percentage points)",
    caption = paste0(
      "Notes: Estimates from a linear probability model with matching weights and cohort fixed effects. ",
      "Students with missing GPA are excluded. ",
      "Wide confidence intervals reflect small within-GPA-bin sample sizes; interpret patterns with caution."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(
      color = "gray40",
      size = 10,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      color = "gray40",
      size = 8,
      hjust = 0,
      margin = margin(t = 10),
      lineheight = 1.3
    ),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = NA),
    plot.margin = margin(10, 15, 10, 15)
  )
