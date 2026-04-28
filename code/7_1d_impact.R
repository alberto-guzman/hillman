# =============================================================================
# 7_1d_impact.R
#
# Purpose: Estimate ATT (average treatment effect on the treated) of the
#          Hillman summer program on college outcomes using doubly-robust
#          linear probability models with matching weights and HC2 robust SEs.
#
# Reference: https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
#
# Estimand: ATT via lm_robust() (estimatr) with matching weights and HC2 SEs.
#           HC2 chosen over subclass clustering given ~1.7 students/school
#           ratio in matched sample (near 1:1 student-to-cluster).
#
# Outcome panels:
#   Panel A — ITT enrollment outcomes (full matched sample; non-NSC = 0)
#   Panel B — NSC-conditional enrollment + institution outcomes
#             (has_nsc_record == 1)
#   Panel C — Persistence + degree outcomes
#             (enroll_ever == 1; cohort restrictions per outcome)
#   Panel D — STEM outcomes
#             (ITT enrollment: full sample; pers_1y_stem: enroll_ever == 1,
#              hs_grad_year <= 2021)
#
# Cohort restrictions:
#   reten_1y, pers_1y, pers_1y_stem — hs_grad_year <= 2021
#   degree outcomes                  — hs_grad_year <= 2022
#   Year 2022 excluded from matching (reten/pers all NA — data lag)
#   Year 2023 excluded from PA matching (only 1 treated student)
#
# Samples:
#   All states:      matched_all_states_year_only.rds
#   PA public schools: matched_pa_year_only.rds
#
# Output:  output/att_results_all_states.rds
#          output/att_results_pa.rds
#          output/att_table_all_states.html/.tex
#          output/att_table_pa.html/.tex
#          output/desc_by_year_all_states.html/.tex
#          output/desc_by_year_pa.html/.tex
#          output/het_plot_all_states.png
#          output/het_plot_pa.png
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(estimatr)
library(gt)
library(ggplot2)
library(here)

# =============================================================================
# 1. LOAD MATCHED DATASETS
# =============================================================================

matched_all <- readRDS(here("data", "matched", "matched_all_states_year_only.rds"))
matched_pa <- readRDS(here("data", "matched", "matched_pa_year_only.rds"))

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

# Panel A: ITT enrollment — all matched students; non-NSC-matches coded 0
panel_a_outcomes <- c(
  "enroll_seamless_itt",
  "enroll_ever_itt"
)

# Panel B: NSC-conditional enrollment + institution — has_nsc_record == 1
panel_b_outcomes <- c(
  "enroll_seamless",
  "enroll_ever",
  "enroll_firsttime_fulltime",
  "inst_4yr_entry",
  "inst_public4yr_entry",
  "inst_private4yr_entry",
  "inst_instate_entry",
  "enroll_delayed"
)

# Panel C: Persistence + degree — enroll_ever == 1; cohort restrictions apply
panel_c_outcomes <- c(
  "reten_1y",
  "pers_1y",
  "deg_any_ever",
  "deg_bach_ever",
  "deg_any_6y",
  "deg_bach_4y",
  "deg_bach_6y"
)

# Panel D: STEM outcomes
# ITT enrollment: full matched sample
# pers_1y_stem:   enroll_ever == 1 & hs_grad_year <= 2021
panel_d_outcomes <- c(
  "enroll_seamless_stem_itt",
  "enroll_ever_stem_itt",
  "enroll_seamless_stem",
  "enroll_ever_stem",
  "pers_1y_stem"
)

# ---------------------------------------------------------------------------
# Labels
# ---------------------------------------------------------------------------

outcome_labels <- c(
  # Panel A
  enroll_seamless_itt = "Seamless enrollment (ITT)",
  enroll_ever_itt = "Ever enrolled (ITT)",
  # Panel B
  enroll_seamless = "Seamless enrollment",
  enroll_ever = "Ever enrolled",
  enroll_firsttime_fulltime = "First-time full-time enrollment",
  inst_4yr_entry = "Initial enrollment: any 4-year",
  inst_public4yr_entry = "Initial enrollment: public 4-year",
  inst_private4yr_entry = "Initial enrollment: private 4-year",
  inst_instate_entry = "Initial enrollment: in-state",
  enroll_delayed = "Delayed enrollment",
  # Panel C
  reten_1y = "Retained into 2nd year",
  pers_1y = "Persisted to 2nd year (any inst.)",
  deg_any_ever = "Any degree ever",
  deg_bach_ever = "Bachelor's degree ever",
  deg_any_6y = "Any degree within 6 years",
  deg_bach_4y = "Bachelor's degree within 4 years",
  deg_bach_6y = "Bachelor's degree within 6 years",
  # Panel D
  enroll_seamless_stem_itt = "Seamless STEM enrollment (ITT)",
  enroll_ever_stem_itt = "Ever enrolled STEM (ITT)",
  enroll_seamless_stem = "Seamless STEM enrollment",
  enroll_ever_stem = "Ever enrolled STEM",
  pers_1y_stem = "Persisted in STEM to 2nd year"
)

# ---------------------------------------------------------------------------
# Covariates — must match PS model exactly (same covars + _miss indicators)
# Grade dummies used (consistent with PS model); raw grade dropped
# ---------------------------------------------------------------------------

base_covars <- c(
  "gender",
  "grade_9",
  "grade_10",
  "grade_11",
  "grade_12",
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
  "us_citizen",
  "first_gen",
  # missing indicators — only included where variation exists in matched sample
  # gender_miss, stipend_miss, us_citizen_miss excluded (one unique value — no variation)
  "gpa_miss",
  "psat_math_miss",
  "house_size_miss",
  "racially_marginalized_miss",
  "bi_multi_racial_miss",
  "urban_miss",
  "suburban_miss",
  "rural_miss",
  "disability_miss",
  "neg_school_miss",
  "first_gen_miss"
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
# 3. HELPER: FIT LPM + EXTRACT ATT
# =============================================================================
# Fits a weighted LPM with HC2 robust SEs via lm_robust(). Year FEs included.
# Constant predictors in a subsample are dropped automatically to avoid
# rank-deficient models (common in small subgroup analyses).

fit_att <- function(data, outcome_var, predictors) {
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

  fit <- lm_robust(
    as.formula(formula_str),
    data = data,
    weights = weights,
    se_type = "HC2"
  )

  coef_row <- summary(fit)$coefficients["treated_in_year", ]

  # HC2 breaks down when any observation has leverage h_ii = 1 (small matched
  # subsamples with replacement). Fall back to HC1 and warn so the caller knows.
  if (is.nan(coef_row["Std. Error"])) {
    warning(
      "HC2 SE is NaN for outcome '", outcome_var,
      "' (n=", nrow(data), ") — falling back to HC1"
    )
    fit <- lm_robust(
      as.formula(formula_str),
      data = data,
      weights = weights,
      se_type = "HC1"
    )
    coef_row <- summary(fit)$coefficients["treated_in_year", ]
  }

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
    att = coef_row["Estimate"],
    se = coef_row["Std. Error"],
    pval = coef_row["Pr(>|t|)"],
    conf_lo = coef_row["CI Lower"],
    conf_hi = coef_row["CI Upper"]
  )
}

# =============================================================================
# 4. HELPER: RUN ALL OUTCOME PANELS FOR ONE SAMPLE
# =============================================================================

run_all_outcomes <- function(matched, covars, sample_label) {
  # Panel A: ITT enrollment — full matched sample
  message(
    "\n--- ",
    sample_label,
    ": Panel A — ITT enrollment (full sample) ---"
  )
  results_a <- map(panel_a_outcomes, \(out) {
    message("  Fitting: ", out)
    fit_att(matched, out, covars)
  }) |>
    list_rbind() |>
    mutate(panel = "A")

  # Panel B: NSC-conditional enrollment — has_nsc_record == 1
  message("\n--- ", sample_label, ": Panel B — NSC-conditional enrollment ---")
  matched_nsc <- matched |> filter(has_nsc_record == 1)
  message(
    "  NSC subsample: ",
    nrow(matched_nsc),
    " (",
    sum(matched_nsc$treated_in_year == 1),
    " treated / ",
    sum(matched_nsc$treated_in_year == 0),
    " control)"
  )
  results_b <- map(panel_b_outcomes, \(out) {
    message("  Fitting: ", out)
    fit_att(matched_nsc, out, covars)
  }) |>
    list_rbind() |>
    mutate(panel = "B")

  # Panel C: Persistence + degree — enroll_ever == 1; cohort restrictions
  message("\n--- ", sample_label, ": Panel C — persistence + degree ---")
  results_c <- map(panel_c_outcomes, \(out) {
    message("  Fitting: ", out)

    data_sub <- matched |>
      filter(enroll_ever == 1, !is.na(.data[[out]]))

    # Cohort restrictions: reten/pers need fall follow-up through 2022;
    # degree outcomes need sufficient time to complete (≤ 2022 for 6-year rates).
    if (out %in% c("reten_1y", "pers_1y")) {
      data_sub <- data_sub |> filter(hs_grad_year <= 2021)
    } else if (out %in% c("deg_any_ever", "deg_bach_ever", "deg_any_6y", "deg_bach_4y", "deg_bach_6y")) {
      data_sub <- data_sub |> filter(hs_grad_year <= 2022)
    }

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

  # Panel D: STEM outcomes
  message("\n--- ", sample_label, ": Panel D — STEM outcomes ---")
  results_d <- map(panel_d_outcomes, \(out) {
    message("  Fitting: ", out)

    data_sub <- if (
      out %in% c("enroll_seamless_stem_itt", "enroll_ever_stem_itt")
    ) {
      # ITT: full matched sample
      matched
    } else if (out == "pers_1y_stem") {
      # Persistence in STEM: enrolled, cohort <= 2021
      matched |>
        filter(enroll_ever == 1, hs_grad_year <= 2021, !is.na(.data[[out]]))
    } else {
      # NSC-conditional STEM enrollment: has_nsc_record == 1
      matched |> filter(has_nsc_record == 1)
    }

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
    mutate(panel = "D")

  bind_rows(results_a, results_b, results_c, results_d) |>
    mutate(subsample = sample_label)
}

# =============================================================================
# 5. RUN OUTCOME MODELS
# =============================================================================

results_all <- run_all_outcomes(matched_all, base_covars, "All States")
results_pa <- run_all_outcomes(matched_pa, pa_covars, "PA Public Schools")

# Quick inspect
results_all |>
  select(panel, label, n_treated, n_control, ctrl_mean, trt_mean, att, se, pval)

results_pa |>
  select(panel, label, n_treated, n_control, ctrl_mean, trt_mean, att, se, pval)

# =============================================================================
# 6. MAKE ATT TABLE (gt)
# =============================================================================
# Four panels: A (ITT enrollment), B (NSC enrollment + inst), C (persistence
# + degree), D (STEM). ATT and means reported as proportions.

make_att_table <- function(results, title_subtitle) {
  fmt_pval <- function(p) {
    case_when(p < 0.001 ~ "<0.001", TRUE ~ sprintf("%.3f", p))
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
        att_fmt = paste0(sprintf("%.3f", att), sig_stars(pval)),
        se_fmt = paste0("(", sprintf("%.3f", se), ")"),
        ci_fmt = paste0(
          "[",
          sprintf("%.3f", conf_lo),
          ", ",
          sprintf("%.3f", conf_hi),
          "]"
        ),
        pval_fmt = fmt_pval(pval)
      ) |>
      select(
        panel,
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

  tbl <- results |>
    prep_panel() |>
    arrange(panel)

  last_row <- nrow(tbl)

  tbl |>
    gt(rowname_col = "label") |>
    tab_header(
      title = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    tab_row_group(
      label = md("**Panel D: STEM Outcomes**"),
      rows = panel == "D"
    ) |>
    tab_row_group(
      label = md(
        "**Panel C: Persistence and Degree Outcomes** *(enrolled students; cohort restrictions apply)*"
      ),
      rows = panel == "C"
    ) |>
    tab_row_group(
      label = md(
        "**Panel B: NSC-Conditional Enrollment and Institution Outcomes** *(NSC-matched students only)*"
      ),
      rows = panel == "B"
    ) |>
    tab_row_group(
      label = md(
        "**Panel A: ITT Enrollment Outcomes** *(full matched sample; non-NSC-matches coded 0)*"
      ),
      rows = panel == "A"
    ) |>
    cols_hide(panel) |>
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
    cols_align(align = "left", columns = label) |>
    cols_align(align = "center", columns = -label) |>
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
    tab_source_note(
      source_note = md(paste0(
        "*Notes:* ATT = average treatment effect on the treated, estimated via a ",
        "linear probability model (LPM) with matching weights, cohort fixed effects, ",
        "and HC2 heteroskedasticity-robust standard errors. ",
        "All outcomes are binary (0/1); ATT and means are reported as proportions. ",
        "\\* p < 0.10, \\*\\* p < 0.05, \\*\\*\\* p < 0.01."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "Panel A uses all matched students with non-NSC-matched students coded as ",
        "non-enrolled (ITT estimand). ",
        "Panel B restricts to NSC-matched students. ",
        "Panel C restricts to enrolled students (enroll_ever == 1); reten_1y and ",
        "pers_1y further restricted to cohorts 2018–2021 (2022 excluded — no fall ",
        "2023 follow-up in NSC file). Degree outcomes restricted to cohorts 2018–2022. ",
        "Panel D STEM enrollment uses ITT (full sample) or NSC-conditional sample; ",
        "pers_1y_stem restricted to enrolled students, cohorts 2018–2021."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "Propensity scores estimated via logistic regression with nearest-neighbor ",
        "matching with replacement, caliper = 0.5 SD, exact match on application year. ",
        "Year 2022 excluded from matching (insufficient follow-up for retention outcomes). ",
        "Year 2023 excluded from PA matching (only 1 treated student)."
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
  results_all,
  c(
    "Table 2: Effects of the Hillman Summer Program on College Outcomes",
    "All States — Propensity Score Matched Sample"
  )
)

att_gt_pa <- make_att_table(
  results_pa,
  c(
    "Table 3: Effects of the Hillman Summer Program on College Outcomes",
    "PA Public Schools — Propensity Score Matched Sample"
  )
)

att_gt_all
att_gt_pa

# =============================================================================
# 7. DESCRIPTIVE TABLE BY COHORT
# =============================================================================
# Weighted means by year and treatment status for focal outcomes.
# No inference — purely descriptive.

focal_outcomes <- c(
  "enroll_seamless_itt",
  "enroll_seamless_stem_itt",
  "inst_4yr_entry",
  "inst_public4yr_entry",
  "reten_1y",
  "deg_any_6y"
)

focal_labels <- c(
  enroll_seamless_itt = "Seamless enrollment (ITT)",
  enroll_seamless_stem_itt = "Seamless STEM enrollment (ITT)",
  inst_4yr_entry = "Initial enrollment: any 4-year",
  inst_public4yr_entry = "Initial enrollment: public 4-year",
  reten_1y = "Retained into 2nd year",
  deg_any_6y = "Any degree within 6 years"
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

  # t() gives year-grouped order: 2017_n_trt, 2017_n_ctrl, ..., 2018_n_trt, ...
  # Without t(), as.vector reads column-major (stats-grouped), scrambling labels.
  year_stat_cols <- c(t(outer(
    as.character(years),
    c("n_trt", "n_ctrl", "ctrl_fmt", "trt_fmt", "diff_fmt"),
    paste,
    sep = "_"
  )))

  ordered_cols <- c("outcome_label", year_stat_cols)
  tbl <- tbl |> select(all_of(ordered_cols))
  n_rows <- nrow(tbl)

  label_list <- setNames(
    rep(list("NT", "NC", "Control", "Treated", "Diff."), length(years)),
    year_stat_cols
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
        "Year 2022 excluded from matching (insufficient follow-up for retention). ",
        "Year 2023 excluded from PA sample (only 1 treated student)."
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

desc_gt_all
desc_gt_pa

# =============================================================================
# 8. HETEROGENEITY ANALYSIS
# =============================================================================
# Re-run fit_att() within subgroups for two focal outcomes:
#   - enroll_seamless_stem_itt (STEM enrollment, Panel D)
#   - pers_1y_stem             (STEM persistence, Panel D)
#
# Subgroups: racially_marginalized, first_gen, gender, urban/rural
# Results visualised as faceted dot-and-whisker plots.
# Small within-subgroup samples produce wide CIs — interpret with caution.

het_outcomes <- c("enroll_seamless_stem_itt", "pers_1y_stem")

het_labels <- c(
  enroll_seamless_stem_itt = "Seamless STEM Enrollment (ITT)",
  pers_1y_stem = "Persisted in STEM to 2nd Year"
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
    list(
      name = "first_gen",
      var = "first_gen",
      levels = list("First generation" = 1, "Not first generation" = 0)
    ),
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

        # Apply same cohort restriction as main analysis for pers_1y_stem
        if (out == "pers_1y_stem") {
          data_sub <- data_sub |>
            filter(enroll_ever == 1, hs_grad_year <= 2021, !is.na(.data[[out]]))
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
results_het_pa <- run_het(matched_pa, pa_covars, "PA Public Schools")

results_het <- bind_rows(results_het_all, results_het_pa) |>
  mutate(
    outcome_label = het_labels[outcome],
    subgroup_label = factor(
      subgroup_label,
      levels = c(
        "Racially marginalized",
        "Not racially marginalized",
        "First generation",
        "Not first generation",
        "Female",
        "Male",
        "Urban",
        "Rural"
      )
    ),
    subsample = factor(subsample, levels = c("All States", "PA Public Schools"))
  )

# Dot-and-whisker plot faceted by outcome and sample
het_plot <- results_het |>
  ggplot(aes(x = subgroup_label, y = att)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(ymin = conf_lo, ymax = conf_hi),
    width = 0.15,
    color = "#2c6e9e",
    linewidth = 0.8
  ) +
  geom_point(size = 3.5, color = "#f28e2b") +
  facet_grid(subsample ~ outcome_label, scales = "free_y") +
  scale_y_continuous(
    labels = \(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 1), " pp"),
    breaks = scales::pretty_breaks(n = 5)
  ) +
  labs(
    title = "Heterogeneity in Program Effects by Subgroup",
    subtitle = "ATT with 95% confidence intervals",
    x = NULL,
    y = "Treatment effect (percentage points)",
    caption = paste0(
      "Notes: Estimates from a linear probability model with matching weights, ",
      "cohort fixed effects, and HC2 robust SEs. ",
      "Subgroups with fewer than 15 treated students are excluded. ",
      "Interpret wide CIs with caution — small within-subgroup samples."
    )
  ) +
  theme_minimal(base_size = 12) +
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
    axis.text.x = element_text(size = 9, angle = 30, hjust = 1),
    axis.title.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA),
    plot.margin = margin(10, 15, 10, 15)
  )

het_plot

# =============================================================================
# 9. SAVE ALL OUTPUTS
# =============================================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# ATT tables
gtsave(att_gt_all, here("output", "tables", "att_table_all_states.html"))
gtsave(att_gt_all, here("output", "tables", "att_table_all_states.tex"))
gtsave(att_gt_pa, here("output", "tables", "att_table_pa.html"))
gtsave(att_gt_pa, here("output", "tables", "att_table_pa.tex"))

# Descriptive tables
gtsave(desc_gt_all, here("output", "tables", "desc_by_year_all_states.html"))
gtsave(desc_gt_all, here("output", "tables", "desc_by_year_all_states.tex"))
gtsave(desc_gt_pa, here("output", "tables", "desc_by_year_pa.html"))
gtsave(desc_gt_pa, here("output", "tables", "desc_by_year_pa.tex"))

# Heterogeneity plot
ggsave(
  here("output", "figures", "het_plot.png"),
  het_plot,
  width = 12,
  height = 7,
  dpi = 300
)

# Results RDS
saveRDS(results_all, here("output", "att_results_all_states.rds"))
saveRDS(results_pa, here("output", "att_results_pa.rds"))
saveRDS(results_het, here("output", "att_results_het.rds"))

message("\n=== Impact analysis complete ===")
message("Saved: att_table_all_states.html/.tex")
message("Saved: att_table_pa.html/.tex")
message("Saved: desc_by_year_all_states.html/.tex")
message("Saved: desc_by_year_pa.html/.tex")
message("Saved: het_plot.png")
message(
  "Saved: att_results_all_states.rds / att_results_pa.rds / att_results_het.rds"
)
