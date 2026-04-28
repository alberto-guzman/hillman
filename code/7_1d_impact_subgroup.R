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
#          output/att_subgroup_table_all_states.html/.tex
#          output/att_subgroup_table_pa.html/.tex
# =============================================================================

library(dplyr)
library(purrr)
library(tidyr)
library(gt)
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

  # (a) ITT enrollment — full subgroup sample (non-NSC coded 0)
  res_enroll <- map(panel_a_outcomes, \(out) {
    fit_att(data_sg, out, covars)
  }) |>
    list_rbind() |>
    mutate(sample = "full")

  # (b) Enrollment — NSC-matched students only
  data_nsc <- data_sg |> filter(has_nsc_record == 1)
  res_enroll_nsc <- map(panel_b_outcomes, \(out) {
    fit_att(data_nsc, out, covars)
  }) |>
    list_rbind() |>
    mutate(sample = "nsc_matched")

  # (c) Degree / persistence — non-censored subsample
  res_degree <- map(panel_c_outcomes, \(out) {
    data_sub <- data_sg |> filter(!is.na(.data[[out]]))
    if (out %in% c("reten_1y", "pers_1y")) {
      data_sub <- data_sub |> filter(hs_grad_year <= 2021)
    } else if (out %in% c("deg_any_ever", "deg_bach_ever", "deg_any_6y", "deg_bach_4y", "deg_bach_6y")) {
      data_sub <- data_sub |> filter(hs_grad_year <= 2022)
    }
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

# =============================================================================
# 6. SUBGROUP ATT TABLE
# =============================================================================
# One table per sample. Rows = subgroups × outcomes; columns match the main
# ATT table style. Suppressed subgroups appear as a single italicised note
# row rather than being dropped entirely so the reader knows they exist.
#
# Layout: one panel per outcome group (enrollment / NSC-matched / degree),
#   within each panel rows are grouped by subgroup.

make_subgroup_table <- function(results, title_subtitle) {
  sig_stars <- function(p) {
    case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.1 ~ "*",
      TRUE ~ ""
    )
  }

  fmt_pval <- function(p) {
    case_when(
      is.na(p) ~ "—",
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  }

  fmt_ci <- function(lo, hi) {
    if_else(
      is.na(lo) | is.na(hi),
      "—",
      paste0("[", sprintf("%.3f", lo), ", ", sprintf("%.3f", hi), "]")
    )
  }

  # Suppressed subgroups: one row per subgroup with n only
  suppressed <- results |>
    filter(suppressed) |>
    distinct(subgroup, subsample, n_treated, n_control) |>
    mutate(
      label = paste0(
        subgroup,
        " (n_treated = ",
        n_treated,
        "; insufficient sample for inference)"
      ),
      sample = "full",
      n_obs = n_treated + n_control,
      ctrl_mean_fmt = "—",
      trt_mean_fmt = "—",
      att_fmt = "—",
      se_fmt = "—",
      ci_fmt = "—",
      pval_fmt = "—",
      panel = "suppressed"
    ) |>
    select(
      subgroup,
      label,
      sample,
      n_obs,
      n_treated,
      n_control,
      ctrl_mean_fmt,
      trt_mean_fmt,
      att_fmt,
      se_fmt,
      ci_fmt,
      pval_fmt,
      panel
    )

  # Estimable subgroups
  estimable <- results |>
    filter(!suppressed, !is.na(outcome)) |>
    mutate(
      ctrl_mean_fmt = sprintf("%.3f", ctrl_mean),
      trt_mean_fmt = sprintf("%.3f", trt_mean),
      att_fmt = paste0(sprintf("%.3f", att), sig_stars(pval)),
      se_fmt = paste0("(", sprintf("%.3f", se), ")"),
      ci_fmt = fmt_ci(conf_lo, conf_hi),
      pval_fmt = fmt_pval(pval),
      panel = sample
    ) |>
    select(
      subgroup,
      label,
      sample,
      n_obs,
      n_treated,
      n_control,
      ctrl_mean_fmt,
      trt_mean_fmt,
      att_fmt,
      se_fmt,
      ci_fmt,
      pval_fmt,
      panel
    )

  panel_order <- c("full", "nsc_matched", "degree_eligible", "suppressed")

  tbl <- bind_rows(estimable, suppressed) |>
    mutate(panel = factor(panel, levels = panel_order)) |>
    arrange(panel, subgroup, label)

  n_rows <- nrow(tbl)

  tbl |>
    gt(rowname_col = "label", groupname_col = "subgroup") |>
    tab_header(
      title = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    tab_row_group(
      label = md(
        "**Suppressed Subgroups** *(n < 20 treated — insufficient for inference)*"
      ),
      rows = panel == "suppressed"
    ) |>
    tab_row_group(
      label = md(
        "**Panel C: Degree and Persistence Outcomes** *(non-censored subsample)*"
      ),
      rows = panel == "degree_eligible"
    ) |>
    tab_row_group(
      label = md(
        "**Panel B: Enrollment Outcomes** *(NSC-matched students only)*"
      ),
      rows = panel == "nsc_matched"
    ) |>
    tab_row_group(
      label = md(
        "**Panel A: Enrollment Outcomes** *(full matched subgroup sample)*"
      ),
      rows = panel == "full"
    ) |>
    cols_hide(c(panel, sample)) |>
    cols_label(
      n_obs = "N",
      n_treated = "Treated",
      n_control = "Control",
      ctrl_mean_fmt = "Control",
      trt_mean_fmt = "Treated",
      att_fmt = "ATT",
      se_fmt = "(SE)",
      ci_fmt = "95% CI",
      pval_fmt = "p-value"
    ) |>
    tab_spanner(
      label = "Sample Size",
      columns = c(n_obs, n_treated, n_control)
    ) |>
    tab_spanner(
      label = "Mean Outcome",
      columns = c(ctrl_mean_fmt, trt_mean_fmt)
    ) |>
    tab_spanner(
      label = "Treatment Effect",
      columns = c(att_fmt, se_fmt, ci_fmt, pval_fmt)
    ) |>
    cols_align(align = "left", columns = label) |>
    cols_align(
      align = "center",
      columns = c(
        n_obs,
        n_treated,
        n_control,
        ctrl_mean_fmt,
        trt_mean_fmt,
        att_fmt,
        se_fmt,
        ci_fmt,
        pval_fmt
      )
    ) |>
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = cells_body(rows = n_rows)
    ) |>
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = cell_text(style = "italic", color = "gray50"),
      locations = cells_body(rows = panel == "suppressed")
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "*Notes:* ATT = average treatment effect on the treated estimated via a ",
        "linear probability model (LPM) with matching weights and cohort fixed effects. ",
        "Standard errors (in parentheses) are HC2 heteroskedasticity-robust. ",
        "95% confidence intervals are reported in brackets. ",
        "All outcomes are binary (0/1); ATT and means are reported as proportions. ",
        "\\* p < 0.10, \\*\\* p < 0.05, \\*\\*\\* p < 0.01."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "Subgroup analyses use subsets of the already-matched dataset — no re-matching ",
        "is performed. Subgroups are defined by a value of 1 on the indicated indicator ",
        "variable. Panel A codes non-NSC-matched students as not enrolled; Panel B ",
        "restricts to NSC-matched students; Panel C restricts to students with ",
        "sufficient follow-up (non-missing outcome). ",
        "Subgroups with fewer than 20 treated students are reported in the suppressed ",
        "panel with sample sizes only."
      ))
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "Propensity scores estimated via logistic regression with nearest-neighbor ",
        "matching with replacement, caliper = 0.5 SD, exact match on application year. ",
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

# =============================================================================
# 7. BUILD AND SAVE TABLES
# =============================================================================

att_subgroup_gt_all <- make_subgroup_table(
  results_subgroup_all,
  c(
    "Subgroup Treatment Effects: Hillman Summer Program on College Outcomes",
    "All States — Propensity Score Matched Sample"
  )
)

att_subgroup_gt_pa <- make_subgroup_table(
  results_subgroup_pa,
  c(
    "Subgroup Treatment Effects: Hillman Summer Program on College Outcomes",
    "PA Public Schools — Propensity Score Matched Sample"
  )
)

att_subgroup_gt_all
att_subgroup_gt_pa

gtsave(
  att_subgroup_gt_all,
  here("output", "tables", "att_subgroup_table_all_states.html")
)
gtsave(att_subgroup_gt_all, here("output", "tables", "att_subgroup_table_all_states.tex"))
gtsave(att_subgroup_gt_pa, here("output", "tables", "att_subgroup_table_pa.html"))
gtsave(att_subgroup_gt_pa, here("output", "tables", "att_subgroup_table_pa.tex"))

message("Saved: att_subgroup_table_all_states.html/.tex")
message("Saved: att_subgroup_table_pa.html/.tex")
