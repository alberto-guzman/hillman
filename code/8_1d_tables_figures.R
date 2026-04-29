# =============================================================================
# 8_1d_tables_figures.R
#
# Purpose: Produce all publication-ready tables and figures for the Hillman
#          summer program impact study in EEPA (APA 7th) journal style.
#
# Style:   Three-rule tables (top/bottom/below column headers), no vertical
#          lines, "Note." footnotes, *p < .05 **p < .01 ***p < .001 convention
#          (EEPA uses .05/.01/.001, not .10). No shading except row-group bands.
#
# Tables:
#   Table 1  — Descriptive statistics: treated vs. control, overall + by cohort
#   Table 2  — Covariate balance: All States (before/after matching, SMDs)
#   Table 3  — ATT results: All States (Panels A–D)
#   Table 4  — ATT results: PA Public Schools (Panels A–D)
#   App. A1  — Covariate balance: PA Public Schools
#   App. A2  — Sample sizes by cohort (before/after matching)
#
# Figures:
#   Figure 1 — Love plot: SMD before/after matching (All States)
#   Figure 2 — Subgroup coefficient plots (seamless enrollment + STEM)
#
# Input:
#   data/matched/matched_all_states_year_only.rds
#   data/matched/matched_pa_year_only.rds
#   data/matched/matchit_object_all_states.rds
#   data/matched/matchit_object_pa.rds
#   data/matched/matching_data_all_states.rds
#   data/matched/matching_data_pa.rds
#   output/att_results_all_states.rds
#   output/att_results_pa.rds
#   output/att_subgroup_all_states.rds
#   output/att_subgroup_pa.rds
#
# Output:
#   output/tables/table_1_descriptive.html
#   output/tables/table_2_balance_all_states.html
#   output/tables/table_3_att_all_states.html
#   output/tables/table_4_att_pa.html
#   output/tables/appendix_a1_balance_pa.html
#   output/tables/appendix_a2_sample_sizes.html
#   output/figures/figure_1_love_plot.png
#   output/figures/figure_2_subgroup_effects.png
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(gt)
library(cobalt)
library(here)

dir.create(here("output", "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. LOAD ALL INPUTS
# =============================================================================

matched_all      <- readRDS(here("data", "matched", "matched_all_states_year_only.rds"))
matched_pa       <- readRDS(here("data", "matched", "matched_pa_year_only.rds"))
m_out_all        <- readRDS(here("data", "matched", "matchit_object_all_states.rds"))
m_out_pa         <- readRDS(here("data", "matched", "matchit_object_pa.rds"))
pre_all          <- readRDS(here("data", "matched", "matching_data_all_states.rds"))
pre_pa           <- readRDS(here("data", "matched", "matching_data_pa.rds"))
results_all      <- readRDS(here("output", "att_results_all_states.rds"))
results_pa       <- readRDS(here("output", "att_results_pa.rds"))
results_sg_all   <- readRDS(here("output", "att_subgroup_all_states.rds"))
results_sg_pa    <- readRDS(here("output", "att_subgroup_pa.rds"))

# =============================================================================
# 2. SHARED HELPERS
# =============================================================================

sig_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

fmt_pval <- function(p) {
  case_when(
    is.na(p)   ~ "—",
    p < 0.001  ~ "<.001",
    TRUE       ~ sub("^0\\.", ".", sprintf("%.3f", p))
  )
}

# Shared gt table options — EEPA three-rule style
eepa_options <- function(gt_tbl) {
  gt_tbl |>
    tab_options(
      table.font.size              = px(11),
      heading.title.font.size      = px(13),
      heading.subtitle.font.size   = px(11),
      heading.align                = "left",
      column_labels.font.weight    = "bold",
      column_labels.border.top.color    = "black",
      column_labels.border.top.width    = px(2),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = px(1),
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(2),
      row_group.background.color   = "gray97",
      row_group.border.top.color   = "black",
      row_group.border.top.width   = px(1),
      row_group.border.bottom.color = "gray80",
      row_group.border.bottom.width = px(1),
      table.border.top.color       = "white",
      table.border.bottom.color    = "white",
      source_notes.font.size       = px(10),
      data_row.padding             = px(3)
    )
}

# Add top/bottom rules to body rows
add_body_rules <- function(gt_tbl, n_rows) {
  gt_tbl |>
    tab_style(
      style     = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style     = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = cells_body(rows = n_rows)
    )
}

# =============================================================================
# 3. TABLE 1 — DESCRIPTIVE STATISTICS
# =============================================================================
# Pre-match analytic sample. Rows = covariates + cohort composition.
# Columns = N, Control Mean, Treated Mean, Standardized Difference.
# Source: pre_all (matching_data_all_states.rds) — same exclusions as matching.

message("\n=== Table 1: Descriptive Statistics ===")

desc_vars <- c(
  "gender",
  "grade_9", "grade_10", "grade_11", "grade_12",
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
  "neg_school"
  # us_citizen excluded: non-citizens are removed before matching, so the
  # column is 1 or NA for everyone in pre_all — uninformative to display.
  # first_gen excluded: 100% missing on the 2017–2018 application forms;
  # included it in earlier specs but the missing-indicator collinear with
  # year FE made it uninformative. Removed from the main analysis.
)

desc_labels <- c(
  gender             = "Female (= 1)",
  grade_9            = "Grade 9",
  grade_10           = "Grade 10",
  grade_11           = "Grade 11",
  grade_12           = "Grade 12",
  gpa                = "High school GPA",
  psat_math          = "PSAT math score",
  stipend            = "Stipend eligible",
  house_size         = "Household size",
  racially_marginalized = "Racially marginalized",
  bi_multi_racial    = "Bi/multi-racial",
  urban              = "Urban",
  suburban           = "Suburban",
  rural              = "Rural",
  disability         = "Documented disability",
  neg_school         = "Negative school environment"
)

# Group labels used for row grouping
desc_group <- c(
  gender             = "Demographics",
  grade_9            = "Grade Level",
  grade_10           = "Grade Level",
  grade_11           = "Grade Level",
  grade_12           = "Grade Level",
  gpa                = "Academic",
  psat_math          = "Academic",
  stipend            = "Socioeconomic",
  house_size         = "Socioeconomic",
  racially_marginalized = "Race/Ethnicity",
  bi_multi_racial    = "Race/Ethnicity",
  urban              = "Geography",
  suburban           = "Geography",
  rural              = "Geography",
  disability         = "Student Background",
  neg_school         = "Student Background"
)

make_desc_table <- function(pre_data, title_subtitle) {
  ctrl <- pre_data |> filter(treated_in_year == 0)
  trt  <- pre_data |> filter(treated_in_year == 1)

  n_ctrl <- nrow(ctrl)
  n_trt  <- nrow(trt)

  means_ctrl <- ctrl |>
    summarise(across(all_of(desc_vars), ~ mean(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "variable", values_to = "ctrl_mean")

  means_trt <- trt |>
    summarise(across(all_of(desc_vars), ~ mean(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "variable", values_to = "trt_mean")

  sd_ctrl <- ctrl |>
    summarise(across(all_of(desc_vars), ~ sd(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "variable", values_to = "sd_ctrl")

  sd_trt <- trt |>
    summarise(across(all_of(desc_vars), ~ sd(., na.rm = TRUE))) |>
    pivot_longer(everything(), names_to = "variable", values_to = "sd_trt")

  sd_pooled <- sd_ctrl |>
    left_join(sd_trt, by = "variable") |>
    mutate(sd_pool = sqrt((sd_ctrl^2 + sd_trt^2) / 2))

  tbl_cov <- means_ctrl |>
    left_join(means_trt,  by = "variable") |>
    left_join(sd_pooled,  by = "variable") |>
    mutate(
      std_diff = (trt_mean - ctrl_mean) / sd_pool,
      label    = desc_labels[variable],
      group    = desc_group[variable],
      ctrl_fmt = sprintf("%.3f", ctrl_mean),
      trt_fmt  = sprintf("%.3f", trt_mean),
      std_fmt  = if_else(is.na(std_diff), "—", sprintf("%.3f", std_diff))
    ) |>
    arrange(match(variable, desc_vars)) |>
    select(group, label, ctrl_fmt, trt_fmt, std_fmt)

  # Cohort composition block
  cohort_ctrl <- ctrl |>
    count(year) |>
    mutate(pct = n / sum(n) * 100, group = "Cohort", label = as.character(year),
           ctrl_fmt = sprintf("%.1f%%", pct), trt_fmt = "", std_fmt = "") |>
    select(group, label, ctrl_fmt, trt_fmt, std_fmt)

  cohort_trt <- trt |>
    count(year) |>
    mutate(pct = n / sum(n) * 100, group = "Cohort", label = as.character(year)) |>
    select(year, trt_pct = pct)

  cohort_combined <- cohort_ctrl |>
    left_join(
      cohort_trt |> mutate(label = as.character(year)) |> select(label, trt_pct),
      by = "label"
    ) |>
    mutate(trt_fmt = sprintf("%.1f%%", coalesce(trt_pct, 0))) |>
    select(group, label, ctrl_fmt, trt_fmt, std_fmt)

  tbl <- bind_rows(tbl_cov, cohort_combined)
  n_rows <- nrow(tbl)

  header_note <- paste0(
    "N (control) = ", n_ctrl, "; N (treated) = ", n_trt, "."
  )

  tbl |>
    gt(rowname_col = "label", groupname_col = "group") |>
    tab_header(
      title    = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    cols_label(
      ctrl_fmt = md(paste0("Control<br>(*n* = ", n_ctrl, ")")),
      trt_fmt  = md(paste0("Treated<br>(*n* = ", n_trt, ")")),
      std_fmt  = "Std. Diff."
    ) |>
    cols_align(align = "left",   columns = label) |>
    cols_align(align = "center", columns = c(ctrl_fmt, trt_fmt, std_fmt)) |>
    tab_style(
      style     = cell_text(style = "italic"),
      locations = cells_row_groups()
    ) |>
    tab_source_note(source_note = md(paste0(
      "*Note.* GPA, PSAT math score, and household size are reported as means; ",
      "all other variables are binary (0/1) and reported as proportions. ",
      "Std. Diff. = (treated mean − control mean) / pooled SD, where pooled SD = ",
      "√[(SD²_ctrl + SD²_trt) / 2]. ",
      "Sample is the pre-match analytic sample after applying exclusions ",
      "(year 2022, confirmed non-citizens, treated before 2017). ",
      header_note
    ))) |>
    add_body_rules(n_rows) |>
    eepa_options()
}

table_1 <- make_desc_table(
  pre_all,
  c(
    "Table 1",
    "Descriptive Statistics by Treatment Status — All States, Pre-Match Analytic Sample"
  )
)
table_1

# =============================================================================
# 4. TABLE 2 / APPENDIX A1 — COVARIATE BALANCE
# =============================================================================
# Covariate means before and after matching with SMDs.

message("\n=== Table 2: Balance (All States) ===")
message("\n=== Appendix A1: Balance (PA) ===")

bal_labels <- c(
  gender                        = "Female",
  gpa                           = "High school GPA",
  psat_math                     = "PSAT math score",
  stipend                       = "Stipend eligible",
  house_size                    = "Household size",
  racially_marginalized         = "Racially marginalized",
  bi_multi_racial               = "Bi/multi-racial",
  urban                         = "Urban",
  suburban                      = "Suburban",
  rural                         = "Rural",
  disability                    = "Documented disability",
  neg_school                    = "Negative school environment",
  us_citizen                    = "U.S. citizen",
  grade_9                       = "Grade 9",
  grade_10                      = "Grade 10",
  grade_11                      = "Grade 11",
  grade_12                      = "Grade 12",
  school_enrollment             = "School enrollment",
  school_pct_econ_disadvantaged = "School % economically disadvantaged",
  school_pct_english_learner    = "School % English learner",
  school_pct_special_ed         = "School % special education",
  school_pct_white              = "School % white"
)

make_balance_table <- function(m_out, pre_data, matched_data, display_vars, title_subtitle) {
  bal <- bal.tab(m_out, un = TRUE, disp = "means")$Balance |>
    tibble::rownames_to_column("variable") |>
    filter(variable %in% display_vars) |>
    select(variable,
           pre_ctrl = M.0.Un, pre_trt = M.1.Un, smd_pre = Diff.Un,
           post_ctrl = M.0.Adj, post_trt = M.1.Adj, smd_post = Diff.Adj)

  n_pre_ctrl  <- sum(pre_data$treated_in_year == 0)
  n_pre_trt   <- sum(pre_data$treated_in_year == 1)
  n_post_ctrl <- sum(matched_data$treated_in_year == 0)
  n_post_trt  <- sum(matched_data$treated_in_year == 1)

  tbl <- bal |>
    mutate(
      label        = coalesce(bal_labels[variable], variable),
      pre_ctrl_f   = sprintf("%.3f", pre_ctrl),
      pre_trt_f    = sprintf("%.3f", pre_trt),
      smd_pre_f    = sprintf("%.3f", smd_pre),
      post_ctrl_f  = sprintf("%.3f", post_ctrl),
      post_trt_f   = sprintf("%.3f", post_trt),
      smd_post_f   = sprintf("%.3f", smd_post)
    ) |>
    arrange(match(variable, display_vars)) |>
    select(label, pre_ctrl_f, pre_trt_f, smd_pre_f,
           post_ctrl_f, post_trt_f, smd_post_f)

  n_rows <- nrow(tbl)

  tbl |>
    gt(rowname_col = "label") |>
    tab_header(
      title    = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    tab_spanner(
      label   = md(paste0("Before Matching<br>Control *n* = ", n_pre_ctrl,
                           ", Treated *n* = ", n_pre_trt)),
      columns = c(pre_ctrl_f, pre_trt_f, smd_pre_f)
    ) |>
    tab_spanner(
      label   = md(paste0("After Matching<br>Control *n* = ", n_post_ctrl,
                           ", Treated *n* = ", n_post_trt)),
      columns = c(post_ctrl_f, post_trt_f, smd_post_f)
    ) |>
    cols_label(
      pre_ctrl_f  = "Control",
      pre_trt_f   = "Treated",
      smd_pre_f   = "SMD",
      post_ctrl_f = "Control",
      post_trt_f  = "Treated",
      smd_post_f  = "SMD"
    ) |>
    cols_align(align = "left",   columns = label) |>
    cols_align(align = "center", columns = -label) |>
    tab_source_note(source_note = md(paste0(
      "*Note.* SMD = standardized mean difference computed by cobalt::bal.tab(). ",
      "|SMD| < 0.10 indicates adequate balance. Post-match means weighted by ",
      "matching weights. Matching: nearest-neighbor with replacement, caliper = 0.5 SD, ",
      "exact match on application year."
    ))) |>
    add_body_rules(n_rows) |>
    eepa_options()
}

display_vars_all <- c(
  "gender", "gpa", "psat_math", "stipend", "house_size",
  "racially_marginalized", "bi_multi_racial",
  "urban", "suburban", "rural",
  "disability", "neg_school", "us_citizen",
  paste0("grade_", 9:12)
)

display_vars_pa <- c(
  display_vars_all,
  "school_enrollment", "school_pct_econ_disadvantaged",
  "school_pct_english_learner", "school_pct_special_ed", "school_pct_white"
)

table_2 <- make_balance_table(
  m_out_all, pre_all, matched_all, display_vars_all,
  c("Table 2",
    "Covariate Balance Before and After Matching — All States")
)
table_2

appendix_a1 <- make_balance_table(
  m_out_pa, pre_pa, matched_pa, display_vars_pa,
  c("Appendix Table A1",
    "Covariate Balance Before and After Matching — PA Public Schools")
)
appendix_a1

# =============================================================================
# 5. TABLE 3 / TABLE 4 — MAIN ATT RESULTS
# =============================================================================
# Three panels: A (ITT enrollment, incl. STEM), B (institution at entry),
# C (persistence + degree, incl. STEM). 9 outcomes total. ATT reported as
# proportions.

message("\n=== Table 3: ATT All States ===")
message("\n=== Table 4: ATT PA ===")

make_att_table <- function(results, matched, title_subtitle, pa_note = NULL) {
  tbl <- results |>
    mutate(
      att_fmt  = paste0(sprintf("%.3f", att), sig_stars(pval)),
      se_fmt   = paste0("(", sprintf("%.3f", se), ")"),
      ci_fmt   = paste0("[", sprintf("%.3f", conf_lo), ", ",
                         sprintf("%.3f", conf_hi), "]"),
      pval_fmt = fmt_pval(pval),
      ctrl_fmt = sprintf("%.3f", ctrl_mean),
      trt_fmt  = sprintf("%.3f", trt_mean)
    )

  panel_labels <- c(
    A = "**Panel A: Enrollment Outcomes** *(NSC-matched students)*",
    B = "**Panel B: Institution at Entry** *(NSC-matched students)*",
    C = "**Panel C: Persistence and Degree Outcomes** *(NSC-matched and enrolled students)*"
  )

  n_rows   <- nrow(tbl)
  n_trt    <- sum(matched$treated_in_year == 1)
  n_ctrl   <- sum(matched$treated_in_year == 0)

  gt_tbl <- tbl |>
    select(panel, label, n_obs, n_treated, ctrl_fmt, trt_fmt,
           att_fmt, se_fmt, ci_fmt, pval_fmt) |>
    gt(rowname_col = "label") |>
    tab_header(
      title    = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    )

  for (p in rev(c("A", "B", "C"))) {
    gt_tbl <- gt_tbl |>
      tab_row_group(
        label = md(panel_labels[p]),
        rows  = panel == p
      )
  }

  gt_tbl <- gt_tbl |>
    cols_hide(panel) |>
    cols_label(
      n_obs     = "*N*",
      n_treated = "Treated",
      ctrl_fmt  = "Control",
      trt_fmt   = "Treated",
      att_fmt   = "ATT",
      se_fmt    = "(*SE*)",
      ci_fmt    = "95% CI",
      pval_fmt  = "*p*"
    ) |>
    tab_spanner(label = "Sample Size",    columns = c(n_obs, n_treated)) |>
    tab_spanner(label = "Mean Outcome",   columns = c(ctrl_fmt, trt_fmt)) |>
    tab_spanner(label = "Treatment Effect", columns = c(att_fmt, se_fmt, ci_fmt, pval_fmt)) |>
    cols_align(align = "left",   columns = label) |>
    cols_align(align = "center", columns = -label) |>
    tab_style(
      style     = cell_text(style = "italic"),
      locations = cells_row_groups()
    ) |>
    tab_source_note(source_note = md(paste0(
      "*Note.* ATT = average treatment effect on the treated estimated via a ",
      "linear probability model (LPM) with matching weights and cohort fixed effects. ",
      "Standard errors (in parentheses) are HC2 heteroskedasticity-robust (HC1 fallback ",
      "if HC2 is undefined due to leverage = 1). ",
      "All outcomes are binary (0/1); ATT and means are proportions. ",
      "95% CIs in brackets. ",
      if (!is.null(pa_note)) paste0(pa_note, " ") else "",
      "Matched sample: N treated = ", n_trt, ", N control = ", n_ctrl, "."
    ))) |>
    tab_source_note(source_note = md(
      "\\*p < .05. \\*\\*p < .01. \\*\\*\\*p < .001."
    )) |>
    tab_source_note(source_note = md(paste0(
      "Analytic sample is restricted to HS graduating cohorts 2018–2021. The 2022 ",
      "and 2023 cohorts are excluded because the NSC query that produced the ",
      "outcome data has incomplete (2022) or no (2023) enrollment coverage. ",
      "All panels condition on has_nsc_record = 1 (the student appears in the NSC ",
      "query). Students not appearing in NSC went to college per Hillman's program ",
      "records but their institutions do not report to NSC, so enrollment cannot ",
      "be reliably inferred for them. ",
      "Panel C additionally conditions on enroll_ever = 1 (NSC-recorded enrollment). ",
      "Bachelor's window reflects Danielle's NSC loop range (0/6 inclusive of ",
      "year 0): deg_bach_6y captures any bachelor's earned within 7 years of HS ",
      "graduation. Degree outcomes are right-censored for cohorts younger than ",
      "the full window."
    ))) |>
    add_body_rules(n_rows) |>
    eepa_options()

  gt_tbl
}

table_3 <- make_att_table(
  results_all, matched_all,
  c("Table 3",
    "Effects of the Hillman Summer Program on College Outcomes — All States")
)
table_3

table_4 <- make_att_table(
  results_pa, matched_pa,
  c("Table 4",
    "Effects of the Hillman Summer Program on College Outcomes — PA Public Schools"),
  pa_note = "PA model additionally controls for school enrollment, % economically disadvantaged, % English learner, % special education, and % white."
)
table_4

# =============================================================================
# 6. APPENDIX TABLE A2 — SAMPLE SIZES BY COHORT
# =============================================================================

message("\n=== Appendix A2: Sample Sizes by Cohort ===")

make_cohort_table <- function(pre_all, pre_pa, matched_all, matched_pa) {
  sc <- function(pre, matched, label) {
    pre |>
      group_by(year) |>
      summarise(
        pre_trt  = sum(treated_in_year == 1),
        pre_ctrl = sum(treated_in_year == 0),
        .groups = "drop"
      ) |>
      left_join(
        matched |>
          group_by(year) |>
          summarise(
            post_trt  = sum(treated_in_year == 1),
            post_ctrl = sum(treated_in_year == 0),
            .groups = "drop"
          ),
        by = "year"
      ) |>
      mutate(sample = label, year = as.character(year))
  }

  tbl_all <- sc(pre_all, matched_all, "All States")
  tbl_pa  <- sc(pre_pa,  matched_pa,  "PA Public Schools")

  totals <- bind_rows(tbl_all, tbl_pa) |>
    group_by(sample) |>
    summarise(
      year = "Total",
      across(c(pre_trt, pre_ctrl, post_trt, post_ctrl), sum),
      .groups = "drop"
    )

  tbl <- bind_rows(tbl_all, tbl_pa, totals) |>
    mutate(year = factor(year, levels = c("2017", "2018", "2019", "2021", "2023", "Total")))

  n_rows <- nrow(tbl)

  tbl |>
    gt(rowname_col = "year", groupname_col = "sample") |>
    tab_header(
      title    = md("**Appendix Table A2**"),
      subtitle = "Sample Sizes Before and After Propensity Score Matching, by Cohort"
    ) |>
    tab_spanner(label = "Before Matching", columns = c(pre_trt,  pre_ctrl)) |>
    tab_spanner(label = "After Matching",  columns = c(post_trt, post_ctrl)) |>
    cols_label(
      pre_trt   = "Treated",
      pre_ctrl  = "Control",
      post_trt  = "Treated",
      post_ctrl = "Control"
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(rows = year == "Total")
    ) |>
    tab_style(
      style     = cell_borders(sides = "top", color = "black", weight = px(1.5)),
      locations = cells_body(rows = year == "Total")
    ) |>
    tab_style(
      style     = cell_text(style = "italic"),
      locations = cells_row_groups()
    ) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left",   columns = year) |>
    tab_source_note(source_note = md(paste0(
      "*Note.* Before-matching counts exclude year 2022 (insufficient NSC follow-up), ",
      "confirmed non-citizens, and students treated before 2017. Year 2023 excluded from ",
      "PA matching (only 1 treated student). After-matching counts are unique matched units; ",
      "with-replacement matching yields an effective sample size of controls lower than shown."
    ))) |>
    add_body_rules(n_rows) |>
    eepa_options()
}

appendix_a2 <- make_cohort_table(pre_all, pre_pa, matched_all, matched_pa)
appendix_a2

# =============================================================================
# 7. FIGURE 1 — LOVE PLOT (All States)
# =============================================================================
# SMD before and after matching for each covariate. EEPA-friendly:
# black-and-white friendly, minimal grid, reference line at 0 and ±0.10.

message("\n=== Figure 1: Love Plot ===")

make_love_plot <- function(m_out, display_vars, var_labels, title) {
  bal_wide <- bal.tab(m_out, un = TRUE)$Balance |>
    tibble::rownames_to_column("variable") |>
    filter(variable %in% display_vars) |>
    select(variable, smd_before = Diff.Un, smd_after = Diff.Adj) |>
    mutate(label = coalesce(var_labels[variable], variable)) |>
    # Sort by pre-match |SMD| descending (standard Love plot convention)
    arrange(abs(smd_before)) |>
    mutate(label = factor(label, levels = unique(label)))

  x_lim <- max(abs(c(bal_wide$smd_before, bal_wide$smd_after)), na.rm = TRUE) * 1.15
  x_lim <- max(x_lim, 0.15)   # always show the ±0.10 threshold lines

  bal <- bal_wide |>
    pivot_longer(c(smd_before, smd_after), names_to = "timing", values_to = "smd") |>
    mutate(
      timing = factor(timing,
                      levels = c("smd_before", "smd_after"),
                      labels = c("Before matching", "After matching"))
    )

  ggplot(bal, aes(x = smd, y = label, shape = timing, color = timing)) +
    geom_vline(xintercept = 0,     linetype = "solid",  color = "gray80", linewidth = 0.4) +
    geom_vline(xintercept =  0.10, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_vline(xintercept = -0.10, linetype = "dashed", color = "gray60", linewidth = 0.4) +
    geom_point(size = 2.5, stroke = 0.8) +
    scale_color_manual(
      values = c("Before matching" = "gray50", "After matching" = "black"),
      name   = NULL
    ) +
    scale_shape_manual(
      values = c("Before matching" = 1, "After matching" = 16),
      name   = NULL
    ) +
    scale_x_continuous(
      limits = c(-x_lim, x_lim),
      breaks = pretty_breaks(n = 6),
      labels = function(x) sprintf("%.2f", x)
    ) +
    labs(
      title   = title,
      x       = "Standardized Mean Difference",
      y       = NULL,
      caption = paste0(
        "Note. Dashed lines at ±0.10 mark the conventional balance threshold. ",
        "Open circles = before matching; filled circles = after matching."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 11, margin = margin(b = 6)),
      plot.caption    = element_text(color = "gray40", size = 8, hjust = 0,
                                     margin = margin(t = 8), lineheight = 1.3),
      axis.text.y     = element_text(size = 9),
      axis.text.x     = element_text(size = 9),
      axis.title.x    = element_text(size = 9),
      legend.position = "bottom",
      legend.text     = element_text(size = 9),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray92"),
      plot.margin = margin(10, 15, 10, 10)
    )
}

figure_1 <- make_love_plot(
  m_out_all, display_vars_all, bal_labels,
  "Figure 1. Covariate Balance Before and After Propensity Score Matching — All States"
)
figure_1

# =============================================================================
# 8. FIGURE 2 — SUBGROUP COEFFICIENT PLOTS
# =============================================================================
# Two panels: seamless enrollment (ITT) and seamless STEM enrollment (ITT).
# Subgroup estimates from results_sg_all; suppressed subgroups excluded.
# ATT plotted in percentage points for readability.

message("\n=== Figure 2: Subgroup Effects ===")

sg_order <- c(
  "Stipend Eligible",
  "Racially Marginalized",
  "Urban",
  "Suburban"
)

make_subgroup_plot <- function(results_sg, outcomes, outcome_labels_map,
                               sample_filter, title, caption) {
  df <- results_sg |>
    filter(outcome %in% outcomes, !suppressed, sample == sample_filter) |>
    mutate(
      att_pp    = att * 100,
      lo_pp     = conf_lo * 100,
      hi_pp     = conf_hi * 100,
      sig       = pval < 0.05,
      out_label = outcome_labels_map[outcome],
      out_label = factor(out_label, levels = outcome_labels_map),
      subgroup  = factor(subgroup, levels = rev(sg_order))
    ) |>
    filter(!is.na(subgroup))

  ggplot(df, aes(x = att_pp, y = subgroup)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray50", linewidth = 0.5) +
    geom_errorbarh(
      aes(xmin = lo_pp, xmax = hi_pp),
      height = 0.2, color = "gray30", linewidth = 0.7
    ) +
    geom_point(aes(shape = sig, fill = sig), size = 3, color = "black") +
    scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 21), guide = "none") +
    scale_fill_manual(
      values = c(`TRUE` = "black", `FALSE` = "white"), guide = "none"
    ) +
    facet_wrap(~ out_label, ncol = 2) +
    scale_x_continuous(
      labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x, 1), " pp"),
      breaks = scales::pretty_breaks(n = 5)
    ) +
    labs(
      title    = title,
      x        = "Treatment effect (percentage points)",
      y        = NULL,
      caption  = caption
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 11, margin = margin(b = 4)),
      plot.caption     = element_text(color = "gray40", size = 8, hjust = 0,
                                      margin = margin(t = 8), lineheight = 1.3),
      axis.text.y      = element_text(size = 10),
      axis.text.x      = element_text(size = 9),
      axis.title.x     = element_text(size = 9),
      strip.text       = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray92"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

sg_outcomes <- c("enroll_seamless", "enroll_seamless_stem")
sg_out_labels <- c(
  enroll_seamless      = "Seamless Enrollment",
  enroll_seamless_stem = "Seamless STEM Enrollment"
)

figure_2 <- make_subgroup_plot(
  results_sg_all,
  sg_outcomes,
  sg_out_labels,
  sample_filter = "nsc_matched",
  title   = "Figure 2. Subgroup Treatment Effects on Enrollment — All States",
  caption = paste0(
    "Note. Each row reports the ATT from a separate LPM limited to that subgroup, ",
    "including all baseline covariates and cohort fixed effects. ",
    "Filled circles = p < .05; open circles = p ≥ .05. ",
    "Bars are 95% confidence intervals (HC2 SEs). ",
    "Subgroups with fewer than 20 treated students are suppressed."
  )
)
figure_2

# =============================================================================
# 9. SAVE ALL TABLES AND FIGURES
# =============================================================================

message("\n=== Saving tables ===")

tbl_list <- list(
  table_1_descriptive         = table_1,
  table_2_balance_all_states  = table_2,
  table_3_att_all_states      = table_3,
  table_4_att_pa              = table_4,
  appendix_a1_balance_pa      = appendix_a1,
  appendix_a2_sample_sizes    = appendix_a2
)

for (nm in names(tbl_list)) {
  gtsave(tbl_list[[nm]], here("output", "tables", paste0(nm, ".html")))
  message("  Saved: ", nm, ".html")
}

message("\n=== Saving figures ===")

ggsave(
  here("output", "figures", "figure_1_love_plot.png"),
  figure_1, width = 7, height = 6, dpi = 300
)
message("  Saved: figure_1_love_plot.png")

ggsave(
  here("output", "figures", "figure_2_subgroup_effects.png"),
  figure_2, width = 9, height = 5, dpi = 300
)
message("  Saved: figure_2_subgroup_effects.png")

message("\n=== All tables and figures complete ===")
