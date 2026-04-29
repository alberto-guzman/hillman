# =============================================================================
# 8_1d_tables.R — Publication-ready tables (gt), EEPA-style
#
# Three tables from the matched samples + ATT results, formatted to match
# Educational Evaluation and Policy Analysis (EEPA) typesetting conventions:
# Times New Roman serif, no color bands, top/bottom rules only, italicized
# row-group labels, tabular figures, APA significance stars, "Note." prefix.
#
#   Table 1: Sample descriptive statistics, matched samples
#   Table 2: Pre- and post-matching covariate balance (SMD + variance ratio)
#   Table 3: Estimated treatment effects (ATT, SE, 95% CI)
#
# Inputs (from scripts 5 and 7):
#   data/matched/matched_{pa,all_states}_year_only.rds
#   data/matched/matchit_object_{pa,all_states}.rds
#   output/att_results_{pa,all_states}.rds
#
# Outputs:
#   output/tables/table{1,2,3}_*.{html,png,rds}
# =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(gt)
library(cobalt)

# PNG export uses chromote (via webshot2). On systems without Chrome installed,
# point CHROMOTE_CHROME at a Chromium-based browser if not already set.
if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
  candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    "/usr/bin/google-chrome",
    "/usr/bin/chromium-browser"
  )
  hit <- candidates[file.exists(candidates)][1]
  if (!is.na(hit)) Sys.setenv(CHROMOTE_CHROME = hit)
}

# -----------------------------------------------------------------------------
# Load
# -----------------------------------------------------------------------------

matched_pa  <- readRDS(here("data", "matched", "matched_pa_year_only.rds"))
matched_all <- readRDS(here("data", "matched", "matched_all_states_year_only.rds"))

m_pa  <- readRDS(here("data", "matched", "matchit_object_pa.rds"))
m_all <- readRDS(here("data", "matched", "matchit_object_all_states.rds"))

att_pa  <- readRDS(here("output", "att_results_pa.rds"))
att_all <- readRDS(here("output", "att_results_all_states.rds"))

out_dir <- here("output", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# EEPA gt theme
# -----------------------------------------------------------------------------
# APA/EEPA conventions: serif font, white background, top + bottom + header
# rules only (no internal horizontal rules, no vertical rules), italicized
# row-group labels (no colored bands), tabular figures.

eepa_theme <- function(gt_obj) {
  gt_obj |>
    # APA/EEPA serif typeface, publication body size (~10pt). This is a
    # typography choice, not a width override — gt still sizes the table
    # to its content (width: auto).
    opt_table_font(font = c("Times New Roman", "Times", "serif")) |>
    tab_options(
      table.font.size                = px(13),
      # White backgrounds — no zebra striping, no colored row-group bars
      table.background.color         = "white",
      heading.background.color       = "white",
      column_labels.background.color = "white",
      row_group.background.color     = "white",
      stub.background.color          = "white",
      # APA-style: top + bottom rules around the table, rule under header
      table.border.top.width            = px(2),
      table.border.top.color            = "black",
      table.border.bottom.width         = px(2),
      table.border.bottom.color         = "black",
      column_labels.border.top.width    = px(2),
      column_labels.border.top.color    = "black",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      # No internal hlines in body; rule before source notes
      table_body.hlines.color           = "white",
      table_body.border.top.color       = "white",
      table_body.border.bottom.width    = px(1),
      table_body.border.bottom.color    = "black",
      # No row-group rules (italic label is the section break)
      row_group.border.top.color        = "white",
      row_group.border.bottom.color     = "white",
      # No stub border, no header bottom border (keeps title/subtitle clean)
      stub.border.color                 = "white",
      heading.border.bottom.color       = "white",
      # Bold title and column labels per APA
      heading.title.font.weight         = "bold",
      column_labels.font.weight         = "bold"
    ) |>
    # Italic row-group section headings
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_row_groups()
    )
}

# -----------------------------------------------------------------------------
# Variable labels
# -----------------------------------------------------------------------------
# Order matters: dictates row order in Tables 1 and 2.

individual_vars <- tibble::tribble(
  ~var,                     ~label,                            ~group,
  "gpa",                    "GPA (4.0 scale)",                 "Academic preparation",
  "psat_math",              "PSAT Math",                       "Academic preparation",
  "stipend",                "Received program stipend (%)",    "Program participation",
  "neg_school",             "Negative school experience (%)",  "Program participation",
  "gender",                 "Male (%)",                        "Demographics",
  "house_size",             "Household size",                  "Demographics",
  "racially_marginalized",  "Racially marginalized (%)",       "Demographics",
  "bi_multi_racial",        "Bi/multi-racial (%)",             "Demographics",
  "disability",             "Disability disclosed (%)",        "Demographics",
  "urban",                  "Urban locale (%)",                "School locale",
  "suburban",               "Suburban locale (%)",             "School locale",
  "rural",                  "Rural locale (%)",                "School locale",
  "grade_9",                "9th grade (%)",                   "Grade in program",
  "grade_10",               "10th grade (%)",                  "Grade in program",
  "grade_12",               "12th grade (%)",                  "Grade in program"
)

school_vars <- tibble::tribble(
  ~var,                              ~label,                            ~group,
  "school_enrollment",               "Enrollment (count)",              "School characteristics (PA only)",
  "school_pct_econ_disadvantaged",   "Economically disadvantaged (%)",  "School characteristics (PA only)",
  "school_pct_english_learner",      "English learners (%)",            "School characteristics (PA only)",
  "school_pct_special_ed",           "Special education (%)",           "School characteristics (PA only)",
  "school_pct_white",                "White students (%)",              "School characteristics (PA only)"
)

is_binary <- function(x) {
  xc <- x[!is.na(x)]
  length(xc) > 0 && all(xc %in% c(0, 1))
}

continuous_vars <- bind_rows(individual_vars, school_vars) |>
  filter(var %in% c("gpa", "psat_math", "house_size",
                    "school_enrollment", "school_pct_econ_disadvantaged",
                    "school_pct_english_learner", "school_pct_special_ed",
                    "school_pct_white")) |>
  pull(var)

# -----------------------------------------------------------------------------
# Table 1 — Sample descriptive statistics, matched samples
# -----------------------------------------------------------------------------

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) < 2) return(NA_real_)
  m <- weighted_mean(x, w)
  sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
}

format_cell <- function(x, w, binary, digits = 2) {
  m <- weighted_mean(x, w)
  if (is.na(m)) return("--")
  if (binary) {
    sprintf("%.1f", 100 * m)
  } else {
    sprintf("%.2f (%.2f)", round(m, digits), round(weighted_sd(x, w), digits))
  }
}

# Continuous covariates carry an upstream "fill NA with 0 + add _miss indicator"
# imputation so the PS logit can use them. Reporting raw means/SDs would treat
# imputed zeros as real values (e.g., a missing PSAT becomes a 0 score).
# Mask imputed-zero rows to NA on a per-row basis before summarizing.
mask_imputed <- function(data, var) {
  miss_col <- paste0(var, "_miss")
  x <- data[[var]]
  if (miss_col %in% names(data)) {
    x[data[[miss_col]] == 1] <- NA_real_
  }
  x
}

build_descriptives <- function(data, var_spec) {
  trt_idx <- data$treated_in_year == 1
  ctl_idx <- data$treated_in_year == 0
  var_spec |>
    mutate(
      treated = map_chr(var, ~ {
        x <- mask_imputed(data, .x)
        format_cell(x[trt_idx], data$weights[trt_idx], !(.x %in% continuous_vars))
      }),
      control = map_chr(var, ~ {
        x <- mask_imputed(data, .x)
        format_cell(x[ctl_idx], data$weights[ctl_idx], !(.x %in% continuous_vars))
      })
    )
}

desc_pa  <- build_descriptives(matched_pa,  bind_rows(individual_vars, school_vars))
desc_all <- build_descriptives(matched_all, individual_vars)

desc_combined <- desc_pa |>
  select(var, label, group, pa_treated = treated, pa_control = control) |>
  left_join(
    desc_all |> select(var, all_treated = treated, all_control = control),
    by = "var"
  ) |>
  mutate(
    all_treated = if_else(is.na(all_treated), "--", all_treated),
    all_control = if_else(is.na(all_control), "--", all_control)
  )

n_pa_t  <- sum(matched_pa$treated_in_year  == 1)
n_pa_c  <- sum(matched_pa$treated_in_year  == 0)
n_all_t <- sum(matched_all$treated_in_year == 1)
n_all_c <- sum(matched_all$treated_in_year == 0)

table1 <- desc_combined |>
  gt(groupname_col = "group", rowname_col = "label") |>
  cols_hide(columns = "var") |>
  cols_label(
    pa_treated  = md(sprintf("Treated<br>(*n* = %d)", n_pa_t)),
    pa_control  = md(sprintf("Comparison<br>(*n* = %d)", n_pa_c)),
    all_treated = md(sprintf("Treated<br>(*n* = %d)", n_all_t)),
    all_control = md(sprintf("Comparison<br>(*n* = %d)", n_all_c))
  ) |>
  tab_spanner(label = "PA public schools",   columns = c(pa_treated,  pa_control)) |>
  tab_spanner(label = "All states",          columns = c(all_treated, all_control)) |>
  tab_header(
    title    = "Table 1",
    subtitle = md("*Sample Descriptive Statistics for the Matched Treated and Comparison Groups*")
  ) |>
  cols_align(align = "right", columns = c(pa_treated, pa_control, all_treated, all_control)) |>
  cols_align(align = "left",  columns = "label") |>
  eepa_theme()

# -----------------------------------------------------------------------------
# Table 2 — Covariate balance, pre vs post
# -----------------------------------------------------------------------------
# Includes signed standardized mean differences (un-adjusted and matched) plus
# variance ratios (matched only) for continuous covariates. Variance ratios
# near 1 indicate balanced second moments (Austin, 2009).

bal_to_df <- function(matchit_obj, sample_label) {
  # Defaults match those used in 5_1d_matching.R (cobalt defaults for matchit:
  # binary = "raw", s.d.denom = "treated"), so balance values agree across
  # script outputs.
  bt <- bal.tab(
    matchit_obj,
    un    = TRUE,
    abs   = FALSE,
    stats = c("mean.diffs", "variance.ratios")
  )$Balance
  tibble::tibble(
    var       = rownames(bt),
    smd_un    = bt[["Diff.Un"]],
    smd_adj   = bt[["Diff.Adj"]],
    vr_adj    = bt[["V.Ratio.Adj"]],
    sample    = sample_label
  )
}

bal_pa  <- bal_to_df(m_pa,  "pa")
bal_all <- bal_to_df(m_all, "all")

label_lookup <- bind_rows(individual_vars, school_vars) |>
  select(var, label, group)

attach_labels <- function(df) {
  df |>
    left_join(label_lookup, by = "var") |>
    filter(!is.na(label))
}

bal_pa  <- attach_labels(bal_pa)
bal_all <- attach_labels(bal_all)

bal_combined <- bal_pa |>
  select(var, label, group,
         pa_un = smd_un, pa_adj = smd_adj, pa_vr = vr_adj) |>
  left_join(
    bal_all |> select(var, all_un = smd_un, all_adj = smd_adj, all_vr = vr_adj),
    by = "var"
  )

# Order: continuous vars first (so variance-ratio rows cluster up top by group),
# then binary; preserve within-group order from var spec.
bal_combined <- bind_rows(individual_vars, school_vars) |>
  select(var, label, group) |>
  left_join(bal_combined |> select(-label, -group), by = "var")

format_smd <- function(x) {
  ifelse(is.na(x), "--", formatC(x, digits = 3, format = "f", flag = ""))
}
format_vr <- function(x, var) {
  ifelse(is.na(x) | !(var %in% continuous_vars), "--",
         formatC(x, digits = 2, format = "f"))
}

table2 <- bal_combined |>
  mutate(
    pa_un  = format_smd(pa_un),
    pa_adj = format_smd(pa_adj),
    pa_vr  = format_vr(pa_vr, var),
    all_un = format_smd(all_un),
    all_adj = format_smd(all_adj),
    all_vr  = format_vr(all_vr, var)
  ) |>
  gt(groupname_col = "group", rowname_col = "label") |>
  cols_hide(columns = "var") |>
  cols_label(
    pa_un   = md("Unadjusted<br>SMD"),
    pa_adj  = md("Matched<br>SMD"),
    pa_vr   = md("Variance<br>ratio"),
    all_un  = md("Unadjusted<br>SMD"),
    all_adj = md("Matched<br>SMD"),
    all_vr  = md("Variance<br>ratio")
  ) |>
  tab_spanner(label = "PA public schools", columns = c(pa_un,  pa_adj,  pa_vr)) |>
  tab_spanner(label = "All states",        columns = c(all_un, all_adj, all_vr)) |>
  tab_header(
    title    = "Table 2",
    subtitle = md("*Standardized Mean Differences and Variance Ratios Before and After Matching*")
  ) |>
  cols_align(align = "right", columns = c(pa_un, pa_adj, pa_vr, all_un, all_adj, all_vr)) |>
  cols_align(align = "left",  columns = "label") |>
  eepa_theme()

# -----------------------------------------------------------------------------
# Table 3 — Estimated treatment effects
# -----------------------------------------------------------------------------

panel_label <- c(
  A = "Panel A. Enrollment",
  B = "Panel B. Initial institution type",
  C = "Panel C. Persistence (conditional on enrollment)"
)

format_pp <- function(x, digits = 1) {
  ifelse(is.na(x), "--", formatC(100 * x, digits = digits, format = "f"))
}

format_att <- function(att, se) {
  out <- ifelse(
    is.na(att),
    "--",
    ifelse(
      is.na(se),
      formatC(100 * att, digits = 1, format = "f"),
      sprintf("%s (%s)",
              formatC(100 * att, digits = 1, format = "f"),
              formatC(100 * se,  digits = 1, format = "f"))
    )
  )
  out
}

format_ci <- function(lo, hi) {
  ifelse(
    is.na(lo) | is.na(hi),
    "--",
    sprintf("[%.1f, %.1f]", 100 * lo, 100 * hi)
  )
}

stars <- function(p) {
  case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ "†",   # dagger for p < .10
    TRUE      ~ ""
  )
}

outcome_order <- c(
  "enroll_seamless", "enroll_seamless_stem",
  "inst_4yr_entry",  "inst_2yr_entry",
  "pers_1y",         "pers_1y_stem"
)

build_impact_block <- function(att_df) {
  att_df |>
    transmute(
      panel,
      outcome,
      label,
      n        = n_obs,
      ctrl_pct = format_pp(ctrl_mean),
      att_se   = paste0(format_att(att, se), stars(pval)),
      ci       = format_ci(conf_lo, conf_hi)
    )
}

impact_pa  <- build_impact_block(att_pa)  |> rename_with(~ paste0("pa_",  .), -c(panel, outcome, label))
impact_all <- build_impact_block(att_all) |> rename_with(~ paste0("all_", .), -c(panel, outcome, label))

impact_wide <- left_join(impact_pa, impact_all, by = c("panel", "outcome", "label")) |>
  mutate(
    panel_full = panel_label[panel],
    outcome    = factor(outcome, levels = outcome_order)
  ) |>
  arrange(panel, outcome) |>
  select(-outcome)

table3 <- impact_wide |>
  gt(groupname_col = "panel_full", rowname_col = "label") |>
  cols_hide(columns = "panel") |>
  cols_label(
    pa_n         = md("*n*"),
    pa_ctrl_pct  = md("Comparison<br>(%)"),
    pa_att_se    = md("ATT<br>(*SE*)"),
    pa_ci        = "95% CI",
    all_n        = md("*n*"),
    all_ctrl_pct = md("Comparison<br>(%)"),
    all_att_se   = md("ATT<br>(*SE*)"),
    all_ci       = "95% CI"
  ) |>
  tab_spanner(label = "PA public schools",
              columns = c(pa_n,  pa_ctrl_pct,  pa_att_se,  pa_ci)) |>
  tab_spanner(label = "All states",
              columns = c(all_n, all_ctrl_pct, all_att_se, all_ci)) |>
  tab_header(
    title    = "Table 3",
    subtitle = md("*Estimated Treatment Effects on College Enrollment, Institution Type, and Persistence*")
  ) |>
  cols_align(align = "right",
             columns = c(pa_n, pa_ctrl_pct, pa_att_se, pa_ci,
                         all_n, all_ctrl_pct, all_att_se, all_ci)) |>
  cols_align(align = "left", columns = "label") |>
  eepa_theme()

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

tables <- list(
  table1_descriptives = table1,
  table2_balance      = table2,
  table3_impact       = table3
)

for (nm in names(tables)) {
  saveRDS(tables[[nm]], file.path(out_dir, paste0(nm, ".rds")))
  gtsave(tables[[nm]], file.path(out_dir, paste0(nm, ".png")))
}

message("\n=== Tables saved ===")
for (nm in names(tables)) {
  message("  ", file.path(out_dir, paste0(nm, ".png")))
}
