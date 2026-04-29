# =============================================================================
# 8_1d_tables.R — Publication-ready tables (gt)
#
# Produces three tables from the matched samples + ATT results:
#   Table 1: Student descriptives (matched sample, T vs C, both samples)
#   Table 2: Covariate balance (un-adjusted vs adjusted SMDs)
#   Table 3: Impact estimates (6 outcomes × 2 samples)
#
# Inputs (from scripts 5 and 7):
#   data/matched/matched_pa_year_only.rds
#   data/matched/matched_all_states_year_only.rds
#   data/matched/matchit_object_pa.rds
#   data/matched/matchit_object_all_states.rds
#   output/att_results_pa.rds
#   output/att_results_all_states.rds
#
# Outputs:
#   output/tables/table1_descriptives.html
#   output/tables/table2_balance.html
#   output/tables/table3_impact.html
#   output/tables/*.rds  (gt objects, for downstream Quarto/Word embedding)
# =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(gt)
library(cobalt)

# PNG export uses chromote (via webshot2). On systems without Chrome installed,
# point CHROMOTE_CHROME at a Chromium-based browser (Brave, Edge, Chromium, etc.)
# if not already set. Skips silently if nothing is found — HTML/RDS still save.
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
# Variable labels
# -----------------------------------------------------------------------------
# Order matters: dictates row order in Table 1 and Table 2.

individual_vars <- tibble::tribble(
  ~var,                     ~label,                            ~group,
  "gpa",                    "GPA (4.0 scale)",                 "Academic",
  "psat_math",              "PSAT Math",                       "Academic",
  "stipend",                "Received program stipend",        "Program",
  "house_size",             "Household size",                  "Demographic",
  "racially_marginalized",  "Racially marginalized",           "Demographic",
  "bi_multi_racial",        "Bi/multi-racial",                 "Demographic",
  "urban",                  "Urban locale",                    "Geography",
  "suburban",               "Suburban locale",                 "Geography",
  "rural",                  "Rural locale",                    "Geography",
  "disability",             "Disability disclosed",            "Demographic",
  "neg_school",             "Negative school experience",      "Program",
  "grade_9",                "Grade 9",                         "Grade",
  "grade_10",               "Grade 10",                        "Grade",
  "grade_12",               "Grade 12",                        "Grade"
)

school_vars <- tibble::tribble(
  ~var,                              ~label,                            ~group,
  "school_enrollment",               "School enrollment (count)",       "School",
  "school_pct_econ_disadvantaged",   "% economically disadvantaged",    "School",
  "school_pct_english_learner",      "% English learner",               "School",
  "school_pct_special_ed",           "% special education",             "School",
  "school_pct_white",                "% white students"                ,"School"
)

# -----------------------------------------------------------------------------
# Table 1 — Student descriptives (matched sample, T vs C)
# -----------------------------------------------------------------------------
# Matched-sample means by treatment, weighted by MatchIt weights.
# For binary covars, mean = proportion (no SD column shown).

is_binary <- function(x) {
  x_clean <- x[!is.na(x)]
  length(x_clean) > 0 && all(x_clean %in% c(0, 1))
}

weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

weighted_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) < 2) return(NA_real_)
  m  <- weighted_mean(x, w)
  ws <- w[ok]
  xs <- x[ok]
  v  <- sum(ws * (xs - m)^2) / sum(ws)
  sqrt(v)
}

format_cell <- function(x, w, binary, digits = 2) {
  m <- weighted_mean(x, w)
  if (is.na(m)) return("--")
  if (binary) {
    sprintf("%.1f%%", 100 * m)
  } else {
    s <- weighted_sd(x, w)
    sprintf("%.2f (%.2f)", round(m, digits), round(s, digits))
  }
}

build_descriptives <- function(data, var_spec) {
  trt <- data %>% filter(treated_in_year == 1)
  ctl <- data %>% filter(treated_in_year == 0)

  var_spec %>%
    mutate(
      treated = map_chr(var, ~ {
        x <- data[[.x]]
        format_cell(trt[[.x]], trt$weights, is_binary(x))
      }),
      control = map_chr(var, ~ {
        x <- data[[.x]]
        format_cell(ctl[[.x]], ctl$weights, is_binary(x))
      })
    )
}

desc_pa  <- build_descriptives(matched_pa,  bind_rows(individual_vars, school_vars))
desc_all <- build_descriptives(matched_all, individual_vars)

# Combine into one frame: var/label/group + PA T/C + All-states T/C
desc_combined <- desc_pa %>%
  select(var, label, group, pa_treated = treated, pa_control = control) %>%
  left_join(
    desc_all %>% select(var, all_treated = treated, all_control = control),
    by = "var"
  ) %>%
  mutate(
    all_treated = if_else(is.na(all_treated), "--", all_treated),
    all_control = if_else(is.na(all_control), "--", all_control)
  )

n_pa_t  <- sum(matched_pa$treated_in_year  == 1)
n_pa_c  <- sum(matched_pa$treated_in_year  == 0)
n_all_t <- sum(matched_all$treated_in_year == 1)
n_all_c <- sum(matched_all$treated_in_year == 0)

table1 <- desc_combined %>%
  gt(groupname_col = "group", rowname_col = "label") %>%
  cols_hide(columns = "var") %>%
  cols_label(
    pa_treated  = md(sprintf("Treated<br>(n = %d)", n_pa_t)),
    pa_control  = md(sprintf("Control<br>(n = %d)", n_pa_c)),
    all_treated = md(sprintf("Treated<br>(n = %d)", n_all_t)),
    all_control = md(sprintf("Control<br>(n = %d)", n_all_c))
  ) %>%
  tab_spanner(label = "PA Public (Primary)",   columns = c(pa_treated,  pa_control)) %>%
  tab_spanner(label = "All States (Robustness)", columns = c(all_treated, all_control)) %>%
  tab_header(
    title    = "Table 1. Student Descriptives in Matched Samples",
    subtitle = md("*Mean (SD) for continuous variables; % for binary variables. Weighted by MatchIt weights.*")
  ) %>%
  tab_source_note(md(paste0(
    "*Note.* PA Public is the primary analytic sample. All States is reported as a robustness check. ",
    "School-level covariates are PA-only. ",
    "Grade 11 is the modal/reference grade. ",
    "Matched samples reflect 1:3 nearest-neighbor PSM with replacement, caliper 0.25 SD."
  ))) %>%
  cols_align(align = "right", columns = c(pa_treated, pa_control, all_treated, all_control))

# -----------------------------------------------------------------------------
# Table 2 — Balance (un-adjusted vs adjusted SMDs)
# -----------------------------------------------------------------------------

bal_to_df <- function(matchit_obj, sample_label) {
  bt <- bal.tab(matchit_obj, un = TRUE, abs = FALSE)$Balance
  tibble::tibble(
    var       = rownames(bt),
    smd_un    = bt[["Diff.Un"]],
    smd_adj   = bt[["Diff.Adj"]],
    sample    = sample_label
  )
}

bal_pa  <- bal_to_df(m_pa,  "PA Public (Primary)")
bal_all <- bal_to_df(m_all, "All States (Robustness)")

# Map cobalt's variable names (which expand factors) back to our labels.
# bal.tab keeps continuous covars by raw name and binary covars by raw name.
label_lookup <- bind_rows(individual_vars, school_vars) %>%
  select(var, label, group)

attach_labels <- function(df) {
  df %>%
    left_join(label_lookup, by = "var") %>%
    # Drop rows we don't want to display (e.g., distance, _miss indicators,
    # grade_11 which is the reference, pa_state in all-states match, year FE)
    filter(!is.na(label))
}

bal_combined <- bind_rows(
  attach_labels(bal_pa)  %>% mutate(sample = "PA Public (Primary)"),
  attach_labels(bal_all) %>% mutate(sample = "All States (Robustness)")
) %>%
  pivot_wider(
    id_cols     = c(var, label, group),
    names_from  = sample,
    values_from = c(smd_un, smd_adj),
    names_glue  = "{sample}__{.value}"
  ) %>%
  rename(
    pa_un   = `PA Public (Primary)__smd_un`,
    pa_adj  = `PA Public (Primary)__smd_adj`,
    all_un  = `All States (Robustness)__smd_un`,
    all_adj = `All States (Robustness)__smd_adj`
  )

flag_imbalance <- function(x, threshold = 0.10) {
  case_when(
    is.na(x)             ~ "--",
    abs(x) > threshold   ~ paste0(formatC(x, digits = 3, format = "f"), "*"),
    TRUE                 ~ formatC(x, digits = 3, format = "f")
  )
}

table2 <- bal_combined %>%
  arrange(group, label) %>%
  mutate(across(c(pa_un, pa_adj, all_un, all_adj), flag_imbalance)) %>%
  gt(groupname_col = "group", rowname_col = "label") %>%
  cols_hide(columns = "var") %>%
  cols_label(
    pa_un   = md("Unadjusted"),
    pa_adj  = md("Matched"),
    all_un  = md("Unadjusted"),
    all_adj = md("Matched")
  ) %>%
  tab_spanner(label = "PA Public (Primary)",     columns = c(pa_un,  pa_adj)) %>%
  tab_spanner(label = "All States (Robustness)", columns = c(all_un, all_adj)) %>%
  tab_header(
    title    = "Table 2. Covariate Balance Before and After Matching",
    subtitle = md("*Standardized mean differences (treated − control). |SMD| > 0.10 flagged with \\*.*")
  ) %>%
  tab_source_note(md(paste0(
    "*Note.* Standardized mean differences computed by `cobalt::bal.tab()`. ",
    "Conventional thresholds: |SMD| ≤ 0.10 is the standard balance criterion (Stuart 2010); ",
    "|SMD| ≤ 0.20 is a relaxed threshold (Cohen). ",
    "1:3 nearest-neighbor with replacement, caliper 0.25 SD, exact-match on year (PA) or year × pa_state (All States)."
  ))) %>%
  cols_align(align = "right", columns = c(pa_un, pa_adj, all_un, all_adj))

# -----------------------------------------------------------------------------
# Table 3 — Impact estimates
# -----------------------------------------------------------------------------

panel_label <- c(
  A = "Panel A. Enrollment",
  B = "Panel B. Institution Type",
  C = "Panel C. Persistence (conditional on enrollment)"
)

format_pct <- function(x) {
  ifelse(is.na(x), "--", formatC(100 * x, digits = 1, format = "f"))
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

format_p <- function(p) {
  ifelse(
    is.na(p),
    "--",
    ifelse(p < 0.001, "<0.001", formatC(p, digits = 3, format = "f"))
  )
}

stars <- function(p) {
  case_when(
    is.na(p)   ~ "",
    p < 0.01   ~ "**",
    p < 0.05   ~ "*",
    p < 0.10   ~ "†",
    TRUE       ~ ""
  )
}

outcome_order <- c(
  "enroll_seamless", "enroll_seamless_stem",
  "inst_4yr_entry",  "inst_2yr_entry",
  "pers_1y",         "pers_1y_stem"
)

build_impact_block <- function(att_df, sample_label) {
  att_df %>%
    transmute(
      panel,
      outcome,
      label,
      n        = n_obs,
      ctrl     = format_pct(ctrl_mean),
      trt      = format_pct(trt_mean),
      att_se   = paste0(format_att(att, se), stars(pval)),
      pval_str = format_p(pval),
      sample   = sample_label
    )
}

impact_long <- bind_rows(
  build_impact_block(att_pa,  "PA Public (Primary)"),
  build_impact_block(att_all, "All States (Robustness)")
)

impact_wide <- impact_long %>%
  pivot_wider(
    id_cols     = c(panel, outcome, label),
    names_from  = sample,
    values_from = c(n, ctrl, trt, att_se, pval_str),
    names_glue  = "{sample}__{.value}"
  )

impact_wide <- impact_wide %>%
  select(
    panel, outcome, label,
    pa_n     = `PA Public (Primary)__n`,
    pa_ctrl  = `PA Public (Primary)__ctrl`,
    pa_trt   = `PA Public (Primary)__trt`,
    pa_att   = `PA Public (Primary)__att_se`,
    pa_p     = `PA Public (Primary)__pval_str`,
    all_n    = `All States (Robustness)__n`,
    all_ctrl = `All States (Robustness)__ctrl`,
    all_trt  = `All States (Robustness)__trt`,
    all_att  = `All States (Robustness)__att_se`,
    all_p    = `All States (Robustness)__pval_str`
  ) %>%
  mutate(
    panel_full = panel_label[panel],
    outcome    = factor(outcome, levels = outcome_order)
  ) %>%
  arrange(panel, outcome) %>%
  select(-outcome)

table3 <- impact_wide %>%
  gt(groupname_col = "panel_full", rowname_col = "label") %>%
  cols_hide(columns = "panel") %>%
  cols_label(
    pa_n     = "n",
    pa_ctrl  = "C %",
    pa_trt   = "T %",
    pa_att   = md("ATT (SE)"),
    pa_p     = "p",
    all_n    = "n",
    all_ctrl = "C %",
    all_trt  = "T %",
    all_att  = md("ATT (SE)"),
    all_p    = "p"
  ) %>%
  tab_spanner(label = "PA Public (Primary)",
              columns = c(pa_n,  pa_ctrl,  pa_trt,  pa_att,  pa_p)) %>%
  tab_spanner(label = "All States (Robustness)",
              columns = c(all_n, all_ctrl, all_trt, all_att, all_p)) %>%
  tab_header(
    title    = "Table 3. Estimated Treatment Effects on College Enrollment and Persistence",
    subtitle = md("*Doubly-robust ATT (percentage points) with HC3 standard errors.*")
  ) %>%
  tab_source_note(md(paste0(
    "*Note.* ATT estimated via g-computation on a weighted linear probability model with year fixed effects and HC3 standard errors. ",
    "All percentages and ATTs in percentage points. C % = matched control mean; T % = matched treated mean. ",
    "Panel C conditions on `enroll_ever == 1`. ",
    "Significance: † p < .10, \\* p < .05, \\*\\* p < .01."
  ))) %>%
  cols_align(align = "right",
             columns = c(pa_n, pa_ctrl, pa_trt, pa_att, pa_p,
                         all_n, all_ctrl, all_trt, all_att, all_p))

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

tables <- list(
  table1_descriptives = table1,
  table2_balance      = table2,
  table3_impact       = table3
)

png_ok <- TRUE
for (nm in names(tables)) {
  gt_obj <- tables[[nm]]
  saveRDS(gt_obj, file.path(out_dir, paste0(nm, ".rds")))
  gtsave(gt_obj,  file.path(out_dir, paste0(nm, ".html")))
  # PNG via webshot2/chromote — `expand` gives a small margin so wide tables don't clip.
  if (png_ok) {
    res <- tryCatch(
      gtsave(
        gt_obj |> tab_options(table.width = px(1400)),
        file.path(out_dir, paste0(nm, ".png")),
        expand = 40, zoom = 2, vwidth = 1600
      ),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      message("PNG export failed (", conditionMessage(res), ") — continuing with HTML only.")
      png_ok <- FALSE
    }
  }
}

message("\n=== Tables saved ===")
for (nm in names(tables)) {
  message("  ", file.path(out_dir, paste0(nm, ".html")))
  if (png_ok) message("  ", file.path(out_dir, paste0(nm, ".png")))
}
