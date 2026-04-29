# =============================================================================
# 8_1d_tables.R — Publication-ready LaTeX tables (kableExtra) + PNG render
#
# Three booktabs-style tables from the matched samples + ATT results, written
# as standalone .tex files that can be \input{} into a LaTeX document. Each
# .tex is also rendered to .png via pdflatex + pdftoppm so a quick visual
# check is available without compiling the full manuscript.
#
#   Table 1: Sample descriptive statistics, matched samples
#   Table 2: Pre- and post-matching covariate balance (SMD + variance ratio)
#   Table 3: Estimated treatment effects (ATT, SE, 95% CI)
#
# All proportions are reported in their natural [0, 1] scale (no percentages).
#
# LaTeX preamble requirements (typical kableExtra defaults):
#   \usepackage{booktabs}
#
# PNG rendering uses the system's pdflatex + pdftoppm (poppler-utils). If
# either is missing, the .tex files are still produced and PNG rendering
# is skipped with a warning.
#
# Inputs (from scripts 5 and 7):
#   data/matched/matched_{pa,all_states}_year_only.rds
#   data/matched/matchit_object_{pa,all_states}.rds
#   output/att_results_{pa,all_states}.rds
#
# Outputs:
#   output/tables/table{1,2,3}_*.tex
#   output/tables/table{1,2,3}_*.png
# =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(knitr)
library(kableExtra)
library(cobalt)

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

individual_vars <- tibble::tribble(
  ~var,                     ~label,                            ~group,
  "gpa",                    "GPA (4.0 scale)",                 "Academic preparation",
  "psat_math",              "PSAT Math",                       "Academic preparation",
  "stipend",                "Received program stipend",        "Program participation",
  "neg_school",             "Negative school experience",      "Program participation",
  "gender",                 "Male",                            "Demographics",
  "house_size",             "Household size",                  "Demographics",
  "racially_marginalized",  "Racially marginalized",           "Demographics",
  "bi_multi_racial",        "Bi/multi-racial",                 "Demographics",
  "disability",             "Disability disclosed",            "Demographics",
  "urban",                  "Urban locale",                    "School locale",
  "suburban",               "Suburban locale",                 "School locale",
  "rural",                  "Rural locale",                    "School locale",
  "grade_9",                "9th grade",                       "Grade in program",
  "grade_10",               "10th grade",                      "Grade in program",
  "grade_12",               "12th grade",                      "Grade in program"
)

school_vars <- tibble::tribble(
  ~var,                              ~label,                              ~group,
  "school_enrollment",               "Enrollment (count)",                "School characteristics (PA only)",
  "school_pct_econ_disadvantaged",   "Economically disadvantaged",        "School characteristics (PA only)",
  "school_pct_english_learner",      "English learners",                  "School characteristics (PA only)",
  "school_pct_special_ed",           "Special education",                 "School characteristics (PA only)",
  "school_pct_white",                "White students",                    "School characteristics (PA only)"
)

continuous_vars <- c(
  "gpa", "psat_math", "house_size",
  "school_enrollment", "school_pct_econ_disadvantaged",
  "school_pct_english_learner", "school_pct_special_ed",
  "school_pct_white"
)

# School-level "% of students who are X" covariates: source data is already
# 0–100, not 0–1. Rescale to proportions for consistent reporting.
school_pct_vars <- c(
  "school_pct_econ_disadvantaged",
  "school_pct_english_learner",
  "school_pct_special_ed",
  "school_pct_white"
)

# -----------------------------------------------------------------------------
# Helpers
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

# Continuous covariates carry an upstream zero-imputation with `_miss`
# indicators. Mask imputed-zero rows to NA before computing descriptive
# means/SDs so reported values reflect observed data, not the imputation.
mask_imputed <- function(data, var) {
  miss_col <- paste0(var, "_miss")
  x <- data[[var]]
  if (miss_col %in% names(data)) {
    x[data[[miss_col]] == 1] <- NA_real_
  }
  x
}

format_cell <- function(x, w, binary, digits = 2) {
  m <- weighted_mean(x, w)
  if (is.na(m)) return("--")
  if (binary) {
    sprintf("%.3f", m)  # proportion in [0, 1]
  } else {
    sprintf("%.2f (%.2f)", round(m, digits), round(weighted_sd(x, w), digits))
  }
}

group_index <- function(group_vec) {
  rle_g <- rle(group_vec)
  setNames(rle_g$lengths, rle_g$values)
}

write_tex <- function(kbl, file) {
  writeLines(as.character(kbl), file)
}

# -----------------------------------------------------------------------------
# Table 1 — Sample descriptive statistics
# -----------------------------------------------------------------------------

build_descriptives <- function(data, var_spec) {
  trt_idx <- data$treated_in_year == 1
  ctl_idx <- data$treated_in_year == 0
  var_spec |>
    mutate(
      treated = map_chr(var, ~ {
        x <- mask_imputed(data, .x)
        if (.x %in% school_pct_vars) x <- x / 100  # 0-100 → 0-1
        format_cell(x[trt_idx], data$weights[trt_idx], !(.x %in% continuous_vars))
      }),
      control = map_chr(var, ~ {
        x <- mask_imputed(data, .x)
        if (.x %in% school_pct_vars) x <- x / 100
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

table1_df <- desc_combined |>
  select(label, pa_treated, pa_control, all_treated, all_control)

table1_kbl <- table1_df |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r", "r"),
    col.names = c(
      "",
      sprintf("Treated ($n=%d$)",    n_pa_t),
      sprintf("Comparison ($n=%d$)", n_pa_c),
      sprintf("Treated ($n=%d$)",    n_all_t),
      sprintf("Comparison ($n=%d$)", n_all_c)
    ),
    caption  = "Sample Descriptive Statistics for the Matched Treated and Comparison Groups",
    label    = "descriptives",
    linesep  = ""
  ) |>
  add_header_above(c(" " = 1, "PA public schools" = 2, "All states" = 2)) |>
  pack_rows(index = group_index(desc_combined$group), italic = TRUE, bold = FALSE) |>
  kable_styling(latex_options = c("hold_position"))

write_tex(table1_kbl, file.path(out_dir, "table1_descriptives.tex"))

# -----------------------------------------------------------------------------
# Table 2 — Covariate balance (already on standardized / ratio scales)
# -----------------------------------------------------------------------------

bal_to_df <- function(matchit_obj) {
  bt <- bal.tab(
    matchit_obj,
    un    = TRUE,
    abs   = FALSE,
    stats = c("mean.diffs", "variance.ratios")
  )$Balance
  tibble::tibble(
    var     = rownames(bt),
    smd_un  = bt[["Diff.Un"]],
    smd_adj = bt[["Diff.Adj"]],
    vr_adj  = bt[["V.Ratio.Adj"]]
  )
}

bal_pa  <- bal_to_df(m_pa)
bal_all <- bal_to_df(m_all)

label_lookup <- bind_rows(individual_vars, school_vars) |>
  select(var, label, group)

bal_combined <- label_lookup |>
  left_join(bal_pa  |> rename_with(~ paste0("pa_",  .), -var), by = "var") |>
  left_join(bal_all |> rename_with(~ paste0("all_", .), -var), by = "var")

format_smd <- function(x) {
  ifelse(is.na(x), "--", formatC(x, digits = 3, format = "f"))
}
format_vr <- function(x, var) {
  ifelse(is.na(x) | !(var %in% continuous_vars), "--",
         formatC(x, digits = 2, format = "f"))
}

table2_df <- bal_combined |>
  mutate(
    pa_un   = format_smd(pa_smd_un),
    pa_adj  = format_smd(pa_smd_adj),
    pa_vr   = format_vr(pa_vr_adj, var),
    all_un  = format_smd(all_smd_un),
    all_adj = format_smd(all_smd_adj),
    all_vr  = format_vr(all_vr_adj, var)
  ) |>
  select(label, group, pa_un, pa_adj, pa_vr, all_un, all_adj, all_vr)

table2_kbl <- table2_df |>
  select(-group) |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r", "r", "r", "r"),
    col.names = c(
      "",
      "Unadj. SMD", "Matched SMD", "Var. ratio",
      "Unadj. SMD", "Matched SMD", "Var. ratio"
    ),
    caption  = "Standardized Mean Differences and Variance Ratios Before and After Matching",
    label    = "balance",
    linesep  = ""
  ) |>
  add_header_above(c(" " = 1, "PA public schools" = 3, "All states" = 3)) |>
  pack_rows(index = group_index(table2_df$group), italic = TRUE, bold = FALSE) |>
  kable_styling(latex_options = c("hold_position"))

write_tex(table2_kbl, file.path(out_dir, "table2_balance.tex"))

# -----------------------------------------------------------------------------
# Table 3 — Estimated treatment effects (proportions)
# -----------------------------------------------------------------------------

panel_label <- c(
  A = "Panel A. Enrollment",
  B = "Panel B. Initial institution type",
  C = "Panel C. Persistence (conditional on enrollment)"
)

format_prop <- function(x, digits = 3) {
  ifelse(is.na(x), "--", formatC(x, digits = digits, format = "f"))
}

format_att <- function(att, se) {
  ifelse(
    is.na(att),
    "--",
    ifelse(
      is.na(se),
      formatC(att, digits = 3, format = "f"),
      sprintf("%s (%s)",
              formatC(att, digits = 3, format = "f"),
              formatC(se,  digits = 3, format = "f"))
    )
  )
}

format_ci <- function(lo, hi) {
  ifelse(
    is.na(lo) | is.na(hi),
    "--",
    sprintf("[%.3f, %.3f]", lo, hi)
  )
}

stars <- function(p) {
  case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "$^{***}$",
    p < 0.01  ~ "$^{**}$",
    p < 0.05  ~ "$^{*}$",
    p < 0.10  ~ "$^{\\dagger}$",
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
      n      = n_obs,
      ctrl   = format_prop(ctrl_mean),
      att_se = paste0(format_att(att, se), stars(pval)),
      ci     = format_ci(conf_lo, conf_hi)
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
  select(label, panel_full,
         pa_n,  pa_ctrl,  pa_att_se,  pa_ci,
         all_n, all_ctrl, all_att_se, all_ci)

table3_kbl <- impact_wide |>
  select(-panel_full) |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r", "r", "r", "r", "r", "r"),
    col.names = c(
      "",
      "$n$", "Comparison", "ATT ($SE$)", "95\\% CI",
      "$n$", "Comparison", "ATT ($SE$)", "95\\% CI"
    ),
    caption = "Estimated Treatment Effects on College Enrollment, Institution Type, and Persistence",
    label   = "impact",
    linesep = ""
  ) |>
  add_header_above(c(" " = 1, "PA public schools" = 4, "All states" = 4)) |>
  pack_rows(index = group_index(impact_wide$panel_full), italic = TRUE, bold = FALSE) |>
  kable_styling(latex_options = c("hold_position"))

write_tex(table3_kbl, file.path(out_dir, "table3_impact.tex"))

message("\n=== LaTeX tables saved ===")
for (f in c("table1_descriptives.tex", "table2_balance.tex", "table3_impact.tex")) {
  message("  ", file.path(out_dir, f))
}

# -----------------------------------------------------------------------------
# Render each .tex to .png via pdflatex + pdftoppm
# -----------------------------------------------------------------------------
# Wraps each table.tex in a `standalone` document with `\standaloneenv{table}`
# so the PDF crops to the table content. `varwidth=30cm` allows the page to be
# wider than letter so the dual-sample tables aren't cut off.

render_one <- function(tex_path) {
  pdflatex <- Sys.which("pdflatex")
  pdftoppm <- Sys.which("pdftoppm")
  if (!nzchar(pdflatex) || !nzchar(pdftoppm)) {
    message("  Skipping PNG render: pdflatex or pdftoppm not on PATH.")
    return(invisible(NULL))
  }

  base <- tools::file_path_sans_ext(basename(tex_path))
  tmp  <- tempfile(pattern = paste0(base, "_"), fileext = "")
  dir.create(tmp)
  wrap_name <- paste0(base, "_wrap.tex")
  wrap_path <- file.path(tmp, wrap_name)

  writeLines(c(
    "\\documentclass[border=8pt,varwidth=30cm]{standalone}",
    "\\usepackage{booktabs}",
    "\\standaloneenv{table}",
    "\\begin{document}",
    sprintf("\\input{%s}", normalizePath(tex_path)),
    "\\end{document}"
  ), wrap_path)

  old_wd <- setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)
  status <- system2(pdflatex, c("-interaction=batchmode", wrap_name),
                    stdout = FALSE, stderr = FALSE)
  setwd(old_wd)

  pdf_path <- file.path(tmp, paste0(base, "_wrap.pdf"))
  if (status != 0 || !file.exists(pdf_path)) {
    message("  pdflatex failed for ", basename(tex_path),
            " (see ", file.path(tmp, paste0(base, "_wrap.log")), ")")
    return(invisible(NULL))
  }

  png_prefix <- file.path(dirname(tex_path), base)
  system2(pdftoppm,
          c("-png", "-r", "200", shQuote(pdf_path), shQuote(png_prefix)),
          stdout = FALSE, stderr = FALSE)
  appended <- paste0(png_prefix, "-1.png")
  final    <- paste0(png_prefix, ".png")
  if (file.exists(appended)) file.rename(appended, final)
  unlink(tmp, recursive = TRUE)
  if (file.exists(final)) message("  ", final)
}

message("\n=== PNG renders ===")
for (f in c("table1_descriptives.tex", "table2_balance.tex", "table3_impact.tex")) {
  render_one(file.path(out_dir, f))
}
