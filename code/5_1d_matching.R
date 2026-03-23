# =============================================================================
# 5_1d_matching.R
#
# Purpose: Propensity score matching (PSM) for two samples:
#            1. All states (merged_df_all)
#            2. PA public schools (merged_df_pa) — adds school-level covariates
#
# Method:  Nearest-neighbor matching with replacement (caliper = 0.5 SD),
#          logistic propensity score, exact match on application year.
#          Missing covariates are mean-imputed; missing indicators are added.
#
# Input:   `merged_df_all` — all-states cleaned dataset (from script 4)
#          `merged_df_pa`  — PA public school dataset (from script 4)
# Output:  data/matched_all_states_year_only.rds
#          data/matched_pa_year_only.rds
#          output/balance_table_all_states.html/.tex
#          output/balance_table_pa.html/.tex
# =============================================================================

library(dplyr)
library(MatchIt)
library(cobalt)
library(gt)
library(here)
library(purrr)

# =============================================================================
# SHARED COVARIATES
# =============================================================================
# Base covariates used in both samples. The PA model adds school-level
# covariates on top of these.
# house_size included as SES proxy alongside stipend.
# gender cast to integer here — sourced as dbl from applicant clean script.

base_covariates <- c(
  "gender",
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
  "first_gen"
)

pa_extra_covariates <- c(
  "school_enrollment",
  "school_pct_econ_disadvantaged",
  "school_pct_english_learner",
  "school_pct_special_ed",
  "school_pct_white"
  # school_title_i excluded — high collinearity with school_pct_econ_disadvantaged
)

# Display labels for balance tables — human-readable names aligned to
# base_covariates + grade dummies
display_covars_all <- c(
  "gender",
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
  "grade_9",
  "grade_10",
  "grade_11",
  "grade_12"
)

display_covars_pa <- c(
  display_covars_all,
  "school_enrollment",
  "school_pct_econ_disadvantaged",
  "school_pct_english_learner",
  "school_pct_special_ed",
  "school_pct_white"
)

# =============================================================================
# HELPER: BUILD BALANCE TABLE (gt)
# =============================================================================
# Compares covariate means before and after matching. Reports standardized
# mean differences (SMD) for each covariate. Sample sizes (N treated / N
# control) before and after matching are included in the table header.

build_balance_gt <- function(
  m.out,
  display_covars,
  title_subtitle
) {
  bal <- bal.tab(
    m.out,
    un = TRUE,
    disp = c("means", "sds"),
    thresholds = c(m = 0.1)
  )$Balance

  bal <- bal |>
    tibble::rownames_to_column("covariate") |>
    filter(covariate %in% display_covars) |>
    select(
      covariate,
      mean_un_control = M.0.Un,
      mean_un_treated = M.1.Un,
      smd_before = Diff.Un,
      mean_adj_control = M.0.Adj,
      mean_adj_treated = M.1.Adj,
      smd_after = Diff.Adj
    ) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  # sample sizes before and after matching
  md_pre <- m.out$model$data
  md_post <- match.data(m.out)

  n_pre_treated <- sum(md_pre$treated_in_year == 1, na.rm = TRUE)
  n_pre_control <- sum(md_pre$treated_in_year == 0, na.rm = TRUE)
  n_post_treated <- sum(md_post$treated_in_year == 1, na.rm = TRUE)
  n_post_control <- sum(md_post$treated_in_year == 0, na.rm = TRUE)

  n_note <- paste0(
    "Before matching: N treated = ",
    n_pre_treated,
    ", N control = ",
    n_pre_control,
    " | After matching: N treated = ",
    n_post_treated,
    ", N control = ",
    n_post_control
  )

  n_rows <- nrow(bal)

  bal |>
    gt(rowname_col = "covariate") |>
    tab_header(
      title = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    tab_spanner(
      label = "Before Matching",
      columns = c(mean_un_control, mean_un_treated, smd_before)
    ) |>
    tab_spanner(
      label = "After Matching",
      columns = c(mean_adj_control, mean_adj_treated, smd_after)
    ) |>
    cols_label(
      mean_un_treated = "Treated",
      mean_un_control = "Control",
      smd_before = "SMD",
      mean_adj_treated = "Treated",
      mean_adj_control = "Control",
      smd_after = "SMD"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = "covariate") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = n_rows)
    ) |>
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = cells_body(rows = n_rows)
    ) |>
    tab_source_note(source_note = md(n_note)) |>
    tab_source_note(
      source_note = md(paste0(
        "*Notes:* SMD = standardized mean difference. ",
        "Matching: nearest-neighbor with replacement, caliper = 0.5 SD, ",
        "exact match on application year."
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

# =============================================================================
# HELPER: PREPARE MATCHING DATA
# =============================================================================
# Applies exclusions (before imputation), casts gender to integer, adds grade
# dummies, builds _miss indicators, then imputes remaining NAs to 0.
#
# Exclusions applied on true values before imputation:
#   - treated_before_2017 == 1: would contaminate control group
#   - us_citizen == 0 (confirmed non-citizen): NA citizenship retained and
#     handled via us_citizen_miss in the PS model
#   - year == 2022: excluded because reten_1y/pers_1y are all NA for this
#     cohort (no fall 2023 follow-up in NSC file). Keeps analytic sample
#     consistent with outcome availability.
#   - missing treated_in_year: data anomaly

prepare_matching_data <- function(data, covariates, exclude_years = NULL) {
  data <- data |>
    mutate(
      gender = as.integer(gender),
      grade_9 = if_else(grade == 9, 1L, 0L),
      grade_10 = if_else(grade == 10, 1L, 0L),
      grade_11 = if_else(grade == 11, 1L, 0L),
      grade_12 = if_else(grade == 12, 1L, 0L)
    )

  data <- data |>
    filter(
      !is.na(treated_in_year),
      !year %in% c(2022, exclude_years),
      us_citizen != 0 | is.na(us_citizen),
      treated_before_2017 == 0
    )

  data <- data |>
    mutate(
      across(
        all_of(covariates),
        list(miss = ~ if_else(is.na(.), 1L, 0L)),
        .names = "{col}_miss"
      )
    ) |>
    mutate(across(all_of(covariates), ~ replace_na(., 0)))

  data
}

# =============================================================================
# HELPER: RUN MATCHING
# =============================================================================

run_matching <- function(data, covariates) {
  miss_vars <- paste0(covariates, "_miss")

  ps_formula <- reformulate(
    termlabels = c(
      "grade_9",
      "grade_10",
      "grade_11",
      "grade_12",
      covariates,
      miss_vars
    ),
    response = "treated_in_year"
  )

  matchit(
    ps_formula,
    data = data,
    method = "nearest",
    exact = ~year,
    distance = "glm",
    caliper = 0.5,          # 0.5 pooled SDs of the propensity score
    caliper.d = "pooled SD",
    replace = TRUE
  )
}

# =============================================================================
# 1. ALL-STATES MATCH
# =============================================================================

message("\n=== ALL-STATES MATCHING ===\n")

matching_data_all <- prepare_matching_data(merged_df_all, base_covariates)

message(
  "Matching sample: ",
  nrow(matching_data_all),
  " students (",
  sum(matching_data_all$treated_in_year == 1),
  " treated / ",
  sum(matching_data_all$treated_in_year == 0),
  " control)"
)

m.out_all <- run_matching(matching_data_all, base_covariates)

summary(m.out_all)
bal.tab(m.out_all, un = TRUE, thresholds = c(m = 0.1))
bal.tab(m.out_all, cluster = "year", un = TRUE, thresholds = c(m = 0.1))

matched_data_all <- match.data(m.out_all)

message(
  "Matched sample (all states): ",
  nrow(matched_data_all),
  " students (",
  sum(matched_data_all$treated_in_year == 1),
  " treated / ",
  sum(matched_data_all$treated_in_year == 0),
  " control)"
)

# =============================================================================
# 2. BALANCE TABLE — ALL STATES
# =============================================================================

balance_gt_all <- build_balance_gt(
  m.out = m.out_all,
  display_covars = display_covars_all,
  title_subtitle = c(
    "Table 1: Covariate Balance Before and After Propensity Score Matching",
    "All States, Year-Only Matching"
  )
)

balance_gt_all

# =============================================================================
# 3. PA PUBLIC SCHOOL MATCH
# =============================================================================
# Adds school-level covariates (enrollment, % econ. disadvantaged, etc.) to
# the PS model. school_title_i excluded due to high collinearity with
# school_pct_econ_disadvantaged — its information is largely captured there.

message("\n=== PA PUBLIC SCHOOL MATCHING ===\n")

pa_covariates <- c(base_covariates, pa_extra_covariates)
# PA: exclude 2023 — only 1 treated student matched, no degree/persistence
# outcomes available for this cohort
matching_data_pa <- prepare_matching_data(
  merged_df_pa,
  pa_covariates,
  exclude_years = 2023
)

message(
  "Matching sample (PA): ",
  nrow(matching_data_pa),
  " students (",
  sum(matching_data_pa$treated_in_year == 1),
  " treated / ",
  sum(matching_data_pa$treated_in_year == 0),
  " control)"
)

m.out_pa <- run_matching(matching_data_pa, pa_covariates)

summary(m.out_pa)
bal.tab(m.out_pa, un = TRUE, thresholds = c(m = 0.1))
bal.tab(m.out_pa, cluster = "year", un = TRUE, thresholds = c(m = 0.1))

matched_data_pa <- match.data(m.out_pa)

message(
  "Matched sample (PA): ",
  nrow(matched_data_pa),
  " students (",
  sum(matched_data_pa$treated_in_year == 1),
  " treated / ",
  sum(matched_data_pa$treated_in_year == 0),
  " control)"
)

# =============================================================================
# 4. BALANCE TABLE — PA PUBLIC SCHOOLS
# =============================================================================

balance_gt_pa <- build_balance_gt(
  m.out = m.out_pa,
  display_covars = display_covars_pa,
  title_subtitle = c(
    "Table 1: Covariate Balance Before and After Propensity Score Matching",
    "PA Public Schools, Year-Only Matching"
  )
)

balance_gt_pa

# =============================================================================
# 5. SAMPLE SIZE TABLES BY YEAR
# =============================================================================
# Shows how many treated and control students were retained or dropped by
# matching in each cohort.

make_sample_table <- function(pre_data, matched_data, title_subtitle) {
  pre <- pre_data |>
    group_by(year) |>
    summarise(
      pre_treated = sum(treated_in_year == 1),
      pre_control = sum(treated_in_year == 0),
      .groups = "drop"
    )

  post <- matched_data |>
    group_by(year) |>
    summarise(
      post_treated = sum(treated_in_year == 1),
      post_control = sum(treated_in_year == 0),
      .groups = "drop"
    )

  tbl <- pre |>
    left_join(post, by = "year") |>
    mutate(year = as.character(year))

  totals <- tbl |>
    summarise(
      year = "Total",
      pre_treated = sum(pre_treated),
      pre_control = sum(pre_control),
      post_treated = sum(post_treated),
      post_control = sum(post_control)
    )

  tbl <- bind_rows(tbl, totals)
  n_data_rows <- nrow(tbl)

  tbl |>
    gt(rowname_col = "year") |>
    tab_header(
      title = md(paste0("**", title_subtitle[1], "**")),
      subtitle = title_subtitle[2]
    ) |>
    tab_spanner(
      label = "Before Matching",
      columns = c(pre_treated, pre_control)
    ) |>
    tab_spanner(
      label = "After Matching",
      columns = c(post_treated, post_control)
    ) |>
    cols_label(
      pre_treated = "Treated",
      pre_control = "Control",
      post_treated = "Treated",
      post_control = "Control"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    cols_align(align = "left", columns = "year") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = n_data_rows)
    ) |>
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(2)),
      locations = cells_body(rows = 1)
    ) |>
    tab_style(
      style = cell_borders(sides = "top", color = "black", weight = px(1.5)),
      locations = cells_body(rows = n_data_rows)
    ) |>
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = cells_body(rows = n_data_rows)
    ) |>
    tab_source_note(
      source_note = md(paste0(
        "*Notes:* Before-matching counts are after applying pre-match exclusions ",
        "(year 2022, confirmed non-citizens, treated before 2017). ",
        "After-matching counts are unique matched units; with replacement matching ",
        "the effective sample size (ESS) of controls is lower than shown."
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

sample_gt_all <- make_sample_table(
  pre_data = matching_data_all,
  matched_data = matched_data_all,
  title_subtitle = c(
    "Table: Sample Sizes Before and After Matching by Year",
    "All States"
  )
)

sample_gt_pa <- make_sample_table(
  pre_data = matching_data_pa,
  matched_data = matched_data_pa,
  title_subtitle = c(
    "Table: Sample Sizes Before and After Matching by Year",
    "PA Public Schools"
  )
)

sample_gt_all
sample_gt_pa

# =============================================================================
# 6. SAVE OUTPUTS
# =============================================================================

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "matched"), recursive = TRUE, showWarnings = FALSE)

saveRDS(matched_data_all, here("data", "matched", "matched_all_states_year_only.rds"))
saveRDS(matched_data_pa, here("data", "matched", "matched_pa_year_only.rds"))

gtsave(balance_gt_all, here("output", "tables", "balance_table_all_states.html"))
gtsave(balance_gt_all, here("output", "tables", "balance_table_all_states.tex"))
gtsave(balance_gt_pa, here("output", "tables", "balance_table_pa.html"))
gtsave(balance_gt_pa, here("output", "tables", "balance_table_pa.tex"))

message("\n=== Matching complete ===")
message(
  "Saved: matched_all_states_year_only.rds (",
  nrow(matched_data_all),
  " rows)"
)
message("Saved: matched_pa_year_only.rds (", nrow(matched_data_pa), " rows)")
# =============================================================================
