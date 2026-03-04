# =============================================================================
# 5_1d_matching.R
#
# Purpose: Propensity score matching (PSM) on the all-states sample.
#          Estimates the ATT by matching treated students to controls with
#          similar pre-treatment characteristics, exact matching on year.
#
# Method:  Nearest-neighbor matching with replacement (caliper = 0.5 SD),
#          logistic propensity score, exact match on application year.
#          Missing covariates are mean-imputed; missing indicators are added.
#
# Input:   `merged_df_all` — all-states cleaned dataset (from script 4)
# Output:  data/matched_all_states_year_only.rds — matched dataset with weights
#          output/balance_table_all_states.html/.tex — covariate balance table
# =============================================================================

library(dplyr)
library(MatchIt)
library(cobalt)
library(gt)
library(here)
library(marginaleffects)
library(purrr)

# =============================================================================
# 1. DATA PREPARATION
# =============================================================================
# Start from the all-states dataset. Create grade dummies, add missing
# indicators for each covariate, and impute NAs to 0.
#
# Exclusions applied BEFORE imputation so they are based on true values:
#   - treated_before_2017 == 1: these students would contaminate the control
#     group (applied again post-2017 but already treated earlier)
#   - us_citizen == 0 (confirmed non-citizens): citizenship status is not
#     comparable to the treated group; NA citizenship is retained and handled
#     via the us_citizen_miss indicator in the PS model
#   - year == 2022: these students would be graduating in 2022–2023, giving
#     them insufficient follow-up time for most outcomes. They are excluded
#     entirely rather than kept only for enrollment outcomes to maintain a
#     single consistent matched sample.
#   - missing treated_in_year: data anomaly, cannot be classified

matching_data <- merged_df_all

covariates <- c(
  "gender",
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
  "us_citizen",
  "first_gen"
)

matching_data <- matching_data |>
  mutate(
    grade_9 = if_else(grade == 9, 1, 0),
    grade_10 = if_else(grade == 10, 1, 0),
    grade_11 = if_else(grade == 11, 1, 0),
    grade_12 = if_else(grade == 12, 1, 0)
  )

# Apply exclusion filters BEFORE imputation to avoid conflating true 0s with
# imputed 0s. Specifically, us_citizen == 0 (confirmed non-citizen) is
# excluded here; students with us_citizen = NA are retained and their
# missingness is captured by the us_citizen_miss indicator below.
matching_data <- matching_data |>
  filter(
    !is.na(treated_in_year),
    year != 2022,
    us_citizen != 0 | is.na(us_citizen),
    treated_before_2017 == 0
  )

# Add missing indicators then impute remaining NAs to 0
matching_data <- matching_data |>
  mutate(
    across(
      all_of(covariates),
      list(miss = ~ if_else(is.na(.), 1, 0)),
      .names = "{col}_miss"
    )
  ) |>
  mutate(across(all_of(covariates), ~ replace_na(., 0)))

# =============================================================================
# 2. PROPENSITY SCORE MATCHING
# =============================================================================
# Nearest-neighbor 1:m matching with replacement. Exact match on year
# ensures within-cohort comparisons. Caliper of 0.5 SD on the logit
# of the propensity score prevents poor matches.

m.out_year <- matchit(
  treated_in_year ~
    gender +
    grade_9 +
    grade_10 +
    grade_11 +
    grade_12 +
    gpa +
    psat_math +
    stipend +
    racially_marginalized +
    bi_multi_racial +
    urban +
    suburban +
    rural +
    disability +
    neg_school +
    us_citizen +
    first_gen +
    gender_miss +
    gpa_miss +
    psat_math_miss +
    stipend_miss +
    racially_marginalized_miss +
    bi_multi_racial_miss +
    urban_miss +
    suburban_miss +
    rural_miss +
    disability_miss +
    neg_school_miss +
    us_citizen_miss +
    first_gen_miss,
  data = matching_data,
  method = "nearest",
  exact = ~year,
  distance = "glm",
  caliper = .5,
  replace = TRUE
)

# =============================================================================
# 3. BALANCE ASSESSMENT
# =============================================================================
# Inspect overall and within-year balance. The cobalt bal.tab() output
# shows standardized mean differences (SMD) before and after matching.

summary(m.out_year)
bal.tab(m.out_year, un = TRUE, thresholds = c(m = 0.2))
bal.tab(m.out_year, cluster = "year", un = TRUE, thresholds = c(m = 0.2))

matched_data_all_year <- match.data(m.out_year)

# =============================================================================
# 4. BALANCE TABLE (gt)
# =============================================================================
# Compute weighted means and SDs before and after matching for each covariate,
# then format as a publication-ready gt table with SMD columns.

bal_stats <- bal.tab(m.out_year, un = TRUE)
bal_df <- bal_stats$Balance

# Means and SDs before matching
before_stats <- matching_data |>
  group_by(treated_in_year) |>
  summarise(
    across(
      c(
        gender,
        grade_9,
        grade_10,
        grade_11,
        grade_12,
        gpa,
        psat_math,
        stipend,
        racially_marginalized,
        bi_multi_racial,
        urban,
        suburban,
        rural,
        disability,
        neg_school,
        first_gen,
        gpa_miss,
        psat_math_miss,
        neg_school_miss,
        first_gen_miss
      ),
      list(mean = ~ mean(., na.rm = TRUE), sd = ~ sd(., na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    -treated_in_year,
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(mean|sd)"
  ) |>
  pivot_wider(
    names_from = treated_in_year,
    values_from = c(mean, sd),
    names_glue = "{.value}_{treated_in_year}"
  )

# Weighted means and SDs after matching
# Note: both mean and SD are weighted to correctly represent the effective
# sample under matching with replacement (controls reused multiple times
# receive higher weight and should contribute less variance).
after_stats <- matched_data_all_year |>
  group_by(treated_in_year) |>
  summarise(
    across(
      c(
        gender,
        grade_9,
        grade_10,
        grade_11,
        grade_12,
        gpa,
        psat_math,
        stipend,
        racially_marginalized,
        bi_multi_racial,
        urban,
        suburban,
        rural,
        disability,
        neg_school,
        first_gen,
        gpa_miss,
        psat_math_miss,
        neg_school_miss,
        first_gen_miss
      ),
      list(
        mean = ~ weighted.mean(., w = weights, na.rm = TRUE),
        sd = ~ sqrt(Hmisc::wtd.var(., weights = weights, na.rm = TRUE))
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    -treated_in_year,
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(mean|sd)"
  ) |>
  pivot_wider(
    names_from = treated_in_year,
    values_from = c(mean, sd),
    names_glue = "{.value}_{treated_in_year}"
  )

# Combine and label
balance_table <- bal_df |>
  tibble::rownames_to_column("variable") |>
  filter(
    (!grepl("^(us_citizen|year|distance)$", variable) &
      !grepl("_miss$", variable)) |
      variable %in%
        c("gpa_miss", "psat_math_miss", "neg_school_miss", "first_gen_miss")
  ) |>
  left_join(before_stats, by = "variable") |>
  left_join(after_stats, by = "variable", suffix = c("_before", "_after")) |>
  mutate(
    Variable = case_when(
      variable == "gender" ~ "Female",
      variable == "grade_9" ~ "Grade 9",
      variable == "grade_10" ~ "Grade 10",
      variable == "grade_11" ~ "Grade 11",
      variable == "grade_12" ~ "Grade 12",
      variable == "gpa" ~ "GPA",
      variable == "psat_math" ~ "PSAT Math",
      variable == "stipend" ~ "Received Stipend",
      variable == "racially_marginalized" ~ "Racially Marginalized",
      variable == "bi_multi_racial" ~ "Bi/Multi-Racial",
      variable == "urban" ~ "Urban",
      variable == "suburban" ~ "Suburban",
      variable == "rural" ~ "Rural",
      variable == "disability" ~ "Disability",
      variable == "neg_school" ~ "Negative School Environment",
      variable == "first_gen" ~ "First Generation",
      variable == "gpa_miss" ~ "GPA (Missing)",
      variable == "psat_math_miss" ~ "PSAT Math (Missing)",
      variable == "neg_school_miss" ~ "Negative School Environment (Missing)",
      variable == "first_gen_miss" ~ "First Generation (Missing)",
      TRUE ~ variable
    ),
    Treated_Before = paste0(
      sprintf("%.3f", mean_1_before),
      " (",
      sprintf("%.3f", sd_1_before),
      ")"
    ),
    Control_Before = paste0(
      sprintf("%.3f", mean_0_before),
      " (",
      sprintf("%.3f", sd_0_before),
      ")"
    ),
    Treated_After = paste0(
      sprintf("%.3f", mean_1_after),
      " (",
      sprintf("%.3f", sd_1_after),
      ")"
    ),
    Control_After = paste0(
      sprintf("%.3f", mean_0_after),
      " (",
      sprintf("%.3f", sd_0_after),
      ")"
    )
  ) |>
  select(
    Variable,
    Treated_Before,
    Control_Before,
    SMD_Before = Diff.Un,
    Treated_After,
    Control_After,
    SMD_After = Diff.Adj
  )

# Format as gt table
balance_gt <- balance_table |>
  gt() |>
  tab_spanner(
    label = "Before Matching",
    columns = c(Treated_Before, Control_Before, SMD_Before)
  ) |>
  tab_spanner(
    label = "After Matching",
    columns = c(Treated_After, Control_After, SMD_After)
  ) |>
  cols_label(
    Variable = "",
    Treated_Before = "Treatment",
    Control_Before = "Control",
    SMD_Before = "SMD",
    Treated_After = "Treatment",
    Control_After = "Control",
    SMD_After = "SMD"
  ) |>
  tab_header(
    title = md(
      "**Table 1: Covariate Balance Before and After Propensity Score Matching**"
    ),
    subtitle = "All States, Year-Only Matching"
  ) |>
  fmt_number(columns = starts_with("SMD"), decimals = 3) |>
  cols_align(align = "left", columns = Variable) |>
  cols_align(
    align = "center",
    columns = c(
      Treated_Before,
      Control_Before,
      SMD_Before,
      Treated_After,
      Control_After,
      SMD_After
    )
  ) |>
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(2)),
    locations = cells_body(rows = 1)
  ) |>
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
    locations = cells_body(rows = nrow(balance_table))
  ) |>
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "gray70",
      weight = px(1),
      style = "dashed"
    ),
    locations = cells_body(rows = Variable %in% c("GPA (Missing)"))
  ) |>
  tab_source_note(
    source_note = md(
      "*Notes:* SMD = Standardized Mean Difference. Values |SMD| < 0.2 indicate adequate balance. Post-matching means are weighted by matching weights to account for matched units used more than once (matching with replacement)."
    )
  ) |>
  tab_source_note(
    source_note = md(paste0(
      "N = ",
      nrow(matching_data),
      " students before matching; N = ",
      nrow(matched_data_all_year),
      " after matching (",
      sum(matched_data_all_year$treated_in_year == 1),
      " treatment, ",
      sum(matched_data_all_year$treated_in_year == 0),
      " control)."
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

balance_gt

# =============================================================================
# 5. SAVE OUTPUTS
# =============================================================================

saveRDS(matched_data_all_year, here("data", "matched_all_states_year_only.rds"))

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
gtsave(balance_gt, here("output", "balance_table_all_states.html"))
gtsave(balance_gt, here("output", "balance_table_all_states.tex"))
# =============================================================================
