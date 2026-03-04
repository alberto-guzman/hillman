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
# Input:   data/matched_all_states_year_only.rds
# Output:  output/att_results_all_states_year_only.rds
# =============================================================================

library(dplyr)
library(purrr)
library(marginaleffects)
library(sandwich)
library(lmtest)
library(here)

# =============================================================================
# 1. LOAD MATCHED DATASET
# =============================================================================

matched <- readRDS(here("data", "matched_all_states_year_only.rds"))

message(
  "Matched data: ",
  nrow(matched),
  " obs | ",
  sum(matched$treated_in_year == 1),
  " treated / ",
  sum(matched$treated_in_year == 0),
  " control"
)

# =============================================================================
# 2. DEFINE OUTCOMES AND COVARIATES
# =============================================================================

# Enrollment outcomes have no censoring and are run on the full matched sample.
# Degree and persistence outcomes require sufficient follow-up; each is subset
# to non-NA observations independently to maximize sample size.

enrollment_outcomes <- c(
  "seamless_enroll",
  "seamless_enroll_stem",
  "enrolled_ever_nsc",
  "enrolled_ever_stem",
  "public4yr_initial",
  "x4yr_initial",
  "firsttime_fulltime"
)

degree_outcomes <- c(
  "degree_ever_nsc",
  "degree_ever_stem_nsc",
  "degree_6years_all_nsc",
  "bachdegree_6years_all_nsc",
  "reten_fall_enter",
  "reten_fall_enter_stem",
  "reten_fall_enter2",
  "reten_fall_enter_stem2",
  "pers_fall_enter",
  "pers_fall_enter_stem",
  "pers_fall_enter3",
  "degree_in6_grad",
  "ste_mdegree_in6_grad"
)

outcome_labels <- c(
  seamless_enroll = "Seamless enrollment (any)",
  seamless_enroll_stem = "Seamless STEM enrollment",
  enrolled_ever_nsc = "Ever enrolled (any)",
  enrolled_ever_stem = "Ever enrolled STEM",
  public4yr_initial = "Initial enrollment: public 4-year",
  x4yr_initial = "Initial enrollment: any 4-year",
  firsttime_fulltime = "First-time full-time enrollment",
  degree_ever_nsc = "Ever earned degree (any)",
  degree_ever_stem_nsc = "Ever earned STEM degree",
  degree_6years_all_nsc = "Any degree within 6 years",
  bachdegree_6years_all_nsc = "Bachelor's degree within 6 years",
  degree_in6_grad = "Graduated within 6 years",
  ste_mdegree_in6_grad = "STEM degree within 6 years",
  reten_fall_enter = "Retained fall of entry year",
  reten_fall_enter_stem = "Retained fall of entry year (STEM)",
  reten_fall_enter2 = "Retained fall of 2nd year",
  reten_fall_enter_stem2 = "Retained fall of 2nd year (STEM)",
  pers_fall_enter = "Persisted fall of entry year",
  pers_fall_enter_stem = "Persisted fall of entry year (STEM)",
  pers_fall_enter3 = "Persisted fall of 3rd year"
)

# Adjustment covariates for doubly robust estimation.
# Excluded from the outcome model (vs. the PS model):
#   - house_size, us_citizen (constant post-filter), grade dummies
#     (collinear with continuous grade), _miss flags with zero variation.
covars <- c(
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


# =============================================================================
# 3. HELPER: FIT LPM + EXTRACT ATT VIA avg_slopes()
# =============================================================================
# For each outcome, fit a weighted LPM with cohort FEs, then use avg_slopes()
# to estimate the ATT restricted to treated units with cluster-robust SEs.
# Constant _miss predictors in a given subsample are dropped automatically.

fit_att <- function(data, outcome_var, predictors) {
  # Drop any _miss predictors that are constant in this subsample
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

  att <- avg_slopes(
    fit,
    variables = "treated_in_year",
    vcov = sandwich::vcovCL(fit, cluster = data$subclass),
    newdata = treated_data,
    wts = "weights"
  )

  wt_mean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)

  ctrl_mean <- wt_mean(
    data[[outcome_var]][data$treated_in_year == 0],
    data$weights[data$treated_in_year == 0]
  )
  trt_mean <- wt_mean(
    data[[outcome_var]][data$treated_in_year == 1],
    data$weights[data$treated_in_year == 1]
  )

  tibble(
    outcome = outcome_var,
    label = outcome_labels[outcome_var],
    sample = "full",
    n_obs = nrow(data),
    n_treated = sum(data$treated_in_year == 1),
    n_control = sum(data$treated_in_year == 0),
    ctrl_mean = ctrl_mean,
    trt_mean = trt_mean,
    raw_diff_pp = (trt_mean - ctrl_mean) * 100,
    att_pp = att$estimate * 100,
    se_pp = att$std.error * 100,
    pval = att$p.value,
    conf_lo_pp = att$conf.low * 100,
    conf_hi_pp = att$conf.high * 100
  )
}

# =============================================================================
# 4. ATT ESTIMATES
# =============================================================================

# Enrollment outcomes: full matched sample
message("\n--- Enrollment outcomes (full sample) ---")

results_enrollment <- map(enrollment_outcomes, \(out) {
  message("  Fitting: ", out)
  fit_att(matched, out, covars)
}) |>
  list_rbind()

# Degree / persistence outcomes: non-censored subsample per outcome
message("\n--- Degree outcomes (non-censored subsample) ---")

results_degree <- map(degree_outcomes, \(out) {
  message("  Fitting: ", out)
  data_sub <- matched |> filter(!is.na(.data[[out]]))
  message(
    "    Sample after removing censored: ",
    nrow(data_sub),
    " obs (",
    sum(data_sub$treated_in_year == 1),
    " treated / ",
    sum(data_sub$treated_in_year == 0),
    " control)"
  )
  fit_att(data_sub, out, covars) |>
    mutate(sample = "degree_eligible")
}) |>
  list_rbind()

results <- bind_rows(results_enrollment, results_degree)

# =============================================================================
# 5. INSPECT RESULTS
# =============================================================================

results |>
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
# 6. SAVE
# =============================================================================

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}

saveRDS(results, here("output", "att_results_all_states_year_only.rds"))


# ...existing code...

# =============================================================================
# 7. HETEROGENEITY BY GPA BIN
# =============================================================================
# Re-run fit_att() separately within each GPA tertile for two focal outcomes:
# seamless STEM enrollment and 2nd-year retention. Small within-bin samples
# produce wide CIs; patterns should be interpreted cautiously.
# Results are visualized with a faceted dot-and-whisker plot.

gpa_bin_levels <- c("3.0 – 3.4", "3.5 – 3.9", "4.0+")

het_outcomes <- c("seamless_enroll_stem", "reten_fall_enter2")

het_labels <- c(
  seamless_enroll_stem = "Seamless STEM Enrollment",
  reten_fall_enter2 = "Retained into 2nd Year"
)

results_gpa_het <- expand.grid(
  gpa_bin = gpa_bin_levels,
  outcome = het_outcomes,
  stringsAsFactors = FALSE
) |>
  purrr::pmap(\(gpa_bin, outcome) {
    data_sub <- matched |>
      mutate(
        gpa_bin = case_when(
          gpa == 0 ~ NA_character_,
          gpa < 3.0 ~ "Below 3.0",
          gpa >= 3.0 & gpa < 3.5 ~ "3.0 – 3.4",
          gpa >= 3.5 & gpa < 4.0 ~ "3.5 – 3.9",
          gpa >= 4.0 ~ "4.0+"
        ),
      ) |>
      filter(gpa_bin == !!gpa_bin)

    if (sum(data_sub$treated_in_year == 1) < 5) {
      return(NULL)
    }

    fit_att(data_sub, outcome, covars) |>
      mutate(gpa_bin = gpa_bin)
  }) |>
  purrr::compact() |>
  purrr::list_rbind() |>
  mutate(
    gpa_bin = factor(gpa_bin, levels = gpa_bin_levels),
    outcome_label = het_labels[outcome]
  )

# Dot-and-whisker plot of ATT ± 95% CI by GPA bin
results_gpa_het |>
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = c("Seamless STEM Enrollment", "Retained into 2nd Year")
    )
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
  facet_wrap(~outcome_label) +
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
      "The comparison group consists of matched students with similar pre-treatment characteristics who did not participate in the program. ",
      "Students with missing GPA (n = 12) are excluded from this analysis. ",
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

# TODO: add bin for GPA < 3.0
# TODO: rerun matching for racially marginalized and stipend subgroups
# TODO: estimate dosage effects (total_times_treated)
# TODO: run analysis on PA public school sample (merged_df_pa)
