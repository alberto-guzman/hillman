# OUTCOME ANALYSIS: Treatment Effects Following MatchIt Best Practices
# =============================================================================
# Reference: https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
# Estimand: ATT via linear probability model on matched data
# Model: lm() with matching weights
# SE approach: cluster-robust by subclass (matched pair) via avg_slopes()
#
# Sample splits:
#   - Enrollment outcomes: full matched sample (all cohorts)
#   - Degree outcomes: restricted to students with non-NA outcome (i.e.,
#     sufficient follow-up time to have completed a degree). NAs in the
#     degree variables already encode censoring from the data build step.
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

# Enrollment outcomes: no censoring concern, run on full sample
enrollment_outcomes <- c(
  "seamless_enroll",
  "seamless_enroll_stem",
  "enrolled_ever_nsc",
  "enrolled_ever_stem",
  "public4yr_initial",
  "x4yr_initial",
  "firsttime_fulltime"
)

# Degree/persistence outcomes: restricted to students with sufficient
# follow-up (non-NA). NAs encode censoring from the data build step.
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
  # Enrollment
  seamless_enroll = "Seamless enrollment (any)",
  seamless_enroll_stem = "Seamless STEM enrollment",
  enrolled_ever_nsc = "Ever enrolled (any)",
  enrolled_ever_stem = "Ever enrolled STEM",
  public4yr_initial = "Initial enrollment: public 4-year",
  x4yr_initial = "Initial enrollment: any 4-year",
  firsttime_fulltime = "First-time full-time enrollment",
  # Degrees
  degree_ever_nsc = "Ever earned degree (any)",
  degree_ever_stem_nsc = "Ever earned STEM degree",
  degree_6years_all_nsc = "Any degree within 6 years",
  bachdegree_6years_all_nsc = "Bachelor's degree within 6 years",
  degree_in6_grad = "Graduated within 6 years",
  ste_mdegree_in6_grad = "STEM degree within 6 years",
  # Retention & persistence
  reten_fall_enter = "Retained fall of entry year",
  reten_fall_enter_stem = "Retained fall of entry year (STEM)",
  reten_fall_enter2 = "Retained fall of 2nd year",
  reten_fall_enter_stem2 = "Retained fall of 2nd year (STEM)",
  pers_fall_enter = "Persisted fall of entry year",
  pers_fall_enter_stem = "Persisted fall of entry year (STEM)",
  pers_fall_enter3 = "Persisted fall of 3rd year"
)

# Covariates for doubly robust adjustment.
# Excluded:
#   - house_size: excluded from PS model for consistency
#   - us_citizen: constant post-filter (all students are U.S. citizens)
#   - us_citizen_miss: constant post-filter
#   - grade dummies (grade_9 through grade_12): collinear with grade (continuous)
#   - _miss indicators that are all-zero in matched data (no variation):
#     gender_miss, stipend_miss
# Retained _miss indicators with actual variation:
#   gpa_miss, psat_math_miss, neg_school_miss, first_gen_miss,
#   racially_marginalized_miss, bi_multi_racial_miss, urban_miss,
#   suburban_miss, rural_miss, disability_miss
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

# Linear probability model (lm) with matching weights.
# MatchIt recommendation for 1:m matching with replacement:
#   - Use weights from matched data
#   - avg_slopes() targets the ATT for the treated subsample
#   - vcovCL clusters SEs by subclass (matched pair)

fit_att <- function(data, outcome_var, predictors) {
  # Drop any _miss predictors that are constant in this subsample
  # (e.g. degree-eligible subset may have different variation than full sample)
  predictors <- predictors[sapply(predictors, \(p) {
    col <- data[[p]]
    !is.null(col) && length(unique(col[!is.na(col)])) > 1
  })]

  formula_str <- paste0(
    outcome_var,
    " ~ treated_in_year + ",
    paste(predictors, collapse = " + "),
    " + factor(year)" # cohort FE; matches exact = ~year from matchit
  )

  fit <- lm(
    as.formula(formula_str),
    data = data,
    weights = weights
  )

  # avg_slopes: ATT restricted to treated units, cluster-robust SE by subclass
  treated_data <- subset(data, treated_in_year == 1)

  att <- avg_slopes(
    fit,
    variables = "treated_in_year",
    vcov = sandwich::vcovCL(fit, cluster = data$subclass),
    newdata = treated_data,
    wts = "weights"
  )

  # Weighted means
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

# --- Enrollment: full matched sample -----------------------------------------
message("\n--- Enrollment outcomes (full sample) ---")

results_enrollment <- map(enrollment_outcomes, \(out) {
  message("  Fitting: ", out)
  fit_att(matched, out, covars)
}) |>
  list_rbind()

# --- Degree: restrict to students with sufficient follow-up ------------------
# NAs in degree variables encode censoring (not enough time to earn a degree).
# We subset to non-NA for each outcome separately so each model uses as many
# observations as possible.
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

# --- Combine -----------------------------------------------------------------
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

# Run fit_att() separately for each GPA bin and outcome
# Uses the same LPM + avg_slopes() approach as the pooled model
# Note: small cell sizes mean wide CIs — interpret patterns cautiously

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

    # Skip if insufficient treated units
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

# Plot ATT + 95% CI by GPA bin
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

#need to add bin by 3 top 3.0
#rerun matching for historically marignalized and stiped
#dosage effect
#run for PA sample
