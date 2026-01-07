# =============================================================================
# OUTCOME ANALYSIS: Treatment Effects Following MatchIt Best Practices
# =============================================================================
# Purpose: Estimate treatment effects using MatchIt recommended approaches
# Reference: https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html
# Features: - Proper matching weights (accounts for replacement)
#           - Cluster-robust SEs by matched pair (subclass)
#           - G-computation for ATT with doubly robust estimation
#           - Pooled and year-specific estimates in percentage points
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(sandwich)
library(lmtest)
library(scales)
library(here)
library(broom)
library(patchwork)
library(modelsummary)

# =============================================================================
# 1. LOAD MATCHED DATASETS
# =============================================================================

# Load year-only matched data (PRIMARY)
matched_year <- readRDS(here("data", "matched_pa_students_year_only.rds"))

message("Year-only matched data loaded: ", nrow(matched_year), " observations")
message("  Treated: ", sum(matched_year$treated_in_year == 1))
message("  Control: ", sum(matched_year$treated_in_year == 0))

# Load year+school matched data (ROBUSTNESS)
matched_year_school <- readRDS(here(
  "data",
  "matched_pa_students_year_school.rds"
))

message(
  "\nYear+school matched data loaded: ",
  nrow(matched_year_school),
  " observations"
)
message("  Treated: ", sum(matched_year_school$treated_in_year == 1))
message("  Control: ", sum(matched_year_school$treated_in_year == 0))


# =============================================================================
# 2. DEFINE OUTCOMES AND COVARIATES
# =============================================================================

# Outcomes to analyze
outcomes <- c(
  "seamless_enroll",
  "seamless_enroll_stem",
  "enrolled_ever_nsc",
  "enrolled_ever_stem",
  "degree_ever_nsc",
  "degree_ever_stem_nsc",
  "bachdegree_6years_all_nsc"
)

# Outcome labels
outcome_labels <- c(
  seamless_enroll = "Seamless enrollment (any)",
  seamless_enroll_stem = "Seamless STEM enrollment",
  enrolled_ever_nsc = "Ever enrolled (any)",
  enrolled_ever_stem = "Ever enrolled STEM",
  degree_ever_nsc = "Ever earned degree (any)",
  degree_ever_stem_nsc = "Ever earned STEM degree",
  bachdegree_6years_all_nsc = "Bachelor's degree (6 years)"
)

# Covariates for doubly robust adjustment
covars <- c(
  "gender",
  "grade",
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

# Missing indicators
miss_indicators <- paste0(covars, "_miss")

# All predictors
all_predictors <- c(covars, miss_indicators)

# Pitt colors
pitt_royal <- "#003594"
pitt_gold <- "#FFB81C"

# =============================================================================
# 3. ESTIMATE POOLED ATT (ACROSS ALL YEARS) - SIMPLE LOOP
# =============================================================================

message("\n=== Estimating Pooled Treatment Effects ===\n")

# Year-only matching results
message("\n########## YEAR-ONLY MATCHING ##########\n")

for (outcome_var in outcomes) {
  message("\n========================================")
  message("Outcome: ", outcome_var)
  message("========================================\n")

  # Build formula
  formula_str <- paste0(
    outcome_var,
    " ~ treated_in_year + ",
    paste(all_predictors, collapse = " + "),
    " + factor(hs_grad_year)"
  )

  # Fit GLM
  fit <- glm(
    as.formula(formula_str),
    data = matched_year,
    weights = weights,
    family = logit() # Use gaussian, not binomial
  )

  # Print summary
  print(summary(fit))

  # Calculate control and treatment means
  control_mean <- mean(
    matched_year[[outcome_var]][matched_year$treated_in_year == 0],
    na.rm = TRUE
  )
  treatment_mean <- mean(
    matched_year[[outcome_var]][matched_year$treated_in_year == 1],
    na.rm = TRUE
  )
  diff_means <- treatment_mean - control_mean

  # Display results
  message("\n--- Summary Statistics ---")
  message(
    "Control Mean:    ",
    sprintf("%.3f (%.1f%%)", control_mean, control_mean * 100)
  )
  message(
    "Treatment Mean:  ",
    sprintf("%.3f (%.1f%%)", treatment_mean, treatment_mean * 100)
  )
  message(
    "Difference:      ",
    sprintf("%.3f (%.1f pp)", diff_means, diff_means * 100)
  )
  message("\n")
}
