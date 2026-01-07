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

# =============================================================================
# 1. LOAD MATCHED DATASETS
# =============================================================================

# Load year-only matched data (PRIMARY)
matched_year <- readRDS(here("data", "matched_pa_students_year_only.rds"))

message("Year-only matched data loaded: ", nrow(matched_year), " observations")
message("  Treated: ", sum(matched_year$treated_in_year == 1))
message("  Control: ", sum(matched_year$treated_in_year == 0))
message(
  "  Unique controls: ",
  length(unique(matched_year$subclass[matched_year$treated_in_year == 0]))
)

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
message(
  "  Unique controls: ",
  length(unique(matched_year_school$subclass[
    matched_year_school$treated_in_year == 0
  ]))
)

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
  "bachdegree_6years_all_nsc",
  "ste_mbachdegree_6years_all_nsc"
)

# Outcome labels
outcome_labels <- c(
  seamless_enroll = "Seamless enrollment (any)",
  seamless_enroll_stem = "Seamless STEM enrollment",
  enrolled_ever_nsc = "Ever enrolled (any)",
  enrolled_ever_stem = "Ever enrolled STEM",
  degree_ever_nsc = "Ever earned degree (any)",
  degree_ever_stem_nsc = "Ever earned STEM degree",
  bachdegree_6years_all_nsc = "Bachelor's degree (6 years)",
  ste_mbachdegree_6years_all_nsc = "STEM bachelor's (6 years)"
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
