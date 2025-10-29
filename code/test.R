# ===================== Libraries =====================
library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(scales)
library(gt)
library(purrr)

# ===================== Inputs =====================
# Define outcome variables of interest
outcomes <- c(
  "seamless_enroll",
  "seamless_enroll_stem",
  "enrolled_ever_nsc",
  "enrolled_ever_stem",
  "degree_ever_nsc",
  "degree_ever_stem_nsc",
  "degree_6years_all_nsc",
  "bachdegree_6years_all_nsc",
  "ste_mbachdegree_6years_all_nsc"
)

# Covariates and missing indicators
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
  "us_citizen"
)

miss_indicators <- c(
  "gender_miss",
  "grade_miss",
  "gpa_miss",
  "psat_math_miss",
  "stipend_miss",
  "house_size_miss",
  "racially_marginalized_miss",
  "bi_multi_racial_miss",
  "urban_miss",
  "suburban_miss",
  "rural_miss",
  "disability_miss",
  "neg_school_miss",
  "us_citizen_miss"
)

# Pitt colors
pitt_royal <- "#003594"
pitt_gold <- "#FFB81C"

# ===================== Prep outcomes =====================
merged_df <- merged_df %>%
  mutate(across(all_of(outcomes), ~ tidyr::replace_na(., 0)))

# ===================== ATT Modeling Function =====================
run_att_model <- function(outcome) {
  fmla <- as.formula(
    paste0(
      outcome,
      " ~ treated_ever + ",
      paste(c(covars, miss_indicators), collapse = " + "),
      " + factor(hs_grad_year.x)"
    )
  )

  fit <- glm(fmla, data = merged_df, weights = weights)

  preds <- avg_predictions(
    fit,
    variables = list(treated_ever = c(0, 1)),
    newdata = subset(merged_df, treated_ever == 1),
    vcov = ~subclass,
    type = "response"
  ) %>%
    mutate(outcome = outcome)

  list(
    outcome = outcome,
    glm_summary = summary(fit),
    predictions = preds
  )
}

results_list <- list()

for (outcome in outcomes) {
  cat("\n=====================================\n")
  cat("OUTCOME:", outcome, "\n")
  cat("=====================================\n")

  result <- run_att_model(outcome)

  # Print glm summary
  print(result$glm_summary)

  # Print average predictions
  cat("\n--- Average Predictions ---\n")
  print(result$predictions)

  # Store results for later use
  results_list[[outcome]] <- result
}


# ===================== Run ATT Models for All Outcomes =====================
results_all <- purrr::map_dfr(outcomes, run_att_model)

# ===================== Format Results Table =====================
results_table <- results_all %>%
  mutate(
    Group = ifelse(treated_ever == 1, "Treated", "Control"),
    Outcome = recode(
      outcome,
      seamless_enroll = "Seamless college enrollment",
      seamless_enroll_stem = "Seamless STEM enrollment",
      enrolled_ever_nsc = "Ever enrolled in college",
      enrolled_ever_stem = "Ever enrolled in STEM",
      degree_ever_nsc = "Ever earned a degree",
      degree_ever_stem_nsc = "Ever earned STEM degree",
      degree_6years_all_nsc = "Any degree within 6 years",
      bachdegree_6years_all_nsc = "Bachelor's within 6 years",
      ste_mbachdegree_6years_all_nsc = "STEM Bachelor's in 6 years"
    )
  ) %>%
  select(Outcome, Group, estimate, conf.low, conf.high)

# ===================== Display Table with gt =====================
results_table %>%
  gt(rowname_col = "Outcome", groupname_col = "Group") %>%
  tab_header(
    title = "Model-Adjusted Probabilities",
    subtitle = "ATT Estimates by Outcome and Group"
  ) %>%
  fmt_percent(
    columns = c(estimate, conf.low, conf.high),
    decimals = 1
  ) %>%
  cols_label(
    estimate = "Estimate",
    conf.low = "Lower CI",
    conf.high = "Upper CI"
  ) %>%
  tab_options(
    table.font.size = px(13),
    column_labels.font.weight = "bold",
    table.width = pct(100)
  )

# ===================== Plot Key Outcomes (Bar Plot) =====================
plot_outcomes <- c("seamless_enroll_stem", "enrolled_ever_stem")

plotdf <- results_all %>%
  filter(outcome %in% plot_outcomes) %>%
  mutate(
    outcome = recode(
      outcome,
      seamless_enroll_stem = "Seamless STEM enrollment",
      enrolled_ever_stem = "Ever enrolled in STEM"
    ),
    group_label = ifelse(
      treated_ever == 1,
      "Hillman Participants",
      "Applicants"
    ),
    label_pct = percent(estimate, accuracy = 1),
    label_y = conf.high + 0.025
  )

# Plot setup
pd <- position_dodge(width = 0.75)
upper_lim <- max(plotdf$label_y, na.rm = TRUE)
y_max <- ceiling((upper_lim + 0.02) * 20) / 20

ggplot(plotdf, aes(x = outcome, y = estimate, fill = group_label)) +
  geom_col(width = 0.7, position = pd, alpha = 0.95) +
  geom_errorbar(
    aes(ymin = conf.low + 0.02, ymax = conf.high + 0.02),
    position = pd,
    width = 0.15,
    linewidth = 0.6
  ) +
  geom_text(
    aes(y = label_y, label = label_pct),
    position = pd,
    vjust = 0,
    size = 3.8,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Applicants" = pitt_gold, "Hillman Participants" = pitt_royal)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 5),
    limits = c(0, y_max)
  ) +
  labs(
    title = "Hillman Impact on STEM College Enrollment",
    x = NULL,
    y = "Model-Adjusted Probability",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
