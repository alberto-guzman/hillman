# ===================== Libraries =====================
library(dplyr)
library(tidyr)
library(ggplot2)
library(marginaleffects)
library(scales)

# ===================== Inputs =====================
# Use merged_df (post-MatchIt data) with: treated_ever, weights, subclass,
# hs_grad_year, covariates & *_miss indicators, and the two outcomes

outcomes <- c("seamless_enroll_stem", "enrolled_ever_stem")

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

# ===================== Helper: ATT model → predicted means =====================
make_overall_bars <- function(y) {
  fmla <- as.formula(
    paste0(
      y,
      " ~ treated_ever + ",
      paste(c(covars, miss_indicators), collapse = " + "),
      " + factor(hs_grad_year)" # cohort fixed effects
    )
  )

  fit <- lm(fmla, data = merged_df, weights = weights)

  avg_predictions(
    fit,
    variables = list(treated_ever = c(0, 1)),
    newdata = subset(merged_df, treated_ever == 1), # ATT target
    vcov = ~subclass,
    type = "response"
  ) |>
    mutate(
      outcome = y,
      group_label = ifelse(
        treated_ever == 1,
        "Treated (model)",
        "Control (model)"
      )
    ) |>
    select(outcome, group_label, estimate, std.error, conf.low, conf.high)
}

# ===================== Build plotting data =====================
# Visual offset to lift error bars above the columns (keeps CI width intact)
err_offset <- 0.02
label_pad <- 0.005

plotdf <- bind_rows(lapply(outcomes, make_overall_bars)) %>%
  mutate(
    outcome = recode(
      outcome,
      seamless_enroll_stem = "Seamless STEM enrollment",
      enrolled_ever_stem = "Ever enrolled in STEM"
    ),
    outcome = factor(
      outcome,
      levels = c("Seamless STEM enrollment", "Ever enrolled in STEM")
    ),
    # Rename groups
    group_label = recode(
      group_label,
      "Control (model)" = "Applicants",
      "Treated (model)" = "Hillman Participants"
    ),
    # Preserve ordering: Applicants first, Hillman Participants second
    group_label = factor(
      group_label,
      levels = c("Applicants", "Hillman Participants")
    ),
    label_pct = percent(estimate, accuracy = 1),
    # Place labels just above the lifted error bars for clean clearance
    label_y = conf.high + err_offset + label_pad
  )

# For a little headroom above lifted error bars
upper_lim <- max(plotdf$label_y, na.rm = TRUE)
y_max <- ceiling((upper_lim + 0.02) * 20) / 20 # round up to nearest 5%

# ===================== Plot: grouped bars side-by-side =====================
pd <- position_dodge(width = 0.75)

p <- ggplot(plotdf, aes(x = outcome, y = estimate, fill = group_label)) +
  geom_col(width = 0.7, position = pd, alpha = 0.95) +
  # Lift the entire error bar segment upward by err_offset
  geom_errorbar(
    aes(ymin = conf.low + err_offset, ymax = conf.high + err_offset),
    position = pd,
    width = 0.15,
    linewidth = 0.6
  ) +
  # Percent labels above the (lifted) error bars
  geom_text(
    aes(y = label_y, label = label_pct),
    position = pd,
    vjust = 0, # sit just above the error bar
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
    title = "Hillman impact on STEM college enrollment",
    x = NULL,
    y = "Mean probability",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, margin = margin(b = 10)),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p
