library(cobalt)

# List of columns to check and fill missing values
cols_to_fill <- c(
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

# Function to create individual missing indicators and fill missing values
merged_df_pa_covars <- merged_df_pa_covars %>%
  mutate(across(
    all_of(cols_to_fill),
    list(miss = ~ if_else(is.na(.), 1, 0)),
    .names = "{col}_miss"
  )) %>%
  mutate(across(all_of(cols_to_fill), ~ replace_na(., 0)))

# Remove all rows that have treated_in_year missing
merged_df_pa_covars <- merged_df_pa_covars[
  !is.na(merged_df_pa_covars$treated_in_year),
]
merged_df_pa_covars <- merged_df_pa_covars[!is.na(merged_df_pa_covars$aun), ]

#exact match by year
# Perform matching using the 'matchit' function with multiple covariates
m.out <- matchit(
  treated_in_year ~
    gender +
      grade +
      gpa +
      psat_math +
      stipend +
      house_size +
      racially_marginalized +
      bi_multi_racial +
      urban +
      suburban +
      rural +
      disability +
      neg_school +
      us_citizen +
      gpa_miss +
      psat_math_miss +
      stipend_miss +
      house_size_miss +
      racially_marginalized_miss +
      disability_miss +
      neg_school_miss +
      us_citizen_miss,
  data = merged_df_pa_covars, # Data source
  method = "nearest", # Matching method
  exact = ~ year + aun, # Exact matching variables
  distance = "glm", # Distance metric
  caliper = .25,
  std.caliper = TRUE, # Caliper settings
  replace = FALSE # Allow replacement in matching
)

# Plot a summary of the matched data
plot(summary(m.out))

# Create a balance plot for propensity scores
bal.plot(
  m.out,
  var.name = "distance",
  which = "both",
  colors = c("#003594", "#FFB81C")
) +
  ggtitle("Propensity Score Distribution") +
  xlab("Propensity Score")

# Display a summary of the matched data
summary(m.out)

# Create a jitter plot of the matched data
plot(m.out, type = "jitter", interactive = FALSE)


# Extract MatchIt data from m.out
matched_data <- match.data(m.out)

school_balance <- matched_data %>%
  group_by(aun) %>%
  summarise(
    n_treated = sum(treated_in_year == 1, na.rm = TRUE),
    n_control = sum(treated_in_year == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    has_both = n_treated > 0 & n_control > 0
  )

# Schools with at least 1 treated and 1 control
schools_with_both <- school_balance %>%
  filter(has_both)

# Schools missing one side
schools_missing_side <- school_balance %>%
  filter(!has_both)


library(dplyr)

matched_data_filtered <- matched_data %>%
  group_by(aun) %>%
  filter(
    sum(treated_in_year == 1, na.rm = TRUE) > 0 &
      sum(treated_in_year == 0, na.rm = TRUE) > 0
  ) %>%
  ungroup()


cohort_counts <- matched_data_filtered %>%
  group_by(year, treated_in_year) %>%
  summarise(
    n_students = n(),
    .groups = "drop"
  ) %>%
  mutate(
    treated_label = ifelse(treated_in_year == 1, "Treated", "Control")
  )
