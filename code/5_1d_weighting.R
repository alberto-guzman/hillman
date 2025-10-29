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

merged_df_pa_covars <- merged_df_pa_covars %>%
  filter(year != 2020)


library(WeightIt)

w.out <- weightit(
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
  data = merged_df_pa_covars,
  method = "gbm",
  stabilize = TRUE,
  estimand = "ATE",
  by = "year"
)


library("cobalt")

bal.tab(w.out, un = TRUE)

plot(summary(w.out), var.name = "distance", which = "both")

#add private versus public

glm(
  enrolled_ever_stem ~ treated_in_year +
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
    us_citizen_miss +
    factor(aun),
  data = merged_df_pa_covars,
  weights = w.out$weights,
  family = "binomial"
) # or gaussian, etc.
