

# List of columns to check and fill missing values
cols_to_fill <- c("gender", "grade", "gpa", "psat_math", "stipend", "house_size", 
                  "first_gen", "racially_marginalized", "bi_multi_racial", "urban", 
                  "suburban", "rural", "disability", "neg_school", "us_citizen")

# Function to create individual missing indicators and fill missing values
merged_df <- merged_df %>%
  mutate(across(all_of(cols_to_fill), 
                list(miss = ~ if_else(is.na(.), 1, 0)), 
                .names = "{col}_miss")) %>%
  mutate(across(all_of(cols_to_fill), ~ replace_na(., 0)))

# Remove all rows that have treated_in_year missing
merged_df <- merged_df[!is.na(merged_df$treated_in_year),]

#exact match by year
# Perform matching using the 'matchit' function with multiple covariates
m.out <- matchit(
  treated_in_year ~ gender + grade + gpa + psat_math + stipend + house_size + first_gen + racially_marginalized +
    bi_multi_racial + urban + suburban +
    rural + disability + neg_school + us_citizen +
    gender_miss + grade_miss + gpa_miss + psat_math_miss +
    stipend_miss + house_size_miss + first_gen_miss + racially_marginalized_miss +
    disability_miss + neg_school_miss + us_citizen_miss,
  data = merged_df, # Data source
  method = "nearest", # Matching method
  exact = ~ year, # Exact matching variables
  distance = "glm", # Distance metric
  caliper = .25,
  std.caliper = TRUE, # Caliper settings
  replace = FALSE # Allow replacement in matching
)

# Plot a summary of the matched data
plot(summary(m.out))

# Create a balance plot for propensity scores
bal.plot(m.out, var.name = "distance", which = "both", colors = c("#003594", "#FFB81C")) +
  ggtitle("Propensity Score Distribution") + xlab("Propensity Score")

# Display a summary of the matched data
summary(m.out)

# Create a jitter plot of the matched data
plot(m.out, type = "jitter", interactive = FALSE)


# Extract MatchIt data from m.out
matched_data <- match.data(m.out)




library(gtsummary)
library(dplyr)
library(purrr)
library(glue)

# Extract matched data
matched_data <- match.data(m.out)

# Define covariates
all_covariates <- c("gender", "grade", "gpa", "psat_math", "stipend", 
                "house_size", "first_gen", "racially_marginalized", 
                "bi_multi_racial", "urban", "suburban", "rural", 
                "disability", "neg_school", "us_citizen")


# Create grouping variable
matched_data <- matched_data %>%
  mutate(treatment_group = if_else(treated_in_year == 1, "Treated", "Control"))

# Force numeric to avoid surprises
matched_data <- matched_data %>%
  mutate(across(all_of(all_covariates), as.numeric))

# Build type list: force ALL variables as continuous
type_list <- all_covariates %>%
  setNames(., .) %>%
  map(~ "continuous")

# -----------------------------------------------
# 1. Descriptive table overall
tbl_overall <- matched_data %>%
  select(treatment_group, all_of(all_covariates)) %>%
  tbl_summary(
    by = treatment_group,
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = type_list,
    digits = all_continuous() ~ 2,          # ✅ two decimal places
    missing = "no"
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Covariate**") %>%
  modify_caption("**Table 1. Descriptive statistics for matched sample (All years)**")

# -----------------------------------------------
# 2. Descriptive tables by year
tbl_by_year <- matched_data %>%
  select(year, treatment_group, all_of(all_covariates)) %>%
  group_split(year) %>%
  map(~ .x %>%
        tbl_summary(
          by = treatment_group,
          statistic = all_continuous() ~ "{mean} ({sd})",
          type = type_list,
          digits = all_continuous() ~ 2,    # ✅ two decimal places
          missing = "no"
        ) %>%
        bold_labels() %>%
        modify_caption(glue("**Descriptive statistics for Year {unique(.x$year)}**"))
  )

# -----------------------------------------------
# View tables
tbl_overall
tbl_by_year




library(gt)
library(gtsummary)
library(dplyr)
library(glue)

# Save overall table
as_gt(tbl_overall) %>%
  gtsave(filename = "Descriptive_Table_All_Years.html")

# Get list of unique years
years <- sort(unique(matched_data$year))

# Loop over years, filter, create table, and save
for (yr in years) {
  temp_data <- matched_data %>%
    filter(year == yr) %>%
    select(treatment_group, all_of(all_covariates))
  
  tbl <- tbl_summary(
    data = temp_data,
    by = treatment_group,
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_continuous() ~ "continuous",
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
    bold_labels() %>%
    modify_caption(glue("**Descriptive statistics for Year {yr}**"))
  
  as_gt(tbl) %>%
    gtsave(filename = glue("Descriptive_Table_{yr}.html"))
}



