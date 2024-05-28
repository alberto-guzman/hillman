

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
  discard = "control", # Discard unmatched control units
  caliper = .45,
  std.caliper = TRUE, # Caliper settings
  replace = TRUE # Allow replacement in matching
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

