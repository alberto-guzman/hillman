
#exact match by year

# Perform matching using the 'matchit' function with multiple covariates
m.out <- matchit(
  treated_in_year ~ gender + grade + age + gpa + psat_math + stipend + house_size + first_gen + racially_marginalized +
    bi_multi_racial + urban + suburban +
    rural + disability + neg_school + us_citizen +
    gender.NA + grade.NA + age.NA + gpa.NA + psat_math.NA +
    stipend.NA + house_size.NA + first_gen.NA + racially_marginalized.NA +
    disability.NA + neg_school.NA + us_citizen.NA,
  data = merged_df_fill, # Data source
  method = "nearest", # Matching method
  exact = ~ year, # Exact matching variables
  distance = "glm", # Distance metric
  link = "probit", # Link function for GLM
  discard = "control", # Discard unmatched control units
  caliper = .50,
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



