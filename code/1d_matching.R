# Perform matching using the 'matchit' function with multiple covariates
m.out <- matchit(
  treated_ever ~ gender + grade + age + gpa + sat_math + sat_verbal + sat_writing + psat_math + psat_verbal + psat_writing +
    act_math + act_read + act_science + act_writing + stipend + house_size + first_gen + racially_marginalized +
    bi_multi_racial + urban + suburban +
    rural + disability + neg_school + us_citizen +
    gender.NA + grade.NA + age.NA + gpa.NA + sat_math.NA + sat_verbal.NA + sat_writing.NA + psat_math.NA + psat_verbal.NA + psat_writing.NA +
    act_math.NA + act_read.NA + act_science.NA + act_writing.NA + stipend.NA + house_size.NA + first_gen.NA +
    disability.NA + neg_school.NA + us_citizen.NA,
  data = master_fill, # Data source
  method = "nearest", # Matching method
  exact = ~ year + racially_marginalized, # Exact matching variables
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
