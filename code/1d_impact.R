# Load necessary libraries
library(dplyr)
library(fixest)
library(purrr)
library(broom)
library(ggplot2)

# List of years in the dataset
years <- unique(merged_df$year)

# Function to estimate the treatment effect for a given year for a specified outcome
estimate_yearly_effect <- function(year, outcome_var) {
  data_year <- merged_df %>% filter(year == !!year)
  
  # Check if the dependent variable is constant
  if (length(unique(data_year[[outcome_var]])) == 1) {
    return(NULL) # Skip this year
  }
  
  model <- feols(as.formula(paste(outcome_var, "~ treated_in_year + gender + grade + gpa + psat_math + stipend + house_size + first_gen + racially_marginalized +",
                                  "bi_multi_racial + urban + suburban + rural + disability + neg_school + us_citizen +",
                                  "gender_miss + grade_miss + gpa_miss + psat_math_miss + stipend_miss + house_size_miss + first_gen_miss + racially_marginalized_miss +",
                                  "disability_miss + neg_school_miss + us_citizen_miss")),
                 data = data_year, weights = ~ weights)
  
  tidy_model <- tidy(model, conf.int = TRUE)
  tidy_model$year <- year # Add the year to the tidy model
  
  return(tidy_model)
}

# Estimate the treatment effect for each year and store the results in a list for enrolled_ever_nsc
yearly_effects_nsc <- map(years, estimate_yearly_effect, outcome_var = "enrolled_ever_nsc")

# Combine the results into a single data frame, removing NULL entries for enrolled_ever_nsc
yearly_effects_df_nsc <- bind_rows(yearly_effects_nsc)

# Filter to include only the treated_in_year coefficient for enrolled_ever_nsc
treated_in_year_effects_nsc <- yearly_effects_df_nsc %>% filter(term == "treated_in_year")

# Estimate the treatment effect for each year and store the results in a list for enrolled_ever_stem
yearly_effects_stem <- map(years, estimate_yearly_effect, outcome_var = "enrolled_ever_stem")

# Combine the results into a single data frame, removing NULL entries for enrolled_ever_stem
yearly_effects_df_stem <- bind_rows(yearly_effects_stem)

# Filter to include only the treated_in_year coefficient for enrolled_ever_stem
treated_in_year_effects_stem <- yearly_effects_df_stem %>% filter(term == "treated_in_year")

# Plot the treatment effects with confidence intervals for each year for enrolled_ever_nsc
plot_nsc <- ggplot(treated_in_year_effects_nsc, aes(x = as.factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(size = 3, color = "#003594") +
  geom_errorbar(width = 0.2, color = "#003594") +
  labs(
    x = "Year",
    y = "Estimated Treatment Effect",
    title = "Outcome: Enrolled in College") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +  # Add a horizontal line at y=0 for reference
  ylim(-2, 2)  # Set the y-axis limits from -2 to 2

# Plot the treatment effects with confidence intervals for each year for enrolled_ever_stem
plot_stem <- ggplot(treated_in_year_effects_stem, aes(x = as.factor(year), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(size = 3, color = "#E69F00") +
  geom_errorbar(width = 0.2, color = "#E69F00") +
  labs(
    x = "Year",
    y = "Estimated Treatment Effect",
    title = "Outcome: Enrollmed in STEM") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +  # Add a horizontal line at y=0 for reference
  ylim(-2, 2)  # Set the y-axis limits from -2 to 2

# Display the plots
print(plot_nsc)
print(plot_stem)

# Calculate the mean of enrolled_ever_nsc by treated_in_year and year
mean_enrolled_nsc_by_treated_year <- matched_data %>%
  group_by(year, treated_in_year) %>%
  summarize(mean_enrolled_ever_nsc = mean(enrolled_ever_stem, na.rm = TRUE)) %>%
  arrange(year, treated_in_year)

# Display the table
print(mean_enrolled_nsc_by_treated_year)

