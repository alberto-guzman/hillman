# Bring in clean outcome STATA file
outcomes <- read_dta(here("data", "clean_Hillman_2017_2025.dta"))


# Clean column names with janitor
outcomes <- outcomes %>%
   clean_names()

# Show column names of outcomes df
colnames(outcomes)


outcomes <- outcomes %>%
   rename(
      first_name = "firstname",
      last_name = "lastname"
   )

# Merge outcome data with matched_data by first name, last name and, hs_grad_year
merged_df <- matched_data %>%
   left_join(outcomes, by = c("first_name", "last_name", "hs_grad_year"))
