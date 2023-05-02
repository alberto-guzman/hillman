todor::todor()

# Pivot the alum dataset to create separate treatment columns for each year
treated_years <- alum %>%
  pivot_wider(names_from = year, values_from = treatment, names_prefix = "treated_") %>%
  replace_na(list(treated_2017 = 0, treated_2018 = 0, treated_2019 = 0, treated_2020 = 0))

# Create an "ever treated" variable based on the year-specific treatment columns
treated_years <- treated_years %>%
  mutate(treated_ever = if_else((treated_2017 + treated_2018 + treated_2019 + treated_2020) > 0, 1, 0))

# Create a "treated before 2017" variable
treated_years <- treated_years %>%
  mutate(treated_before_2017 = if_else(rowSums(select(., starts_with("treated_")), na.rm = TRUE) - (treated_2017 + treated_2018 + treated_2019 + treated_2020) > 0, 1, 0))

# Merge the applicant and treated_years_wide datasets
master <- applicants %>%
  left_join(treated_years, by = c("first_name", "last_name")) %>%
  mutate(across(starts_with("treated"), ~ ifelse(is.na(.), 0, .)))

# Recode stipend as binary
# TODO Needs to capture other forms of stipend eligible
master$stipend <- recode(master$stipend, "Yes" = 1, "No" = 0)

# Create binary columns for each race
master <- master %>%
  mutate(
    african_american = if_else(str_detect(self_identity, "African"), 1, 0),
    asian = if_else(self_identity == "Asian", 1, 0),
    hawaiian_pacific_islander = if_else(str_detect(self_identity, "Hawaiian|Pacific"), 1, 0),
    latinx = if_else(str_detect(self_identity, "Hispanic"), 1, 0),
    white = if_else(str_detect(self_identity, "Caucasian|White"), 1, 0),
    bi_multi_racial = if_else(rowSums(select(., african_american:white)) == 0, 1, 0),
    racially_marginalized = if_else(african_american == 1 | hawaiian_pacific_islander == 1 | latinx == 1, 1, 0)
  ) %>%
  select(-self_identity)

# Create binary columns for geographic location
master <- master %>%
  mutate(
    urban = if_else(geographic_location == "Urban", 1, 0),
    suburban = if_else(geographic_location == "Suburban", 1, 0),
    rural = if_else(str_detect(geographic_location, "Rural|Small"), 1, 0)
  ) %>%
  select(-geographic_location)

# Recode disability as binary
master$disability <- recode(master$documented_disability, "Yes" = 1, "yes" = 1, "Disability" = 1, "No" = 0, "no" = 0)
master <- select(master, -documented_disability)

# Create binary column for negative school impact
master$neg_school <- if_else(str_detect(master$school_impact, "yes|Yes"), 1, 0)
master <- select(master, -school_impact)

# Create binary column for US citizenship
master$us_citizen <- if_else(str_detect(master$american_citizen, "yes|Yes"), 1, 0)
master <- select(master, -american_citizen)

# Recode first generation college status as binary
master$first_gen <- recode(master$first_gen, "Yes" = 1, "yes" = 1, "1st Gen. College" = 1, "No" = 0, "no" = 0)

# Fill missing values using fill.NAs function from optmatch package
master_fill <- fill.NAs(
  treatment ~ gender + grade + age + gpa + sat_math + sat_verbal + sat_writing + psat_math + psat_verbal + psat_writing +
    act_math + act_read + act_science + act_writing + stipend + house_size + first_gen + racially_marginalized +
    bi_multi_racial + urban + suburban + year +
    rural + disability + neg_school + us_citizen,
  data = master
)

# Rename variables
names(master_fill)
