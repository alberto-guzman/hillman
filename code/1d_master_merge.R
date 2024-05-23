# Pivot the alum dataset to create separate treatment columns for each year
treated_years <- alum %>%
  pivot_wider(names_from = year, values_from = treatment, names_prefix = "treated_") %>%
  replace_na(list(treated_2009 = 0, treated_2010 = 0, treated_2011 = 0, treated_2012 = 0, treated_2013 = 0, treated_2014 = 0, treated_2015 = 0, treated_2016 = 0, treated_2017 = 0, treated_2018 = 0, treated_2019 = 0, treated_2020 = 0, treated_2021 = 0, treated_2022 = 0, treated_2023 = 0))

# Reorder the columns to start from treated_2009 onwards
treated_years <- treated_years %>%
  select(order(grepl("^treated_", names(.)), str_extract(names(.), "\\d{4}") %>% as.numeric()))

# Create an "ever treated" variable based on the year-specific treatment columns
treated_years <- treated_years %>%
  mutate(treated_ever = if_else((treated_2017 + treated_2018 + treated_2019 + treated_2020 + treated_2021 + treated_2022) > 0, 1, 0))

# Create a "treated before 2017" variable
treated_years <- treated_years %>%
  mutate(treated_before_2017 = if_else(rowSums(select(., matches("^treated_200[0-9]$|^treated_201[0-6]$")), na.rm = TRUE) > 0, 1, 0))

# Subset treated years
treated_years <- treated_years %>%
  select(first_name, last_name, gender, treated_ever, treated_before_2017, treated_2017:treated_2023)

# Frequency table for treated_2017 to treated_2023
treated_years %>%
  select(treated_2017:treated_2023) %>%
  gather(key = "year", value = "treated") %>%
  group_by(year, treated) %>%
  summarize(n = n()) %>%
  spread(key = "treated", value = "n")



#### This works!





# Merge the applicant and treated_years_wide datasets
merged_df <- applicants %>%
  left_join(treated_years, by = c("first_name", "last_name", "gender")) %>%
  mutate(across(starts_with("treated"), ~ ifelse(is.na(.), 0, .)))












# Clean first-gen
merged_df <- merged_df %>%
  mutate(first_gen = recode(
    first_gen,
    `1st Gen. College` = 1,
    `Do not wish to answer.` = 0,
    "Yes" = 1,
    "No" = 0
  ))

missing_plot(merged_df)
glimpse(merged_df)
skim(merged_df)










# Create binary columns for each race
merged_df <- merged_df %>%
  mutate(
    african_american = if_else(str_detect(self_identity, "African"), 1, 0),
    asian = if_else(self_identity == "Asian", 1, 0),
    hawaiian_pacific_islander = if_else(str_detect(self_identity, "Hawaiian|Pacific"), 1, 0),
    latinx = if_else(str_detect(self_identity, "Hispanic"), 1, 0),
    white = if_else(str_detect(self_identity, "Caucasian|White"), 1, 0)
  ) %>%
  mutate(
    bi_multi_racial = if_else(rowSums(select(., african_american:white)) == 0, 1, 0),
    racially_marginalized = if_else(african_american == 1 | hawaiian_pacific_islander == 1 | latinx == 1, 1, 0)
  ) %>%
  select(-self_identity)

# Create binary columns for geographic location
merged_df <- merged_df %>%
  mutate(
    urban = if_else(geographic_location == "Urban", 1, 0),
    suburban = if_else(geographic_location == "Suburban", 1, 0),
    rural = if_else(str_detect(geographic_location, "Rural/Small"), 1, 0)
  ) %>%
  select(-geographic_location)

# Recode disability as binary
merged_df$disability <- recode(merged_df$documented_disability, "Yes" = 1, "yes" = 1, "Disability" = 1, "No" = 0, "no" = 0)
merged_df <- select(merged_df, -documented_disability)

# Create binary column for negative school impact
merged_df$neg_school <- if_else(str_detect(merged_df$school_impact, "yes|Yes"), 1, 0)
merged_df <- select(merged_df, -school_impact)

# Create binary column for US citizenship
merged_df$us_citizen <- if_else(str_detect(merged_df$american_citizen, "yes|Yes"), 1, 0)
merged_df <- select(merged_df, -american_citizen)

# Recode first generation college status as binary
merged_df$first_gen <- recode(merged_df$first_gen, "Yes" = 1, "yes" = 1, "1st Gen. College" = 1, "No" = 0, "no" = 0)

# Recode stipend as binary
merged_df$stipend <- recode(merged_df$stipend, "Yes" = 1, "Stipend Eligible" = 1, "No" = 0, "Not Stipend Eligible" = 0)

# Create a 'treated_in_year' variable based on year columns
merged_df <- merged_df %>%
  mutate(
    treated_in_year = case_when(
      year == 2017 ~ treated_2017,
      year == 2018 ~ treated_2018,
      year == 2019 ~ treated_2019,
      year == 2020 ~ treated_2020,
      year == 2021 ~ treated_2021,
      year == 2022 ~ treated_2022,
      year == 2023 ~ treated_2023,
      TRUE ~ NA_real_
    )
  )

merged_df <- merged_df %>%
  arrange(first_name, last_name, gender, year) %>%
  group_by(first_name, last_name, gender) %>%
  mutate(pastTreatments = cumsum(treated_in_year) - treated_in_year) %>%
  ungroup()

# Fill missing values using fill.NAs function from optmatch package
merged_df_fill <- fill.NAs(
  treated_in_year ~ gender + grade + gpa + psat_math + stipend + house_size + first_gen + racially_marginalized +
    bi_multi_racial + urban + suburban +
    rural + disability + neg_school + us_citizen + year,
  data = merged_df
)

