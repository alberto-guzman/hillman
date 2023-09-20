# Load the data
alum <- read_csv(
  here("data", "Alumni Tracker SL 5.9.2022.csv"),
  col_types = cols(
    `Source (Survey, LinkedIn, NSC Etc.)` = col_skip(),
    `Last check` = col_skip(),
    Email = col_skip(),
    `Project title` = col_skip(),
    `Public Profile` = col_skip(),
    Site = col_skip(),
    PI = col_skip(),
    `PI Email` = col_skip(),
    `Parent Name` = col_skip(),
    `Parent Email` = col_skip(),
    Phone = col_skip(),
    Profession = col_skip(),
    `Publication link` = col_skip()
  )
)

# Clean column names
alum <- alum %>%
  rename_with(~ str_to_lower(.) %>%
    str_replace_all(., "\\s", "_") %>%
    str_replace_all(., ":", "") %>%
    str_replace_all(., "\\?", ""))

# Clean first and last names
alum <- alum %>%
  mutate(
    first_name = tolower(gsub("\\(.*", "", first)),
    last_name = tolower(gsub("\\(.*", "", last))
  ) %>%
  select(-first, -last)

# Clean gender
alum <- alum %>%
  mutate(gender = recode(
    gender,
    `M` = 1,
    `F` = 0,
    "Male" = 1,
    "Female" = 0
  ))

# Create max and min participation year
alum <- alum %>%
  mutate(
    year_max = pmax(year1, year2, year3, year4, na.rm = TRUE),
    year_min = pmin(year1, year2, year3, year4, na.rm = TRUE)
  )


# Remove specific duplicate rows
alum <- alum[!(alum$first_name == "amanda" & alum$last_name == "lu"), ]

# Pivot to long format
alum_long <- alum %>%
  select(first_name, last_name, year1:year4) %>%
  pivot_longer(
    cols = starts_with("year"),
    names_to = "year"
  ) %>%
  select(first_name, last_name, value) %>%
  rename(year = value) %>%
  drop_na()

# Get year_max and year_min data for each person
alum_cov <- alum %>%
  select(first_name, last_name, year_max, year_min, gender) %>%
  distinct() # This ensures one row per person

# Join back year_max and year_min to the pivoted data
alum <- alum_long %>%
  left_join(alum_cov, by = c("first_name", "last_name"))

# Add treatment column
alum$treatment <- 1
rm(alum_cov, alum_long)

# Count of unique participants by year
alum %>%
  group_by(first_name, last_name, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(total_unique_students = sum(n))
