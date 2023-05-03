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

# High school graduation
alum$high_school_graduation_expected <- gsub("\\(.*", "", alum$high_school_graduation_expected)

# Filter unique records
alum <- alum %>%
  group_by(first_name, last_name) %>%
  filter(n() == 1) %>%
  ungroup()

# Pivot to long format
alum <- alum %>%
  select(first_name, last_name, year1:year4) %>%
  pivot_longer(
    cols = starts_with("year"),
    names_to = "year"
  ) %>%
  select(first_name, last_name, value) %>%
  rename(year = value) %>%
  drop_na()

# Add treatment column
alum$treatment <- 1
