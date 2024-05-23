# Load packages
library(tidyverse)
library(readxl)
library(here)
library(stringr)
library(styler)
library(lubridate)
library(naniar)
library(optmatch)
library(MatchIt)
library(cobalt)
library(finalfit)
library(skimr)
library(janitor)
library(readr)


# Set default code style for {styler} functions
grkstyle::use_grk_style()

# Set directory
here()

########################################################
######################## 2017
########################################################
# Read in data
df_2017 <- read_excel(here("data", "hillman_2017.xlsx"))

# Clean column names
df_2017 <- df_2017 %>%
  rename_with(~ str_to_lower(.) %>%
    str_replace_all(., "\\s", "_") %>%
    str_replace_all(., ":", "") %>%
    str_replace_all(., "\\?", ""))

# Select and rename columns
df_2017 <- df_2017 %>%
  select(
    -c(
      date_of_birth,
      email,
      home_phone,
      cell_phone,
      address_1,
      address_2,
      parent_guard_full_name,
      parent_guard_email,
      parent_guard_add_1,
      parent_guard_add_2,
      parent_guard_city,
      parent_guard_state,
      parent_guard_zip,
      science_math_courses,
      int_cancer_biology,
      int_tumor_immun,
      int_computer_science,
      int_drug_discovery,
      int_women_cancer,
      int_cancer_env,
      prior_research,
      jkcf_advisor,
      jkcf_start_date,
      jkcf_end_date,
      first_reference_full_name,
      first_reference_email,
      sec_reference_full_name,
      sec_reference_email,
      computer_proficiency,
      lab_experimentation,
      housing,
      marketing,
      name,
      self_agree_statement,
      ...69,
      ...70,
      green_card
    )
  ) %>%
  rename(
    high_school = high_school_name,
    grade = current_grade,
    jkcf = jkcf_young_scholar,
    house_size = household_size,
    gpa_weight = gpa_weighted
  )

# Clean and process data
df_2017 <- df_2017 %>%
  mutate(
    gender = if_else(tolower(gender) == "male", 1, 0),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1, 0),
    high_school_pub_priv = str_to_lower(high_school_pub_priv),
    jkcf = str_to_lower(jkcf),
    zip = as.integer(zip),
    grade = as.integer(grade),
    gpa = as.numeric(gpa),
    sat_math = as.integer(sat_math),
    sat_verbal = as.integer(sat_verbal),
    sat_writing = as.integer(sat_writing),
    psat_math = as.integer(psat_math),
    psat_verbal = as.integer(psat_verbal),
    psat_writing = as.integer(psat_writing),
    act_math = as.integer(act_math),
    act_read = as.integer(act_read),
    act_science = as.integer(act_science),
    act_writing = as.integer(act_writing),
    house_size = as.integer(house_size),
    year = 2017
  )

########################################################
######################## 2018
########################################################

# Read in data
df_2018 <- read_excel(here("data", "hillman_2018.xlsx"))

# Clean column names
df_2018 <- df_2018 %>%
  rename_with(~ str_to_lower(.) %>%
    str_replace_all(., "\\s", "_") %>%
    str_replace_all(., ":", "") %>%
    str_replace_all(., "\\?", ""))

# Select and rename columns
df_2018 <- df_2018 %>%
  select(
    -c(
      site,
      date_of_birth,
      wet_lab_eligible,
      alumnus,
      "absences_(y/n)",
      "absences_(#_of_days)",
      "wet_v_dry",
      "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following",
      "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following_[other]",
      "list_prior_research_experiences_(if_any),_extracurricular_activities,_honors_and_awards.",
      "list_science_and_math_courses_with_grades",
      "essay_written_or_uploaded",
      "upload_your_essay",
      "a.",
      "b.",
      "one_other_topic_of_your_choice_from_the_list_above.",
      recommender,
      do_you_have_permanent_resident_or_green_card_status,
      submission_date,
      "if_yes,_please_explain",
      email,
      home_phone,
      cell_phone,
      address,
      "1st_choice",
      "2nd_choice",
      "3rd",
      "4th",
      "5th",
      "6th",
      "stage_completion_(%)",
      "do_you_qualify_based_on_your_family_size_and_income_please_refer_to_the_chart_below_indicating_maximum_thresholds.",
      "how_do_you_identify_yourself_[other]",
      "vi._housing"
    )
  ) %>%
  rename(
    stipend = "stipend_eligible",
    self_identity = "how_do_you_identify_yourself",
    documented_disability = "do_you_have_a_documented_disabilityâ__(e.g.,_auditory,_motor,_visual,_cognitive,_other)_that_substantially_limits_one_or_more_major_life_activites_as_described_by_theâ_americans_with_disabilities_act_of_1990",
    grade = "grade_(year)",
    gpa_weight = "gpa_weighted",
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_(jkcf)_young_scholar",
    psat_math = "psat_scores_|__|_math",
    sat_math = "sat_scores_|__|_math",
    act_math = "act_scores_|__|_math",
    psat_verbal = "psat_scores_|__|_verbal",
    psat_writing = "psat_scores_|__|_writing",
    sat_verbal = "sat_scores_|__|_verbal",
    sat_writing = "sat_scores_|__|_writing",
    act_science = "act_scores_|__|_science",
    act_read = "act_scores_|__|_reading",
    act_writing = "act_scores_|__|_writing",
    act_verbal = "act_scores_|__|_verbal",
    geographic_location = "how_do_you_describe_where_you_live",
    house_size = "what_is_your_total_household_size",
    american_citizen = "are_you_an_american_citizen",
    school_impact = "do_you_believe_your_school_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research"
  )


# Clean and process columns
df_2018 <- df_2018 %>%
  mutate(
    gender = if_else(tolower(gender) == "male", 1, 0),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1, 0),
    zip = as.integer(zip),
    grade = as.integer(grade),
    gpa = as.numeric(gpa),
    sat_math = as.integer(sat_math),
    sat_verbal = as.integer(sat_verbal),
    sat_writing = as.integer(sat_writing),
    psat_math = as.integer(psat_math),
    psat_verbal = as.integer(psat_verbal),
    psat_writing = as.integer(psat_writing),
    act_math = as.integer(act_math),
    act_read = as.integer(act_read),
    act_science = as.integer(act_science),
    act_writing = as.integer(act_writing),
    house_size = as.integer(house_size),
    year = 2018
  )

######################################################## 3
######################## 2019
########################################################

# Read in data
df_2019 <- read_excel(here("data", "hillman_2019.xlsx"), sheet = "All (Extra info)")

# Clean column names
df_2019 <- df_2019 %>%
  rename_with(~ str_to_lower(.) %>%
    str_replace_all(., "\\s", "_") %>%
    str_replace_all(., ":", "") %>%
    str_replace_all(., "\\?", ""))

# Select and rename columns
df_2019 <- select(df_2019, -c(
  date_of_birth,
  email,
  home_phone,
  cell_phone,
  address...10,
  "parent_or_legal_guardian's_full_name...14",
  "parent_or_legal_guardian's_email...15",
  "housing",
  "1st_choice",
  "2nd",
  "3rd",
  "4th",
  "5th",
  "6th",
  expected_absences,
  "#_of_absences",
  wet_or_dry_lab_preferred,
  "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following",
  "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following_[other]",
  "parent_or_legal_guardian's_full_name...40",
  "parent_or_legal_guardian's_email...41",
  "upload_your_essay...50",
  "a.",
  "b.",
  "upload_transcript",
  "would_you_like_to_upload_your_essay_or_use_the_space_provided",
  "recommender",
  "submission_date",
  "stage_completion_(%)",
  "possible_2_year_program__are_you_interested_in_a_new_two-year_program_through_the_hillman_academy_the_hillman_academy_scholars_program_provides_a_paid_($3600/year),_comprehensive,_two-year_experience_focused_on_researc...",
  "do_you_have_permanent_resident_or_green_card_status",
  "do_you_have_a_documented_disabilityâ__(e.g.,_auditory,_motor,_visual,_cognitive,_other)_that_substantially_limits_one_or_more_major_life_activites_as_described_by_theâ_americans_with_disabilities_act_of_1990_[other]",
  "if_yes,_please_explain",
  "how_do_you_identify_yourself_[caucasian/_white]",
  "how_do_you_identify_yourself_[african/_african_american/_black]",
  "how_do_you_identify_yourself_[asian]",
  "how_do_you_identify_yourself_[hawaiian_or_pacific_islander]",
  "how_do_you_identify_yourself_[native_american/_alaska_native]",
  "how_do_you_identify_yourself_[hispanic/_latino]",
  "how_do_you_identify_yourself_[do_not_wish_to_answer]",
  "upload_your_essay...79":"applicants_-_applicant's_statement_and_signature",
  "address...36",
  "city...37",
  "state...38",
  "zip...39",
  group,
  "prior_research_experiences.",
  list_science_and_math_courses_with_grades,
  "stipend_eligible"
))

# Rename columns
df_2019 <- df_2019 %>%
  rename(
    high_school = "school",
    city = "city...11",
    state = "state...12",
    zip = "zip...13",
    jkcf = "jack_kent_cooke",
    gpa_weight = "gpa_weighted_yes/no",
    grade = "current_grade",
    self_identity = "race",
    documented_disability = "do_you_have_a_documented_disabilityâ__(e.g.,_auditory,_motor,_visual,_cognitive,_other)_that_substantially_limits_one_or_more_major_life_activites_as_described_by_theâ_americans_with_disabilities_act_of_1990",
    psat_math = "psat_scores_|__|_math",
    sat_math = "sat_scores_|__|_math",
    act_math = "act_scores_|__|_math",
    psat_verbal = "psat_scores_|__|_verbal",
    psat_writing = "psat_scores_|__|_writing",
    sat_verbal = "sat_scores_|__|_verbal",
    sat_writing = "sat_scores_|__|_writing",
    act_science = "act_scores_|__|_science",
    act_read = "act_scores_|__|_reading",
    act_writing = "act_scores_|__|_writing",
    act_verbal = "act_scores_|__|_verbal",
    american_citizen = "are_you_an_american_citizen",
    first_gen = "will_you_be_the_first_person_in_your_family_to_attend_college",
    school_impact = "do_you_believe_your_school_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research",
    geographic_location = "how_do_you_describe_where_you_live",
    house_size = "household_size"
  )

# Clean and process columns
df_2019 <- df_2019 %>%
  mutate(
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = as.integer(tolower(gpa_weight) == "yes"),
    zip = as.integer(zip),
    grade = as.integer(grade),
    gpa = as.numeric(gpa),
    sat_math = as.integer(sat_math),
    sat_verbal = as.integer(sat_verbal),
    sat_writing = as.integer(sat_writing),
    psat_math = as.integer(psat_math),
    psat_verbal = as.integer(psat_verbal),
    psat_writing = as.integer(psat_writing),
    act_math = as.integer(act_math),
    act_read = as.integer(act_read),
    act_science = as.integer(act_science),
    act_writing = as.integer(act_writing),
    house_size = as.integer(house_size),
    year = 2019
  )


df_stipend <- read_excel(here("data", "hillman_raw.xlsx"), sheet = "2019") %>%
  select(first_name, last_name, stipend)

df_2019 <- df_2019 %>%
  left_join(df_stipend)

rm(df_stipend)

########################################################
######################## 2020
########################################################

# Read in data
df_2020 <- read_excel(here("data", "hillman_2020.xlsx"))

# Clean column names
df_2020 <- df_2020 %>%
  rename_with(~ str_to_lower(.) %>%
    str_replace_all(., "\\s", "_") %>%
    str_replace_all(., ":", "") %>%
    str_replace_all(., "\\?", ""))

# Select and rename columns
df_2020 <- select(
  df_2020,
  c(
    first,
    last,
    gender,
    current_grade...5,
    alumni,
    race...15,
    school,
    city,
    state,
    zip,
    "are_you_a_current_jack_kent_cooke_foundation_(jkcf)_young_scholar",
    gpa,
    "weighted_(y/n)",
    "psat_scores_|__|_math":"act_scores_|__|_science",
    "low_income",
    "disability",
    "1st_gen_college",
    "what_is_your_total_household_size",
    "how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below,_if_you_meet_at_least_two_of_the_following_criteria_you_w...",
    "are_you_an_american_citizen",
    "stipend_eligible...77"
  )
)

# Rename columns
df_2020 <- df_2020 %>%
  rename(
    first_name = "first",
    last_name = "last",
    grade = "current_grade...5",
    self_identity = "race...15",
    high_school = school,
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_(jkcf)_young_scholar",
    gpa_weight = "weighted_(y/n)",
    psat_math = "psat_scores_|__|_math",
    psat_reading_writing = "psat_scores_|__|_reading_and_writing",
    sat_math = "sat_scores_|__|_math",
    sat_reading_writing = "sat_scores_|__|_reading_and_writing",
    act_math = "act_scores_|__|_math",
    act_verbal = "act_scores_|__|_verbal",
    act_writing = "act_scores_|__|_writing",
    act_read = "act_scores_|__|_reading",
    act_science = "act_scores_|__|_science",
    documented_disability = disability,
    first_gen = "1st_gen_college",
    house_size = "what_is_your_total_household_size",
    geographic_location = "how_do_you_describe_where_you_live",
    school_impact = "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below,_if_you_meet_at_least_two_of_the_following_criteria_you_w...",
    american_citizen = "are_you_an_american_citizen",
    stipend = "stipend_eligible...77"
  )

# Process data
df_2020 <- df_2020 %>%
  mutate(
    gender = tolower(gender),
    gender = if_else(gender == "male", 1, 0),
    gpa_weight = tolower(gpa_weight),
    gpa_weight = if_else(gpa_weight == "yes", 1, 0),
    zip = as.integer(zip),
    grade = as.integer(grade),
    gpa = as.numeric(gpa),
    sat_math = as.integer(sat_math),
    sat_reading_writing = as.integer(sat_reading_writing),
    psat_math = as.integer(psat_math),
    psat_reading_writing = as.integer(psat_reading_writing),
    act_math = as.integer(act_math),
    act_read = as.integer(act_read),
    act_science = as.integer(act_science),
    act_writing = as.integer(act_writing),
    house_size = as.integer(house_size)
  )

# Add year column
df_2020$year <- 2020

########################################################
######################## 2021
########################################################

# Read in data
df_2021 <- read_excel(here("data", "hillman_2021.xlsx"))

# Clean column names
df_2021 <- clean_names(df_2021)

# Select and rename columns
df_2021 <- select(
  df_2021,
  c(
    first_1,
    last_2,
    gender,
    date_of_birth,
    grades_current_grade_6,
    race,
    "school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    city,
    state,
    zip,
    "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    grades_gpa,
    grades_is_this_gpa_weighted,
    "psat_scores_math":"act_scores_science",
    "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    "will_you_be_the_first_person_in_your_family_to_attend_college",
    "how_many_people_live_in_your_home",
    "how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    "are_you_an_american_citizen",
    "stipend_eligible"
  )
)

# Rename columns
df_2021 <- df_2021 %>%
  rename(
    first_name = "first_1",
    last_name = "last_2",
    grade = "grades_current_grade_6",
    self_identity = "race",
    gpa = "grades_gpa",
    high_school = "school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    gpa_weight = "grades_is_this_gpa_weighted",
    psat_math = "psat_scores_math",
    psat_reading_writing = "psat_scores_reading_and_writing",
    sat_math = "sat_scores_math",
    sat_reading_writing = "sat_scores_reading_and_writing",
    act_math = "act_scores_math",
    act_verbal = "act_scores_verbal",
    act_writing = "act_scores_writing",
    act_read = "act_scores_reading",
    act_science = "act_scores_science",
    documented_disability = "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    first_gen = "will_you_be_the_first_person_in_your_family_to_attend_college",
    house_size = "how_many_people_live_in_your_home",
    geographic_location = "how_do_you_describe_where_you_live",
    school_impact = "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    american_citizen = "are_you_an_american_citizen",
    stipend = "stipend_eligible"
  )

# Process data
df_2021 <- df_2021 %>%
  mutate(
    gender = case_when(
      tolower(gender) == "male" ~ 1,
      tolower(gender) == "female" ~ 0,
      TRUE ~ NA_real_  # Assigns NA to all other cases
    ),
    gpa_weight = case_when(
      tolower(gpa_weight) == "yes" ~ 1,
      tolower(gpa_weight) == "no" ~ 0,
      TRUE ~ NA_real_  # Assigns NA to all other cases
    ),
    zip = as.integer(zip),
    grade = as.integer(grade),
    gpa = as.numeric(gpa),
    sat_math = as.integer(sat_math),
    sat_reading_writing = as.integer(sat_reading_writing),
    psat_math = as.integer(psat_math),
    psat_reading_writing = as.integer(psat_reading_writing),
    act_math = as.integer(act_math),
    act_read = as.integer(act_read),
    act_science = as.integer(act_science),
    act_writing = as.integer(act_writing),
    house_size = as.integer(house_size)
  )

# Add year column
df_2021$year <- 2021



########################################################
######################## 2022
########################################################

# Read in data
df_2022 <- read_excel(here("data", "hillman_2022.xlsx"))

# Clean column names
df_2022 <- clean_names(df_2022)

# Select and rename columns
df_2022 <- select(
  df_2022,
  c(
    applicant_first_name,
    applicant_last_name,
    personal_information_gender,
    #personal_information_date_of_birth,
    grades_current_grade,
    what_race_s_do_you_identify_with,
    "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    personal_information_city,
    personal_information_state,
    personal_information_zip,
    "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    grades_gpa,
    grades_is_this_gpa_weighted,
    "psat_scores_math":"act_scores_science",
    "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    "will_you_be_the_first_person_in_your_family_to_attend_college",
    "how_many_people_live_in_your_home",
    "how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    "are_you_an_american_citizen",
    "stipend_eligible_y_n"
  )
)

# Rename columns
df_2022 <- df_2022 %>%
  rename(
    first_name = "applicant_first_name",
    last_name = "applicant_last_name",
    grade = "grades_current_grade",
    #date_of_birth = "personal_information_date_of_birth",
    city = "personal_information_city",
    state = "personal_information_state",
    zip = "personal_information_zip", 
    gender = "personal_information_gender",
    self_identity = "what_race_s_do_you_identify_with",
    gpa = "grades_gpa",
    high_school = "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    gpa_weight = "grades_is_this_gpa_weighted",
    psat_math = "psat_scores_math",
    psat_reading_writing = "psat_scores_reading_and_writing",
    sat_math = "sat_scores_math",
    sat_reading_writing = "sat_scores_reading_and_writing",
    act_math = "act_scores_math",
    act_verbal = "act_scores_verbal",
    act_writing = "act_scores_writing",
    act_read = "act_scores_reading",
    act_science = "act_scores_science",
    documented_disability = "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    first_gen = "will_you_be_the_first_person_in_your_family_to_attend_college",
    house_size = "how_many_people_live_in_your_home",
    geographic_location = "how_do_you_describe_where_you_live",
    school_impact = "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    american_citizen = "are_you_an_american_citizen",
    stipend = "stipend_eligible_y_n"
  )

# Process data
df_2022 <- df_2022 %>%
  mutate(
    gender = case_when(
      tolower(gender) == "male" ~ 1,
      tolower(gender) == "female" ~ 0,
      TRUE ~ NA_real_  # Assigns NA to all other cases
    ),
    gpa_weight = case_when(
      tolower(gpa_weight) == "yes" ~ 1,
      tolower(gpa_weight) == "no" ~ 0,
      TRUE ~ NA_real_  # Assigns NA to all other cases
    ),
    zip = as.integer(zip),
    grade = as.integer(grade),
    gpa = as.numeric(gpa),
    sat_math = as.integer(sat_math),
    sat_reading_writing = as.integer(sat_reading_writing),
    psat_math = as.integer(psat_math),
    psat_reading_writing = as.integer(psat_reading_writing),
    act_math = as.integer(act_math),
    act_read = as.integer(act_read),
    act_science = as.integer(act_science),
    act_writing = as.integer(act_writing),
    house_size = as.integer(house_size)
  )

# Add year column
df_2022$year <- 2022

########################################################
######################## 2023
########################################################

# Read in data
df_2023 <- read_csv(here("data","hillman_2023.csv"))

# Clean column names
df_2023 <- clean_names(df_2023)

# Select and rename columns
df_2023 <- select(
  df_2023,
  c(
    applicant_first_name,
    applicant_last_name,
    personal_information_gender,
    #personal_information_date_of_birth,
    grades_current_grade,
    what_race_s_do_you_identify_with,
    "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    personal_information_city,
    personal_information_state,
    personal_information_zip,
    "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    grades_gpa,
    grades_is_this_gpa_weighted,
    "psat_scores_math":"act_scores_science",
    "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    "did_any_of_your_parents_guardians_attend_college",
    "how_many_people_live_in_your_home",
    "how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    "are_you_an_american_citizen",
   # "stipend_eligible_y_n",
    "application_form_hillman_academy_completion_status"
  )
)

# Rename columns
df_2023 <- df_2023 %>%
  rename(
    first_name = "applicant_first_name",
    last_name = "applicant_last_name",
    grade = "grades_current_grade",
    #date_of_birth = "personal_information_date_of_birth",
    city = "personal_information_city",
    state = "personal_information_state",
    zip = "personal_information_zip", 
    gender = "personal_information_gender",
    self_identity = "what_race_s_do_you_identify_with",
    gpa = "grades_gpa",
    high_school = "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    gpa_weight = "grades_is_this_gpa_weighted",
    psat_math = "psat_scores_math",
    psat_reading_writing = "psat_scores_reading_and_writing",
    sat_math = "sat_scores_math",
    sat_reading_writing = "sat_scores_reading_and_writing",
    act_math = "act_scores_math",
    act_writing = "act_scores_writing",
    act_read = "act_scores_reading",
    act_science = "act_scores_science",
    documented_disability = "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    first_gen = "did_any_of_your_parents_guardians_attend_college",
    house_size = "how_many_people_live_in_your_home",
    geographic_location = "how_do_you_describe_where_you_live",
    school_impact = "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    american_citizen = "are_you_an_american_citizen"
   # stipend = "stipend_eligible_y_n"
  )


# Subset to Completion status
df_2023 <- df_2023 |>
  filter(application_form_hillman_academy_completion_status == "Completed")






# Process data
df_2023 <- df_2023 %>%
  mutate(
    gender = case_when(
      tolower(gender) == "male" ~ 1,
      tolower(gender) == "female" ~ 0,
      TRUE ~ NA_real_  # Assigns NA to all other cases
    ),
    gpa_weight = case_when(
      tolower(gpa_weight) == "yes" ~ 1,
      tolower(gpa_weight) == "no" ~ 0,
      TRUE ~ NA_real_  # Assigns NA to all other cases
    ),
    zip = as.integer(zip),
    grade = as.integer(grade),
    gpa = as.numeric(gpa),
    sat_math = as.integer(sat_math),
    sat_reading_writing = as.integer(sat_reading_writing),
    psat_math = as.integer(psat_math),
    psat_reading_writing = as.integer(psat_reading_writing),
    act_math = as.integer(act_math),
    act_read = as.integer(act_read),
    act_science = as.integer(act_science),
    act_writing = as.integer(act_writing),
    house_size = as.integer(house_size)
  )

# Add year column
df_2023$year <- 2023




# Read in stipend eligible .txt file from David - Dec 2023 updated
df2023_stipend <- read_delim("data/2023_applicants_stipendEligible.txt", 
                                               delim = "\t", escape_double = FALSE, 
                                               trim_ws = TRUE)
# Use clean names on df2023_stipend df
df2023_stipend <- clean_names(df2023_stipend)

# Rename columns
df2023_stipend <- df2023_stipend %>%
  rename(stipend = "stipend_eligible",
         first_name = "applicant_first_name",
         last_name = "applicant_last_name")

# Merge stipend eligible
df_2023 <- df_2023 %>%
  left_join(df2023_stipend, relationship = "many-to-many")

rm(df2023_stipend)

########################################################
######################## Merge Code
########################################################

# 1. Append all dataframes
df <- bind_rows(df_2017, df_2018, df_2019, df_2020, df_2021, df_2022, df_2023)

# 2. Read and merge date of birth (dob) data
# dob_df <- read_excel(
#   here("data", "hillman_raw.xlsx"),
#   col_types = c("text", "text", "date", "numeric")
# )
# 
# dob_df <- rename(dob_df, year = "hillman_year")
# df <- left_join(df, dob_df)

# 3. Clean the dataframe
df <- df %>% select(-c(
  high_school_pub_priv,
  #date_of_birth,
  school_district,
  reduced_lunch,
  offer_ap,
  decision,
  `income_eligible_(reduced_lunch)`,
  alumni,
  act_verbal,
  low_income,
  middle_name
))

# 5. Clean first and last names
df <- df %>% mutate(
  first_name = tolower(first_name),
  last_name = tolower(last_name)
)

df$first_name <- gsub("\\(.*", "", df$first_name)
df$first_name <- gsub('\\".*', "", df$first_name)

# 6. Calculate age relative to the year they applied
# df$year_dt <- ymd(df$year, truncated = 2L)
# df$age <- trunc((df$dob %--% df$year_dt) / years(1))

# 7. Fix GPA values
df <- df %>% mutate(gpa = case_when(
  gpa > 96 ~ 4.0,
  between(gpa, 90, 96) ~ 3.7,
  TRUE ~ as.numeric(gpa)
))

# 8. Fix SAT, PSAT, and ACT scores by replacing 0 with NA
test_cols <- colnames(df)[startsWith(colnames(df), "sat_") | startsWith(colnames(df), "psat_") | startsWith(colnames(df), "act_")]
df[test_cols] <- lapply(df[test_cols], function(x) ifelse(x == 0, NA, x))

# 9. Fix house size
df$house_size[df$house_size > 11] <- NA

# 10. Fix age values
# df$age[df$age == 0] <- NA
# df$age[df$age == 7] <- NA
# 
# bring back DOB

# 11. Identify duplicate records
df %>%
  group_by(first_name, last_name, year) %>%
  filter(n() > 1) %>%
  summarize(n = n())

# 13. DOUBLE CHECK THESE WITH DAVID
df <- df[!(df$first_name == "amanda" & df$last_name == "lu"), ]
df <- df[!(df$first_name == "angela" & df$last_name == "tao"), ]
df <- df[!(df$first_name == "arnav" & df$last_name == "patel"), ]
df <- df[!(df$first_name == "charles" & df$last_name == "mawhinney"), ]
df <- df[!(df$first_name == "daniel" & df$last_name == "wang"), ]
df <- df[!(df$first_name == "grace" & df$last_name == "wang"), ]
df <- df[!(df$first_name == "imani" & df$last_name == "smith"), ]
df <- df[!(df$first_name == "sanyah" & df$last_name == "nabi"), ]



# 14. Subset to final dataframe
df <- df %>% select(
  first_name,
  last_name,
  gender,
  high_school,
  city,
  state,
  zip,
  grade,
  gpa,
  psat_math,
  jkcf,
  stipend,
  house_size,
  self_identity,
  geographic_location,
  documented_disability,
  school_impact,
  american_citizen,
  year,
  first_gen
)

# Final dataframe
applicants <- df
rm(list = setdiff(ls(), "applicants"))

###
missing_plot(applicants)
glimpse(applicants)
skim(applicants)

# Count of unique applicants by year
applicants %>%
  group_by(first_name, last_name, year) %>%
  summarise(n = n()) %>%
  group_by(year) %>%
  summarise(total_unique_students = sum(n))


# preparing file for danielle

# subset applicants file to only include first_name, last_name, grade, year
applicants_danielle <- applicants %>%
  select(first_name, last_name, grade, year)

# create a new variable called hs_grad_year which gives me the year a student will graduate high based on the grade they were in when they applied
applicants_danielle <- applicants_danielle %>%
  mutate(hs_grad_year = year + (12 - grade))

# write out csv thats called hillman_hs_grad_year.csv
write_csv(applicants_danielle, here("data", "hillman_hs_grad_year.csv"))
