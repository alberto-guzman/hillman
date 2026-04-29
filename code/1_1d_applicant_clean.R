# =============================================================================
# 1_1d_applicant_clean.R
#
# Purpose: Load and clean Hillman Summer program applicant data across cohort
#          years, standardize variables, and produce one analysis-ready dataset.
#
# Input:   data/raw/applicants/ — year-specific Excel/CSV applicant files (2017–2023)
# Output:  `applicants`  — cleaned long-format applicant data frame
#          `applicant_n` — raw applicant counts by year
#          output/counts/n_applicants_by_year.csv
# =============================================================================

library(tidyverse)
library(readxl)
library(here)
library(janitor)

source(here("code", "helpers.R"))

dir.create(here("output", "counts"), recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# HELPER FUNCTION
# =============================================================================

clean_custom_names <- function(df) {
  df |>
    rename_with(
      ~ str_to_lower(.) |>
        str_replace_all("\\s", "_") |>
        str_replace_all(":", "") |>
        str_replace_all("\\?", "")
    )
}

# =============================================================================
# LOAD AND CLEAN YEAR-BY-YEAR FILES
# =============================================================================

# Note: 2017-2022 source files contain only submitted/complete records.
# 2023 is exported from a system that includes incomplete records, so a
# completion status filter is applied there explicitly.

# --- 2017 --------------------------------------------------------------------
df_2017 <- read_excel(here("data", "raw", "applicants", "hillman_2017.xlsx")) |>
  clean_custom_names() |>
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
  ) |>
  rename(
    high_school = high_school_name,
    grade = current_grade,
    jkcf = jkcf_young_scholar,
    house_size = household_size,
    gpa_weight = gpa_weighted
  ) |>
  mutate(
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1L, 0L),
    high_school_pub_priv = str_to_lower(high_school_pub_priv),
    jkcf = str_to_lower(jkcf),
    zip = as.character(zip),
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

# --- 2018 --------------------------------------------------------------------
df_2018 <- read_excel(here("data", "raw", "applicants", "hillman_2018.xlsx")) |>
  clean_custom_names() |>
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
  ) |>
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
    geographic_location = "how_do_you_describe_where_you_live",
    house_size = "what_is_your_total_household_size",
    american_citizen = "are_you_an_american_citizen",
    school_impact = "do_you_believe_your_school_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research"
  ) |>
  mutate(
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1L, 0L),
    zip = as.character(zip),
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

# --- 2019 --------------------------------------------------------------------
df_2019 <- read_excel(
  here("data", "raw", "applicants", "hillman_2019.xlsx"),
  sheet = "All (Extra info)"
) |>
  clean_custom_names() |>
  select(
    -c(
      date_of_birth,
      email,
      home_phone,
      cell_phone,
      address...10,
      "parent_or_legal_guardian's_full_name...14",
      "parent_or_legal_guardian's_email...15",
      housing,
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
      recommender,
      submission_date,
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
      "upload_your_essay...79",
      "address...36",
      "city...37",
      "state...38",
      "zip...39",
      group,
      "prior_research_experiences.",
      list_science_and_math_courses_with_grades,
      "stipend_eligible"
    )
  ) |>
  rename(
    high_school = school,
    city = "city...11",
    state = "state...12",
    zip = "zip...13",
    jkcf = jack_kent_cooke,
    gpa_weight = "gpa_weighted_yes/no",
    grade = current_grade,
    self_identity = race,
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
    house_size = household_size
  ) |>
  mutate(
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1L, 0L),
    zip = as.character(zip),
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

# Stipend data for 2019 is in a separate file
df_stipend <- read_excel(here("data", "raw", "applicants", "hillman_raw.xlsx"), sheet = "2019") |>
  select(first_name, last_name, stipend)

df_2019 <- df_2019 |>
  left_join(df_stipend, by = c("first_name", "last_name"))
rm(df_stipend)

# --- 2020 --------------------------------------------------------------------
# Loaded for completeness; excluded from analysis in script 3b due to COVID disruption.
df_2020 <- read_excel(here("data", "raw", "applicants", "hillman_2020.xlsx")) |>
  clean_custom_names() |>
  select(
    first,
    last,
    gender,
    `current_grade...5`,
    alumni,
    `race...15`,
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
  ) |>
  rename(
    first_name = first,
    last_name = last,
    grade = `current_grade...5`,
    self_identity = `race...15`,
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
  ) |>
  mutate(
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1L, 0L),
    zip = as.character(zip),
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
  ) |>
  mutate(year = 2020)

# --- 2021 --------------------------------------------------------------------
df_2021 <- read_excel(here("data", "raw", "applicants", "hillman_2021.xlsx")) |>
  clean_names() |>
  # 2021 source file contains exact duplicate rows for every applicant — remove
  distinct() |>
  select(
    first_1,
    last_2,
    gender,
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
  ) |>
  rename(
    first_name = first_1,
    last_name = last_2,
    grade = grades_current_grade_6,
    self_identity = race,
    gpa = grades_gpa,
    high_school = "school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    gpa_weight = grades_is_this_gpa_weighted,
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
    stipend = stipend_eligible
  ) |>
  mutate(
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1L, 0L),
    zip = as.character(zip),
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
  ) |>
  mutate(year = 2021)

# --- 2022 --------------------------------------------------------------------
df_2022 <- read_excel(here("data", "raw", "applicants", "hillman_2022.xlsx")) |>
  clean_names() |>
  select(
    personal_information_first_name,
    personal_information_last_name,
    personal_information_gender,
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
  ) |>
  rename(
    first_name = personal_information_first_name,
    last_name = personal_information_last_name,
    grade = grades_current_grade,
    city = personal_information_city,
    state = personal_information_state,
    zip = personal_information_zip,
    gender = personal_information_gender,
    self_identity = what_race_s_do_you_identify_with,
    gpa = grades_gpa,
    high_school = "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    gpa_weight = grades_is_this_gpa_weighted,
    psat_math = psat_scores_math,
    psat_reading_writing = psat_scores_reading_and_writing,
    sat_math = sat_scores_math,
    sat_reading_writing = sat_scores_reading_and_writing,
    act_math = act_scores_math,
    act_verbal = act_scores_verbal,
    act_writing = act_scores_writing,
    act_read = act_scores_reading,
    act_science = act_scores_science,
    documented_disability = "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    first_gen = "will_you_be_the_first_person_in_your_family_to_attend_college",
    house_size = "how_many_people_live_in_your_home",
    geographic_location = "how_do_you_describe_where_you_live",
    school_impact = "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    american_citizen = "are_you_an_american_citizen",
    stipend = "stipend_eligible_y_n"
  ) |>
  mutate(
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1L, 0L),
    zip = as.character(zip),
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
  ) |>
  mutate(year = 2022)

# --- 2023 --------------------------------------------------------------------
df_2023 <- read_csv(here("data", "raw", "applicants", "hillman_2023.csv")) |>
  clean_names() |>
  select(
    personal_information_first_name,
    personal_information_last_name,
    personal_information_gender,
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
    "application_form_hillman_academy_completion_status"
  ) |>
  rename(
    first_name = personal_information_first_name,
    last_name = personal_information_last_name,
    grade = grades_current_grade,
    city = personal_information_city,
    state = personal_information_state,
    zip = personal_information_zip,
    gender = personal_information_gender,
    self_identity = what_race_s_do_you_identify_with,
    gpa = grades_gpa,
    high_school = "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    jkcf = "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    gpa_weight = grades_is_this_gpa_weighted,
    psat_math = psat_scores_math,
    psat_reading_writing = psat_scores_reading_and_writing,
    sat_math = sat_scores_math,
    sat_reading_writing = sat_scores_reading_and_writing,
    act_math = act_scores_math,
    act_writing = act_scores_writing,
    act_read = act_scores_reading,
    act_science = act_scores_science,
    documented_disability = "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    first_gen = "did_any_of_your_parents_guardians_attend_college",
    house_size = "how_many_people_live_in_your_home",
    geographic_location = "how_do_you_describe_where_you_live",
    school_impact = "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    american_citizen = "are_you_an_american_citizen"
  ) |>
  filter(application_form_hillman_academy_completion_status == "Completed") |>
  mutate(
    # 2023 asks "did any of your parents/guardians attend college?" -- opposite
    # polarity to 2019-2022 "will you be the FIRST person to attend college?"
    # Flip so first_gen = "Yes" consistently means first-generation in all years.
    first_gen = case_when(
      tolower(first_gen) == "no" ~ "Yes",
      tolower(first_gen) == "yes" ~ "No",
      TRUE ~ first_gen
    ),
    gender = as.integer(tolower(gender) == "male"),
    gpa_weight = if_else(tolower(gpa_weight) == "yes", 1L, 0L),
    zip = as.character(zip),
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
  ) |>
  mutate(year = 2023)

# Stipend data for 2023 is in a separate text file
df2023_stipend <- read_delim(
  here("data", "raw", "applicants", "2023_applicants_stipendEligible.txt"),
  delim = "\t",
  escape_double = FALSE,
  trim_ws = TRUE
) |>
  clean_names() |>
  rename(
    stipend = stipend_eligible,
    first_name = applicant_first_name,
    last_name = applicant_last_name
  )

df_2023 <- df_2023 |>
  left_join(df2023_stipend, by = c("first_name", "last_name")) |>
  # stipend join creates duplicates when nickname partially matches legal name;
  # keep row with non-missing stipend, otherwise first row. This resolves most
  # cases at join time; the global remove_duplicates block below catches any
  # that remain after bind_rows.
  group_by(first_name, last_name) |>
  arrange(desc(!is.na(stipend)), .by_group = TRUE) |>
  slice(1) |>
  ungroup()
rm(df2023_stipend)

# =============================================================================
# COMBINE ALL YEARS
# =============================================================================

applicants <- bind_rows(
  df_2017,
  df_2018,
  df_2019,
  df_2020,
  df_2021,
  df_2022,
  df_2023
)

# act_verbal exists in 2019-2022 source files and is dropped here along with
# other year-specific columns not carried forward to the combined dataset.
applicants <- applicants |>
  select(
    -c(
      high_school_pub_priv,
      school_district,
      reduced_lunch,
      offer_ap,
      `income_eligible_(reduced_lunch)`,
      alumni,
      act_verbal,
      low_income,
      middle_name
    )
  )

# =============================================================================
# STANDARDIZE VARIABLES
# =============================================================================

# Name cleaning via shared helper. strip_suffix = TRUE on last_name so e.g.
# "Smith Jr" matches "Smith" in the alumni tracker (which strips suffixes).
applicants <- applicants |>
  mutate(
    first_name = clean_person_name(first_name),
    last_name  = clean_person_name(last_name, strip_suffix = TRUE)
  )

# GPA standardization to 4.0 scale.
#
# Conversion logic:
#   ≤ 4.0           → assume valid 4.0-scale GPA, keep as-is
#   4.0 < x ≤ 5.0   → if weighted, convert from 5.0 scale; otherwise NA
#                     (a 4.5 unweighted GPA on a 4.0 scale is not possible)
#   5.0 < x ≤ 6.0   → if weighted, convert from 6.0 scale; otherwise NA
#   6.0 < x ≤ 10.0  → unconditionally convert from 10.0 scale
#                     (no plausible 4.0-scale interpretation, so the weighted
#                      flag is uninformative here)
#   10.0 < x < 65.0 → no recognized scale mapping → NA
#   65 ≤ x ≤ 100    → convert from 100-point scale via standard letter-grade
#                     bands (A = 93+, A- = 90+, B+ = 87+, ...)
applicants <- applicants |>
  mutate(
    gpa_old = gpa,
    gpa_num = as.numeric(gpa_old)
  ) |>
  mutate(
    gpa = case_when(
      is.na(gpa_num) ~ NA_real_,
      gpa_num == 0 ~ NA_real_,
      gpa_num <= 4.0 ~ gpa_num,
      gpa_num > 4.0 & gpa_num <= 5.0 ~
        if_else(gpa_weight == 1, (gpa_num / 5.0) * 4.0, NA_real_),
      gpa_num > 5.0 & gpa_num <= 6.0 ~
        if_else(gpa_weight == 1, (gpa_num / 6.0) * 4.0, NA_real_),
      gpa_num > 6.0 & gpa_num <= 10.0 ~ (gpa_num / 10.0) * 4.0,
      gpa_num >= 93 ~ 4.0,
      gpa_num >= 90 ~ 3.7 + (gpa_num - 90) * 0.1,
      gpa_num >= 87 ~ 3.3 + (gpa_num - 87) * 0.133,
      gpa_num >= 83 ~ 3.0 + (gpa_num - 83) * 0.075,
      gpa_num >= 80 ~ 2.7 + (gpa_num - 80) * 0.1,
      gpa_num >= 77 ~ 2.3 + (gpa_num - 77) * 0.133,
      gpa_num >= 73 ~ 2.0 + (gpa_num - 73) * 0.075,
      gpa_num >= 70 ~ 1.7 + (gpa_num - 70) * 0.1,
      gpa_num >= 65 ~ 1.0 + (gpa_num - 65) * 0.15,
      gpa_num < 65 ~ NA_real_,
      TRUE ~ NA_real_
    ),
    gpa = round(gpa, 3)
  ) |>
  select(-gpa_num, -gpa_old)

# Out-of-range value cleaning
applicants <- applicants |>
  mutate(
    grade = if_else(grade < 9 | grade > 12, NA_integer_, grade),
    house_size = if_else(
      house_size == 0 | house_size > 11,
      NA_integer_,
      house_size
    ),
    # zip stored as character to preserve leading zeros (e.g. CT "06510")
    zip_int = suppressWarnings(as.integer(zip)),
    zip = if_else(
      is.na(zip_int) | zip_int < 501 | zip_int > 99950,
      NA_character_,
      zip
    ),

    # SAT sections: 200-800; PSAT sections: 160-760; ACT sections: 1-36.
    # Out-of-range (incl. zero) → NA.
    sat_math = if_else(sat_math < 200 | sat_math > 800, NA_integer_, sat_math),
    sat_verbal = if_else(sat_verbal < 200 | sat_verbal > 800, NA_integer_, sat_verbal),
    sat_writing = if_else(sat_writing < 200 | sat_writing > 800, NA_integer_, sat_writing),
    sat_reading_writing = if_else(
      sat_reading_writing < 200 | sat_reading_writing > 800,
      NA_integer_,
      sat_reading_writing
    ),

    psat_math = if_else(psat_math < 160 | psat_math > 760, NA_integer_, psat_math),
    psat_verbal = if_else(psat_verbal < 160 | psat_verbal > 760, NA_integer_, psat_verbal),
    psat_writing = if_else(psat_writing < 160 | psat_writing > 760, NA_integer_, psat_writing),
    psat_reading_writing = if_else(
      psat_reading_writing < 160 | psat_reading_writing > 760,
      NA_integer_,
      psat_reading_writing
    ),

    act_math = if_else(act_math < 1 | act_math > 36, NA_integer_, act_math),
    act_read = if_else(act_read < 1 | act_read > 36, NA_integer_, act_read),
    act_science = if_else(act_science < 1 | act_science > 36, NA_integer_, act_science),
    act_writing = if_else(act_writing < 1 | act_writing > 36, NA_integer_, act_writing)
  ) |>
  select(-any_of(c(
    "date_of_birth",
    "application_form_hillman_academy_completion_status",
    "zip_int"
  )))

# Derive HS graduation year from application year and grade
applicants <- applicants |>
  mutate(hs_grad_year = year + (12 - grade))

# =============================================================================
# REMOVE DUPLICATES
# =============================================================================

duplicates_check <- applicants |>
  group_by(first_name, last_name, year) |>
  filter(n() > 1) |>
  summarise(n = n(), .groups = "drop")

if (nrow(duplicates_check) > 0) {
  message(
    "Warning: ",
    nrow(duplicates_check),
    " duplicate name/year groups found"
  )
  print(duplicates_check)
}

# drop blank rows introduced by 2022 file
applicants <- applicants |>
  filter(!is.na(first_name), !is.na(last_name))

# Manually identified duplicate entries verified against source data.
# For each flagged name/year, the row with non-NA stipend (then non-NA gpa)
# is kept as row 1, and any subsequent rows are dropped. The deterministic
# arrange before row_number() prevents the kept row from drifting if the
# raw export reorders records on a re-pull.
# year column ensures only the duplicate entry for that specific year is
# removed — not all records for that student across all years.
remove_duplicates <- tibble(
  first_name = c(
    # pre-existing duplicates
    "amanda",
    "angela",
    "grace",
    "imani",
    "sanyah",
    # 2022 duplicates from legal name column switch
    "arnav",
    "charles",
    "daniel",
    # 2023 duplicates from stipend join
    "andrew",
    "angelina",
    "anjali",
    "bhoomika",
    "dylan",
    "ethan",
    "hannah",
    "jeremy",
    "maria",
    "nandita",
    "nevaeh",
    "nitin",
    "peter",
    "rashmita",
    "risha",
    "rithvik",
    "shivani",
    "sophia",
    "sravan",
    "vienna"
  ),
  last_name = c(
    "lu",
    "tao",
    "wang",
    "smith",
    "nabi",
    "patel",
    "mawhinney",
    "wang",
    "li",
    "huang",
    "srinivasan",
    "navandar",
    "sun",
    "hu",
    "habershaw",
    "lee",
    "parkhitko",
    "kolli",
    "foster",
    "gupta",
    "ko",
    "chekka",
    "solanki",
    "avula",
    "umesh",
    "hadi",
    "koduri",
    "li"
  ),
  year = c(
    2018L,
    2021L,
    2021L,
    2017L,
    2021L,
    2022L,
    2022L,
    2022L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L,
    2023L
  )
)

# Tag each row within its name/year group, then drop rows 2+ only for flagged
# duplicates. This preserves the first (kept) record even if there are 3+ rows.
applicants <- applicants |>
  group_by(first_name, last_name, year) |>
  arrange(
    desc(!is.na(stipend)),
    desc(!is.na(gpa)),
    .by_group = TRUE
  ) |>
  mutate(.row_n = row_number()) |>
  ungroup() |>
  anti_join(
    remove_duplicates |> mutate(.row_n = 2L),
    by = c("first_name", "last_name", "year", ".row_n")
  ) |>
  select(-.row_n)

# Normalize state to uppercase 2-letter abbreviations.
# Strips non-alpha characters (e.g. "PA`"), converts full names to
# abbreviations. Only states observed in this dataset are listed —
# unrecognized values pass through as-is for manual review.
applicants <- applicants |>
  mutate(
    state = {
      s <- state |> str_to_upper() |> str_remove_all("[^A-Z]")
      case_match(
        s,
        "PENNSYLVANIA" ~ "PA",
        "NEWJERSEY" ~ "NJ",
        "NEWYORK" ~ "NY",
        "OHIO" ~ "OH",
        "VIRGINIA" ~ "VA",
        "MARYLAND" ~ "MD",
        "DELAWARE" ~ "DE",
        "WESTVIRGINIA" ~ "WV",
        "CONNECTICUT" ~ "CT",
        "MASSACHUSETTS" ~ "MA",
        "NORTHCAROLINA" ~ "NC",
        "SOUTHCAROLINA" ~ "SC",
        "GEORGIA" ~ "GA",
        "FLORIDA" ~ "FL",
        "TEXAS" ~ "TX",
        "CALIFORNIA" ~ "CA",
        "ILLINOIS" ~ "IL",
        "MICHIGAN" ~ "MI",
        "INDIANA" ~ "IN",
        "WASHINGTON" ~ "WA",
        "HAWAII" ~ "HI",
        "ALABAMA" ~ "AL",
        "COLORADO" ~ "CO",
        "TENNESSEE" ~ "TN",
        "MISSOURI" ~ "MO",
        "DISTRICTOFCOLUMBIA" ~ "DC",
        "IOWA" ~ "IA",
        "KANSAS" ~ "KS",
        "MINNESOTA" ~ "MN",
        "NEVADA" ~ "NV",
        "OREGON" ~ "OR",
        "NEBRASKA" ~ "NE",
        "PUERTORICO" ~ "PR",
        "WISCONSIN" ~ "WI",
        "ARIZONA" ~ "AZ",
        "ARKANSAS" ~ "AR",
        "LOUISIANA" ~ "LA",
        "NEWMEXICO" ~ "NM",
        "IDAHO" ~ "ID",
        "OKLAHOMA" ~ "OK",
        "VERMONT" ~ "VT",
        # Non-US entries — recode to NA
        "BRAZIL" ~ NA_character_,
        "INDIA" ~ NA_character_,
        # Invalid / ambiguous — recode to NA
        "NA" ~ NA_character_,
        .default = s
      )
    }
  )

# =============================================================================
# FINAL WHITELIST
# =============================================================================
# Per-year `select()` lists are inconsistent across cohorts, so the union
# carried forward by `bind_rows` includes year-specific columns that no
# downstream script consumes (admin form fields, unrenamed score columns,
# decision codes, etc.). Restrict to the columns that 3a/3b/5 actually use,
# plus all cleaned test-score variants for descriptive/sensitivity work.

applicants <- applicants |>
  select(
    # identifiers & admin
    first_name, last_name, year, hs_grad_year,
    # demographics
    gender, grade,
    # academics
    gpa, gpa_weight,
    psat_math, psat_verbal, psat_writing, psat_reading_writing,
    sat_math, sat_verbal, sat_writing, sat_reading_writing,
    act_math, act_read, act_science, act_writing,
    # geography
    state, city, zip, high_school,
    # self-reported (raw — cleaned in 3b into the analysis covariates)
    self_identity,
    geographic_location,
    school_impact,
    american_citizen,
    documented_disability,
    # first_gen is extracted here for descriptive tables but is NOT used as
    # a covariate in the PS or outcome models — the question wasn't on the
    # 2017 or 2018 application forms, so its missing-indicator is collinear
    # with year FE. See script 5 base_covariates note.
    first_gen,
    stipend,
    house_size,
    jkcf
  )

# =============================================================================
# FINAL COUNTS
# =============================================================================

applicant_n <- applicants |>
  count(year, name = "n_applicants") |>
  mutate(
    cumulative_n = cumsum(n_applicants),
    pct_of_total = round(n_applicants / sum(n_applicants) * 100, 1)
  )

write_csv(applicant_n, here("output", "counts", "n_applicants_by_year.csv"))

applicant_n

rm(list = setdiff(ls(), c("applicants", "applicant_n")))
