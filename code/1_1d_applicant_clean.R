# =============================================================================
# Hillman Summer Applicants
# =============================================================================

library(tidyverse)
library(readxl)
library(here)
library(stringr)
library(lubridate)
library(naniar)
library(janitor)
library(readr)
library(tidylog)
library(MatchIt)
library(finalfit)
library(skimr)
library(haven)
library(cobalt)

# -----------------------------------------------------------------------------
here()

# -----------------------------------------------------------------------------
# Helper Function: Custom column name cleaning
clean_custom_names <- function(df) {
  df |>
    rename_with(
      ~ str_to_lower(.) |>
        str_replace_all("\\s", "_") |>
        str_replace_all(":", "") |>
        str_replace_all("\\?", "")
    )
}

# -----------------------------------------------------------------------------
# 2017 Data ------------------------------------------------------------------
df_2017 <- read_excel(here("data", "hillman_2017.xlsx")) |>
  clean_custom_names() |>
  select(
    -any_of(
      # changed: use any_of() to avoid errors when columns are absent
      c(
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
    # changed: wrap in as.character() to avoid errors for non-character/factor inputs
    gender = if_else(tolower(as.character(gender)) == "male", 1, 0),
    gpa_weight = if_else(tolower(as.character(gpa_weight)) == "yes", 1, 0),
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

# -----------------------------------------------------------------------------
# 2018 Data ------------------------------------------------------------------
df_2018 <- read_excel(here("data", "hillman_2018.xlsx")) |>
  clean_custom_names() |>
  select(
    -any_of(
      # changed
      c(
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
    gender = if_else(tolower(as.character(gender)) == "male", 1, 0),
    gpa_weight = if_else(tolower(as.character(gpa_weight)) == "yes", 1, 0),
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

# -----------------------------------------------------------------------------
# 2019 Data ------------------------------------------------------------------
df_2019 <- read_excel(
  here("data", "hillman_2019.xlsx"),
  sheet = "All (Extra info)"
) |>
  clean_custom_names() |>
  select(
    -any_of(
      # changed
      c(
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
    gender = as.integer(tolower(as.character(gender)) == "male"), # changed
    gpa_weight = as.integer(tolower(as.character(gpa_weight)) == "yes"), # changed
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

# Merge in stipend data for 2019
df_stipend <- read_excel(here("data", "hillman_raw.xlsx"), sheet = "2019") |>
  select(first_name, last_name, stipend)

df_2019 <- df_2019 |>
  left_join(df_stipend, by = c("first_name", "last_name"))
rm(df_stipend)

# -----------------------------------------------------------------------------
# 2020 Data ------------------------------------------------------------------
df_2020 <- read_excel(here("data", "hillman_2020.xlsx")) |>
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
    gender = if_else(tolower(as.character(gender)) == "male", 1, 0),
    gpa_weight = if_else(tolower(as.character(gpa_weight)) == "yes", 1, 0),
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
  ) |>
  mutate(year = 2020)

# -----------------------------------------------------------------------------
# 2021 Data ------------------------------------------------------------------
df_2021 <- read_excel(here("data", "hillman_2021.xlsx")) |>
  clean_names() |>
  select(
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
    gender = if_else(tolower(as.character(gender)) == "male", 1, 0), # changed
    gpa_weight = if_else(tolower(as.character(gpa_weight)) == "yes", 1, 0), # changed
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
  ) |>
  mutate(year = 2021)

# -----------------------------------------------------------------------------
# 2022 Data ------------------------------------------------------------------
df_2022 <- read_excel(here("data", "hillman_2022.xlsx")) |>
  clean_names() |>
  select(
    applicant_first_name,
    applicant_last_name,
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
    first_name = applicant_first_name,
    last_name = applicant_last_name,
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
    gender = if_else(tolower(as.character(gender)) == "male", 1, 0), # changed
    gpa_weight = if_else(tolower(as.character(gpa_weight)) == "yes", 1, 0), # changed
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
  ) |>
  mutate(year = 2022)

# -----------------------------------------------------------------------------
# 2023 Data ------------------------------------------------------------------
df_2023 <- read_csv(here("data", "hillman_2023.csv")) |>
  clean_names() |>
  select(
    applicant_first_name,
    applicant_last_name,
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
    first_name = applicant_first_name,
    last_name = applicant_last_name,
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
    gender = if_else(tolower(as.character(gender)) == "male", 1, 0), # changed
    gpa_weight = if_else(tolower(as.character(gpa_weight)) == "yes", 1, 0), # changed
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
  ) |>
  mutate(year = 2023)

# Merge stipend information for 2023 from the external text file
df2023_stipend <- read_delim(
  here("data", "2023_applicants_stipendEligible.txt"),
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
  left_join(df2023_stipend, by = c("first_name", "last_name"))
rm(df2023_stipend)

# -----------------------------------------------------------------------------
# Merge All Years ------------------------------------------------------------

applicants <- bind_rows(
  df_2017,
  df_2018,
  df_2019,
  df_2020,
  df_2021,
  df_2022,
  df_2023
)

# Remove columns not needed in the final analysis and clean up names
applicants <- applicants |>
  select(
    -any_of(
      # changed
      c(
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
  ) |>
  # Clean first and last names (convert to lower case and remove extra characters)
  mutate(
    first_name = str_to_lower(first_name) |> str_remove("\\(.*") |> str_trim(),
    last_name = str_to_lower(last_name) |> str_trim()
  )

# -----------------------------------------------------------------------------
# Additional Data Adjustments ------------------------------------------------

# Clean names
applicants <- applicants |>
  mutate(
    first_name = first_name |>
      str_to_lower() |>
      str_replace_all('"(.*?)"|\\((.*?)\\)', " ") |>
      str_replace_all("[^a-z]", " ") |>
      str_squish(),

    last_name = last_name |>
      str_to_lower() |>
      str_replace_all('"(.*?)"|\\((.*?)\\)', " ") |>
      str_replace_all("[^a-z]", " ") |>
      str_squish()
  )

# Standardize GPA to unweighted 4.0 scale
applicants <- applicants |>
  # standardize gpa_weight to 1/0/NA first
  mutate(
    gpa_weight = case_when(
      is.na(gpa_weight) ~ NA_real_,
      tolower(as.character(gpa_weight)) %in% c("1", "yes", "y", "true", "t") ~
        1,
      tolower(as.character(gpa_weight)) %in% c("0", "no", "n", "false", "f") ~
        0,
      TRUE ~ suppressWarnings(as.numeric(as.character(gpa_weight)))
    )
  ) |>

  # keep original and parse numeric GPA
  mutate(
    gpa_old = gpa,
    gpa_num = as.numeric(gpa_old)
  ) |>

  # compute standardized unweighted GPA (use gpa_weight for 4-6 range)
  mutate(
    gpa = case_when(
      is.na(gpa_num) ~ NA_real_,
      gpa_num < 0 ~ NA_real_, # suspicious negative -> manual review
      gpa_num > 120 ~ NA_real_, # extremely large -> manual review

      # percentage-like / percentish (including modestly >100 due to extra credit)
      gpa_num > 10 ~ pmin(4, gpa_num / 25),

      # 0-10 scale (7-10)
      gpa_num > 6 ~ pmin(4, gpa_num * 4 / 10),

      # 4-6 : use gpa_weight to decide:
      # - if weighted (1): assume 0-5 weighted -> scale *4/5
      # - if explicitly not weighted (0): assume the value is reported on a 4.0-ish scale or mis-entry -> cap at 4
      # - if gpa_weight is NA: conservatively cap at 4 but flag for review
      gpa_num > 4 & gpa_num <= 6 ~
        case_when(
          gpa_weight == 1 ~ pmin(4, gpa_num * 4 / 5),
          gpa_weight == 0 ~ pmin(4, gpa_num), # cap to 4
          is.na(gpa_weight) ~ pmin(4, gpa_num) # conservative: cap to 4 (flag created below)
        ),

      # <=4 : assume already on 4.0 scale
      TRUE ~ pmin(4, gpa_num)
    ),

    # flags for QA
    gpa_over_100_flag = if_else(!is.na(gpa_num) & gpa_num > 100, TRUE, FALSE),
    gpa_weight_missing_for_4to6 = if_else(
      is.na(gpa_weight) & gpa_num > 4 & gpa_num <= 6,
      TRUE,
      FALSE
    ),

    # rounding for matching stability
    gpa = round(gpa, 3)
  ) |>

  # drop temporary numeric helper
  select(-gpa_num)


# Fix anomalous house size entries (set values greater than 11 to NA)
applicants <- applicants |>
  mutate(house_size = if_else(house_size > 11, NA_integer_, house_size))

# Create High School Graduation Year variable
applicants <- applicants |>
  mutate(hs_grad_year = year + (12 - grade))

# Check for duplicate applicants by first name, last name, and year.
applicants |>
  group_by(first_name, last_name, year) |>
  filter(n() > 1) |>
  summarise(n = n(), .groups = "drop")

# Remove manually flagged duplicate records
remove_duplicates <- tibble(
  first_name = c(
    "amanda",
    "angela",
    "arnav",
    "charles",
    "daniel",
    "grace",
    "imani",
    "sanyah"
  ),
  last_name = c(
    "lu",
    "tao",
    "patel",
    "mawhinney",
    "wang",
    "wang",
    "smith",
    "nabi"
  )
)

applicants <- applicants |>
  anti_join(remove_duplicates, by = c("first_name", "last_name"))

# -----------------------------------------------------------------------------
# Final Checks & Summaries ---------------------------------------------------

# Count unique applicants by year
applicants |>
  group_by(first_name, last_name, year) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(year) |>
  summarise(total_unique_students = sum(n), .groups = "drop")

rm(list = setdiff(ls(), "applicants")) # Keep only the applicants data frame in memory
