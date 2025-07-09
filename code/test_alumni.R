# =============================================================================
# Hillman Summer Applicants: Data Cleaning Script
# =============================================================================

# 1. Load necessary packages
library(tidyverse)   # includes dplyr, tidyr, stringr, readr, purrr, etc.
library(readxl)      # for Excel files
library(here)        # for project-relative file paths
library(janitor)     # for clean_names()
library(naniar)      # for missingness visualizations
library(skimr)       # for detailed summaries

# -----------------------------------------------------------------------------
# 2. Helper functions

# Clean and standardize column names:
clean_custom_names <- function(df) {
  df %>%
    clean_names() %>%                                             # lowercase, underscores
    rename_with(~ str_replace_all(., "[^[:alnum:]_]", ""), everything())  # drop any remaining punctuation
}

# Convert Yes/No and Male/Female to binary 1/0
to_binary <- function(x) {
  case_when(
    tolower(x) %in% c("yes", "male")   ~ 1,
    tolower(x) %in% c("no",  "female") ~ 0,
    TRUE                                ~ NA_real_
  )
}

# -----------------------------------------------------------------------------
# 3. Read & clean each year's data

# ---- 2017 -------------------------------------------------------------------
df_2017 <- read_excel(here("data", "hillman_2017.xlsx")) %>%
  clean_custom_names() %>%
  select(-any_of(c(
    "date_of_birth","email","home_phone","cell_phone","address_1","address_2",
    "parent_guard_full_name","parent_guard_email","parent_guard_add_1",
    "parent_guard_add_2","parent_guard_city","parent_guard_state","parent_guard_zip",
    "science_math_courses","int_cancer_biology","int_tumor_immun",
    "int_computer_science","int_drug_discovery","int_women_cancer","int_cancer_env",
    "prior_research","jkcf_advisor","jkcf_start_date","jkcf_end_date",
    "first_reference_full_name","first_reference_email",
    "sec_reference_full_name","sec_reference_email",
    "computer_proficiency","lab_experimentation","housing","marketing",
    "name","self_agree_statement","...69","...70","green_card"
  ))) %>%
  rename(
    high_school = high_school_name,
    grade       = current_grade,
    jkcf        = jkcf_young_scholar,
    house_size  = household_size,
    gpa_weight  = gpa_weighted
  ) %>%
  mutate(
    across(any_of(c("gender","gpa_weight","jkcf")), to_binary),
    across(any_of(c("zip","grade","gpa",
                    starts_with("sat_"), starts_with("psat_"), starts_with("act_"),
                    "house_size")),
           as.numeric),
    year = 2017
  )

# ---- 2018 -------------------------------------------------------------------
df_2018 <- read_excel(here("data", "hillman_2018.xlsx")) %>%
  clean_custom_names() %>%
  select(-any_of(c(
    "site","date_of_birth","wet_lab_eligible","alumnus",
    "absences_y_n","absences__#_of_days","wet_v_dry",
    "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following",
    "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following_other",
    "list_prior_research_experiences_if_any_extracurricular_activities_honors_and_awards",
    "list_science_and_math_courses_with_grades","essay_written_or_uploaded",
    "upload_your_essay","a","b","one_other_topic_of_your_choice_from_the_list_above",
    "recommender","do_you_have_permanent_resident_or_green_card_status",
    "submission_date","if_yes_please_explain","email","home_phone","cell_phone","address",
    "1st_choice","2nd_choice","3rd","4th","5th","6th",
    "stage_completion_%","do_you_qualify_based_on_your_family_size_and_income_please_refer_to_the_chart_below_indicating_maximum_thresholds",
    "how_do_you_identify_yourself_other","vi_housing"
  ))) %>%
  rename(
    stipend              = stipend_eligible,
    self_identity        = how_do_you_identify_yourself,
    documented_disability = do_you_have_a_documented_disability_a_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_a_americans_with_disabilities_act_of_1990,
    grade                = grade_year,
    gpa_weight           = gpa_weighted,
    jkcf                 = are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar,
    psat_math            = psat_scores_math,
    sat_math             = sat_scores_math,
    act_math             = act_scores_math,
    psat_verbal          = psat_scores_verbal,
    sat_verbal           = sat_scores_verbal,
    act_science          = act_scores_science,
    act_read             = act_scores_reading,
    act_writing          = act_scores_writing,
    geographic_location  = how_do_you_describe_where_you_live,
    house_size           = what_is_your_total_household_size,
    american_citizen     = are_you_an_american_citizen,
    school_impact        = do_you_believe_your_school_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research
  ) %>%
  mutate(
    across(any_of(c("gender","gpa_weight","jkcf","american_citizen")), to_binary),
    across(any_of(c("zip","grade","gpa",
                    starts_with("sat_"), starts_with("psat_"), starts_with("act_"),
                    "house_size")),
           as.numeric),
    year = 2018
  )

# ---- 2019 -------------------------------------------------------------------
df_2019 <- read_excel(here("data", "hillman_2019.xlsx"), sheet = "All (Extra info)") %>%
  clean_custom_names() %>%
  select(-any_of(c(
    "date_of_birth","email","home_phone","cell_phone","address_10",
    "parent_or_legal_guardian_s_full_name_14","parent_or_legal_guardian_s_email_15",
    "housing","1st_choice","2nd","3rd","4th","5th","6th",
    "expected_absences","#_of_absences","wet_or_dry_lab_preferred",
    "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following",
    "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following_other",
    "parent_or_legal_guardian_s_full_name_40","parent_or_legal_guardian_s_email_41",
    "upload_your_essay_50","a","b","upload_transcript",
    "would_you_like_to_upload_your_essay_or_use_the_space_provided",
    "recommender","submission_date","stage_completion_%","possible_2_year_program__are_you_interested_in_a_new_two_year_program_through_the_hillman_academy",
    "do_you_have_permanent_resident_or_green_card_status",
    "do_you_have_a_documented_disability__e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    "if_yes_please_explain","how_do_you_identify_yourself_caucasian_white",
    "how_do_you_identify_yourself_african_african_american_black","how_do_you_identify_yourself_asian",
    "how_do_you_identify_yourself_hawaiian_or_pacific_islander",
    "how_do_you_identify_yourself_native_american_alaska_native",
    "how_do_you_identify_yourself_hispanic_latino","how_do_you_identify_yourself_do_not_wish_to_answer",
    "upload_your_essay_79","address_36","city_37","state_38","zip_39","group",
    "prior_research_experiences","list_science_and_math_courses_with_grades","stipend_eligible"
  ))) %>%
  rename(
    high_school           = school,
    city                  = city_11,
    state                 = state_12,
    zip                   = zip_13,
    jkcf                  = jack_kent_cooke,
    gpa_weight            = gpa_weighted_yes_no,
    grade                 = current_grade,
    self_identity         = race,
    documented_disability = do_you_have_a_documented_disability_a_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_a_americans_with_disabilities_act_of_1990,
    psat_math             = psat_scores_math,
    sat_math              = sat_scores_math,
    act_math              = act_scores_math,
    psat_verbal           = psat_scores_verbal,
    sat_verbal            = sat_scores_verbal,
    act_science           = act_scores_science,
    act_read              = act_scores_reading,
    act_writing           = act_scores_writing,
    act_verbal            = act_scores_verbal,
    american_citizen      = are_you_an_american_citizen,
    first_gen             = will_you_be_the_first_person_in_your_family_to_attend_college,
    school_impact         = do_you_believe_your_school_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research,
    geographic_location   = how_do_you_describe_where_you_live,
    house_size            = household_size
  ) %>%
  mutate(
    across(any_of(c("gender","gpa_weight","jkcf","american_citizen")), to_binary),
    across(any_of(c("zip","grade","gpa",
                    starts_with("sat_"), starts_with("psat_"), starts_with("act_"),
                    "house_size")),
           as.numeric),
    year = 2019
  ) %>%
  # Merge stipend info
  left_join(
    read_excel(here("data", "hillman_raw.xlsx"), sheet = "2019") %>%
      clean_custom_names() %>%
      select(first_name, last_name, stipend),
    by = c("first_name", "last_name")
  )

# ---- 2020 -------------------------------------------------------------------
df_2020 <- read_excel(here("data", "hillman_2020.xlsx")) %>%
  clean_custom_names() %>%
  select(any_of(c(
    "first","last","gender","current_grade_5","alumni","race_15","school",
    "city","state","zip",
    "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    "gpa","weighted_y_n",
    "low_income","disability","x1st_gen_college",
    "what_is_your_total_household_size",
    "how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_w",
    "are_you_an_american_citizen","stipend_eligible_77"
  )), psat_scores_math:act_scores_science) %>%
  rename(
    first_name             = first,
    last_name              = last,
    grade                  = current_grade_5,
    self_identity          = race_15,
    high_school            = school,
    jkcf                   = are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar,
    gpa_weight             = weighted_y_n,
    psat_math              = psat_scores_math,
    psat_reading_writing   = psat_scores_reading_and_writing,
    sat_math               = sat_scores_math,
    sat_reading_writing    = sat_scores_reading_and_writing,
    act_math               = act_scores_math,
    act_verbal             = act_scores_verbal,
    act_read               = act_scores_reading,
    act_science            = act_scores_science,
    documented_disability  = disability,
    first_gen              = x1st_gen_college,
    house_size             = what_is_your_total_household_size,
    geographic_location    = how_do_you_describe_where_you_live,
    school_impact          = do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_w,
    american_citizen       = are_you_an_american_citizen,
    stipend                = stipend_eligible_77
  ) %>%
  mutate(
    across(any_of(c("gender","gpa_weight","jkcf","american_citizen")), to_binary),
    across(any_of(c("zip","grade","gpa",
                    starts_with("sat_"), starts_with("psat_"), starts_with("act_"),
                    "house_size")),
           as.numeric),
    year = 2020
  )

# ---- 2021 -------------------------------------------------------------------
df_2021 <- read_excel(here("data", "hillman_2021.xlsx")) %>%
  clean_custom_names() %>%
  select(any_of(c(
    "first_1","last_2","gender","date_of_birth","grades_current_grade_6",
    "race","school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    "city","state","zip",
    "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    "grades_gpa","grades_is_this_gpa_weighted",
    "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    "will_you_be_the_first_person_in_your_family_to_attend_college",
    "how_many_people_live_in_your_home","how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    "are_you_an_american_citizen","stipend_eligible", "act_scores_read"
  )), psat_scores_math:act_scores_science) %>%
  rename(
    first_name           = first_1,
    last_name            = last_2,
    grade                = grades_current_grade_6,
    self_identity        = race,
    gpa                  = grades_gpa,
    high_school          = school_you_attend_if_homeschooled_please_enter_school_district_of_residence,
    jkcf                 = are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar,
    gpa_weight           = grades_is_this_gpa_weighted,
    psat_math            = psat_scores_math,
    psat_reading_writing = psat_scores_reading_and_writing,
    sat_math             = sat_scores_math,
    sat_reading_writing  = sat_scores_reading_and_writing,
    act_math             = act_scores_math,
    act_verbal           = act_scores_verbal,
    act_read             = act_scores_reading,
    act_science          = act_scores_science,
    act_writing          = act_scores_writing,
    documented_disability= do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other,
    first_gen            = will_you_be_the_first_person_in_your_family_to_attend_college,
    house_size           = how_many_people_live_in_your_home,
    geographic_location  = how_do_you_describe_where_you_live,
    school_impact        = do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify,
    american_citizen     = are_you_an_american_citizen,
    stipend              = stipend_eligible
  ) %>%
  mutate(
    across(any_of(c("gender","gpa_weight","jkcf","american_citizen")), to_binary),
    across(any_of(c("zip","grade","gpa",
                    starts_with("sat_"), starts_with("psat_"), starts_with("act_"),
                    "house_size")),
           as.numeric),
    year = 2021
  )

# ---- 2022 -------------------------------------------------------------------
df_2022 <- read_excel(here("data", "hillman_2022.xlsx")) %>%
  clean_custom_names() %>%
  select(any_of(c(
    "applicant_first_name","applicant_last_name","personal_information_gender",
    "grades_current_grade","what_race_s_do_you_identify_with",
    "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    "personal_information_city","personal_information_state","personal_information_zip",
    "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    "grades_gpa","grades_is_this_gpa_weighted",
    "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    "will_you_be_the_first_person_in_your_family_to_attend_college",
    "how_many_people_live_in_your_home","how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    "are_you_an_american_citizen","stipend_eligible_y_n"
  )), psat_scores_math:act_scores_science) %>%
  rename(
    first_name           = applicant_first_name,
    last_name            = applicant_last_name,
    grade                = grades_current_grade,
    gender               = personal_information_gender,
    self_identity        = what_race_s_do_you_identify_with,
    high_school          = personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence,
    city                 = personal_information_city,
    state                = personal_information_state,
    zip                  = personal_information_zip,
    jkcf                 = are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar,
    gpa                  = grades_gpa,
    gpa_weight           = grades_is_this_gpa_weighted,
    psat_math            = psat_scores_math,
    psat_reading_writing = psat_scores_reading_and_writing,
    sat_math             = sat_scores_math,
    sat_reading_writing  = sat_scores_reading_and_writing,
    act_math             = act_scores_math,
    act_verbal           = act_scores_verbal,
    act_read             = act_scores_reading,
    act_science          = act_scores_science,
    act_writing          = act_scores_writing,
    documented_disability= do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other,
    first_gen            = will_you_be_the_first_person_in_your_family_to_attend_college,
    house_size           = how_many_people_live_in_your_home,
    geographic_location  = how_do_you_describe_where_you_live,
    school_impact        = do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify,
    american_citizen     = are_you_an_american_citizen,
    stipend              = stipend_eligible_y_n
  ) %>%
  mutate(
    across(any_of(c("gender","gpa_weight","jkcf","american_citizen")), to_binary),
    across(any_of(c("zip","grade","gpa",
                    starts_with("sat_"), starts_with("psat_"), starts_with("act_"),
                    "house_size")),
           as.numeric),
    year = 2022
  )

# ---- 2023 -------------------------------------------------------------------
df_2023 <- read_csv(here("data", "hillman_2023.csv")) %>%
  clean_custom_names() %>%
  select(any_of(c(
    "applicant_first_name","applicant_last_name","personal_information_gender",
    "grades_current_grade","what_race_s_do_you_identify_with",
    "personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence",
    "personal_information_city","personal_information_state","personal_information_zip",
    "are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar",
    "grades_gpa","grades_is_this_gpa_weighted",
    "do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other",
    "did_any_of_your_parents_guardians_attend_college",
    "how_many_people_live_in_your_home","how_do_you_describe_where_you_live",
    "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify",
    "are_you_an_american_citizen","application_form_hillman_academy_completion_status"
  )), psat_scores_math:act_scores_science) %>%
  filter(application_form_hillman_academy_completion_status == "Completed") %>%
  rename(
    first_name           = applicant_first_name,
    last_name            = applicant_last_name,
    grade                = grades_current_grade,
    gender               = personal_information_gender,
    self_identity        = what_race_s_do_you_identify_with,
    high_school          = personal_information_school_you_attend_if_homeschooled_please_enter_school_district_of_residence,
    city                 = personal_information_city,
    state                = personal_information_state,
    zip                  = personal_information_zip,
    jkcf                 = are_you_a_current_jack_kent_cooke_foundation_jkcf_young_scholar,
    gpa                  = grades_gpa,
    gpa_weight           = grades_is_this_gpa_weighted,
    psat_math            = psat_scores_math,
    psat_reading_writing = psat_scores_reading_and_writing,
    sat_math             = sat_scores_math,
    sat_reading_writing  = sat_scores_reading_and_writing,
    act_math             = act_scores_math,
    act_read             = act_scores_reading,
    act_science          = act_scores_science,
    act_writing          = act_scores_writing,
    documented_disability= do_you_have_a_documented_disability_e_g_auditory_motor_visual_cognitive_other_that_substantially_limits_one_or_more_major_life_activites_as_described_by_the_americans_with_disabilities_act_of_1990_other,
    first_gen            = did_any_of_your_parents_guardians_attend_college,
    house_size           = how_many_people_live_in_your_home,
    geographic_location  = how_do_you_describe_where_you_live,
    school_impact        = do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below_if_you_meet_at_least_two_of_the_following_criteria_you_would_be_eligible_for_a_stipend_please_explain_how_you_qualify,
    american_citizen     = are_you_an_american_citizen
  ) %>%
  mutate(
    across(any_of(c("gender","gpa_weight","jkcf","american_citizen")), to_binary),
    across(any_of(c("zip","grade","gpa",
                    starts_with("sat_"), starts_with("psat_"), starts_with("act_"),
                    "house_size")),
           as.numeric),
    year = 2023
  ) %>%
  # Merge stipend info
  left_join(
    read_delim(
      here("data", "2023_applicants_stipendEligible.txt"),
      delim = "\t", escape_double = FALSE, trim_ws = TRUE
    ) %>%
      clean_custom_names() %>%
      rename(
        first_name = applicant_first_name,
        last_name  = applicant_last_name,
        stipend    = stipend_eligible
      ),
    by = c("first_name", "last_name")
  )

# -----------------------------------------------------------------------------
# 4. Combine all years

df_2017 <- df_2017 %>%
  mutate(
    sat_math = as.numeric(sat_math),
    sat_verbal = as.numeric(sat_verbal),
    psat_math = as.numeric(psat_math),
    psat_verbal = as.numeric(psat_verbal),
    act_math = as.numeric(act_math),
    act_read = as.numeric(act_read),
    act_science = as.numeric(act_science),
    act_writing = as.numeric(act_writing),
    american_citizen = as.numeric(american_citizen)
  )

df_2023 <- df_2023 %>%
  mutate(
    sat_math = as.numeric(sat_math),
    psat_math = as.numeric(psat_math),
    act_math = as.numeric(act_math),
    act_read = as.numeric(act_read),
    act_science = as.numeric(act_science),
    act_writing = as.numeric(act_writing),
    american_citizen = as.numeric(american_citizen)  )

applicants <- bind_rows(df_2017, df_2018, df_2019,
                        df_2020, df_2021, df_2022, df_2023)

# 5. Final cleanup & adjustments
remove_duplicates <- tibble(
  first_name = c("amanda","angela","arnav","charles","daniel","grace","imani","sanyah"),
  last_name  = c("lu","tao","patel","mawhinney","wang","wang","smith","nabi")
)

applicants <- applicants %>%
  select(-any_of(c(
    "high_school_pub_priv","school_district","reduced_lunch","offer_ap",
    "income_eligible_reduced_lunch","alumni","act_verbal","low_income","middle_name"
  ))) %>%
  mutate(
    across(c(first_name, last_name),
           ~ str_to_lower(.) %>% str_remove("\\(.*\\)") %>% str_trim()),
    # Convert percent-style GPAs to 4.0 scale
    gpa = case_when(
      gpa > 96             ~ 4.0,
      between(gpa, 90, 96) ~ 3.7,
      TRUE                 ~ gpa
    )
  ) %>%
  # Replace zeros with NA in test scores
  mutate(across(matches("^(sat|psat|act)_"), as.numeric)) %>%
  mutate(across(matches("^(sat|psat|act)_"), ~ na_if(., 0))) %>%
  # Fix anomalous house sizes
  mutate(house_size = if_else(house_size > 11, NA_integer_, house_size)) %>%
  # Add HS graduation year
  mutate(hs_grad_year = year + (12 - grade)) %>%
  # Remove manually flagged duplicates
  anti_join(remove_duplicates, by = c("first_name", "last_name"))

# -----------------------------------------------------------------------------
# 6. Final checks & summaries
naniar::vis_miss(applicants)
glimpse(applicants)
skim(applicants)

# Keep only the final data frame in memory
rm(list = setdiff(ls(), "applicants"))
