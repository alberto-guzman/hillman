# load packaged, must have pacman already installed
pacman::p_load(
  tidyverse, magrittr, pastecs, readr, readxl, sjlabelled, tibble, psych, dplyr, janitor,
  eeptools, cobalt, gtsummary, stargazer, data.table, skimr, stringr, vioplot, finalfit,
  parallel, data.table, tictoc, batchtools, styler, tidyverse, magrittr, lubridate, here, vtable
)

# directory
here()

######################################################## 3
######################## 2017
########################################################

# read in data
df <- read_excel(here("data", "hillman_2017.xlsx"))

colnames(df) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(df) %<>% str_replace_all(":", "")
colnames(df) %<>% str_replace_all("\\?", "")

df <- select(df, -c(
  date_of_birth, email, home_phone, cell_phone, address_1, address_2,
  parent_guard_full_name, parent_guard_email, parent_guard_add_1,
  parent_guard_add_2, parent_guard_city, parent_guard_state, parent_guard_zip,
  science_math_courses, int_cancer_biology, int_tumor_immun, int_computer_science,
  int_drug_discovery, int_women_cancer, int_cancer_env, prior_research, jkcf_advisor,
  jkcf_start_date, jkcf_end_date, first_reference_full_name, first_reference_email, sec_reference_full_name, sec_reference_email, computer_proficiency, lab_experimentation,
  housing, marketing, name, self_agree_statement, ...69, ...70, green_card
))

df <- rename(df, high_school = "high_school_name")
df <- rename(df, grade = "current_grade")
df <- rename(df, jkcf = "jkcf_young_scholar")
df <- rename(df, house_size = "household_size")
df <- rename(df, gpa_weight = "gpa_weighted")

df <- df %>% mutate(gender = tolower(gender))
df <- df %>% mutate(gender = if_else(gender == "male", 1, 0))
df <- df %>% mutate(gpa_weight = tolower(gpa_weight))
df <- df %>% mutate(gpa_weight = if_else(gpa_weight == "yes", 1, 0))
df <- df %>% mutate(high_school_pub_priv = tolower(high_school_pub_priv))
df <- df %>% mutate(jkcf = tolower(jkcf))

df <- df %>%
  mutate(
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
    house_size = as.integer(house_size)
  )

df$year <- 2017

df_2017 <- df

######################################################## 3
######################## 2018
########################################################

# read in data
df <- read_excel(here('data','hillman_2018.xlsx'))
                 
colnames(df) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(df) %<>% str_replace_all(":", "")
colnames(df) %<>% str_replace_all("\\?", "")

df <- select(df, -c(
  site, date_of_birth, wet_lab_eligible, alumnus, "absences_(y/n)", "absences_(#_of_days)", "wet_v_dry",
  "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following",
  "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following_[other]",
  "list_prior_research_experiences_(if_any),_extracurricular_activities,_honors_and_awards.",
  "list_science_and_math_courses_with_grades",
  "essay_written_or_uploaded",
  "upload_your_essay",
  "a.", "b.", "one_other_topic_of_your_choice_from_the_list_above.", recommender,
  do_you_have_permanent_resident_or_green_card_status,
  submission_date,
  "if_yes,_please_explain",
  email, home_phone, cell_phone, address, "1st_choice", "2nd_choice",
  "3rd", "4th", "5th", "6th", "stage_completion_(%)",
  "do_you_qualify_based_on_your_family_size_and_income_please_refer_to_the_chart_below_indicating_maximum_thresholds.",
  "how_do_you_identify_yourself_[other]",
  "vi._housing"
))


df <- rename(df, stipend = "stipend_eligible")
df <- rename(df, self_identity = "how_do_you_identify_yourself")
df <- rename(df, documented_disability = "do_you_have_a_documented_disabilityâ__(e.g.,_auditory,_motor,_visual,_cognitive,_other)_that_substantially_limits_one_or_more_major_life_activites_as_described_by_theâ_americans_with_disabilities_act_of_1990")
df <- rename(df, grade = "grade_(year)")
df <- rename(df, gpa_weight = "gpa_weighted")
df <- rename(df, jkcf = "are_you_a_current_jack_kent_cooke_foundation_(jkcf)_young_scholar")
df <- rename(df, psat_math = "psat_scores_|__|_math")
df <- rename(df, sat_math = "sat_scores_|__|_math")
df <- rename(df, act_math = "act_scores_|__|_math")
df <- rename(df, psat_verbal = "psat_scores_|__|_verbal")
df <- rename(df, psat_writing = "psat_scores_|__|_writing")
df <- rename(df, sat_verbal = "sat_scores_|__|_verbal")
df <- rename(df, sat_writing = "sat_scores_|__|_writing")
df <- rename(df, act_science = "act_scores_|__|_science")
df <- rename(df, act_read = "act_scores_|__|_reading")
df <- rename(df, act_writing = "act_scores_|__|_writing")
df <- rename(df, act_verbal = "act_scores_|__|_verbal")
df <- rename(df, geographic_location = "how_do_you_describe_where_you_live")
df <- rename(df, house_size = "what_is_your_total_household_size")
df <- rename(df, american_citizen = "are_you_an_american_citizen")
df <- rename(df, school_impact = "do_you_believe_your_school_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research")

df <- df %>% mutate(gender = tolower(gender))
df <- df %>% mutate(gender = if_else(gender == "male", 1, 0))
df <- df %>% mutate(gpa_weight = tolower(gpa_weight))
df <- df %>% mutate(gpa_weight = if_else(gpa_weight == "yes", 1, 0))


df <- df %>%
  mutate(
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
    house_size = as.integer(house_size)
  )




df$year <- 2018

df_2018 <- df













######################################################## 3
######################## 2019
########################################################

# read in data
df <- read_excel(here('data',"hillman_2019.xlsx"), sheet = "All (Extra info)")
colnames(df) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(df) %<>% str_replace_all(":", "")
colnames(df) %<>% str_replace_all("\\?", "")

df <- select(df, -c(
  date_of_birth, email, home_phone, cell_phone, address...10,
  "parent_or_legal_guardian's_full_name...14",
  "parent_or_legal_guardian's_email...15",
  "housing", "1st_choice", "2nd", "3rd", "4th", "5th", "6th", expected_absences,
  "#_of_absences", wet_or_dry_lab_preferred, "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following",
  "how_would_you_judge_your_sense_of_comfort_in_computer_use_and_applications_choose_one_of_the_following_[other]", "parent_or_legal_guardian's_full_name...40", "parent_or_legal_guardian's_email...41",
  "upload_your_essay...50",
  "a.", "b.", "upload_transcript",
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
  "zip...39", group, "prior_research_experiences.", list_science_and_math_courses_with_grades, "stipend_eligible"
))

df <- rename(df, high_school = "school")

df <- rename(df, city = "city...11")
df <- rename(df, state = "state...12")
df <- rename(df, zip = "zip...13")
df <- rename(df, jkcf = "jack_kent_cooke")
df <- rename(df, gpa_weight = "gpa_weighted_yes/no")
df <- rename(df, grade = "current_grade")
df <- rename(df, self_identity = "race")
df <- rename(df, documented_disability = "do_you_have_a_documented_disabilityâ__(e.g.,_auditory,_motor,_visual,_cognitive,_other)_that_substantially_limits_one_or_more_major_life_activites_as_described_by_theâ_americans_with_disabilities_act_of_1990")
df <- rename(df, psat_math = "psat_scores_|__|_math")
df <- rename(df, sat_math = "sat_scores_|__|_math")
df <- rename(df, act_math = "act_scores_|__|_math")
df <- rename(df, psat_verbal = "psat_scores_|__|_verbal")
df <- rename(df, psat_writing = "psat_scores_|__|_writing")
df <- rename(df, sat_verbal = "sat_scores_|__|_verbal")
df <- rename(df, sat_writing = "sat_scores_|__|_writing")
df <- rename(df, act_science = "act_scores_|__|_science")
df <- rename(df, act_read = "act_scores_|__|_reading")
df <- rename(df, act_writing = "act_scores_|__|_writing")
df <- rename(df, act_verbal = "act_scores_|__|_verbal")
df <- rename(df, american_citizen = "are_you_an_american_citizen")
df <- rename(df, first_gen = "will_you_be_the_first_person_in_your_family_to_attend_college")
df <- rename(df, school_impact = "do_you_believe_your_school_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research")
df <- rename(df, geographic_location = "how_do_you_describe_where_you_live")
df <- rename(df, house_size = "household_size")


df <- df %>% mutate(gender = tolower(gender))
df <- df %>% mutate(gender = if_else(gender == "male", 1, 0))
df <- df %>% mutate(gpa_weight = tolower(gpa_weight))
df <- df %>% mutate(gpa_weight = if_else(gpa_weight == "yes", 1, 0))


df <- df %>%
  mutate(
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
    house_size = as.integer(house_size)
  )

# need to bring in stipend eligible
df_stipend <- read_excel(here('data',"hillman_raw.xlsx"),
  sheet = "2019")

df_stipend <- df_stipend %>% select(first_name, last_name, stipend)
df <- left_join(df, df_stipend)

df$year <- 2019

df_2019 <- df








########################################################
######################## 2020
########################################################

# read in data
df <- read_excel(here('data',"hillman_2020.xlsx"))
colnames(df) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(df) %<>% str_replace_all(":", "")
colnames(df) %<>% str_replace_all("\\?", "")

colnames(df)
df <- select(df, c(
  first, last, gender, current_grade...5, alumni, race...15,
  school, city, state, zip, "are_you_a_current_jack_kent_cooke_foundation_(jkcf)_young_scholar",
  gpa, "weighted_(y/n)", "psat_scores_|__|_math":"act_scores_|__|_science",
  "low_income", "disability", "1st_gen_college", "what_is_your_total_household_size",
  "how_do_you_describe_where_you_live", "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below,_if_you_meet_at_least_two_of_the_following_criteria_you_w...",
  "are_you_an_american_citizen",
  "stipend_eligible...77"
))

df <- rename(df, high_school = "school")

df <- rename(df, first_name = first)
df <- rename(df, last_name = last)
df <- rename(df, grade = "current_grade...5")
df <- rename(df, self_identity = "race...15")
df <- rename(df, jkcf = "are_you_a_current_jack_kent_cooke_foundation_(jkcf)_young_scholar")
df <- rename(df, gpa_weight = "weighted_(y/n)")
df <- rename(df, psat_math = "psat_scores_|__|_math")
df <- rename(df, psat_reading_writing = "psat_scores_|__|_reading_and_writing")
df <- rename(df, sat_math = "sat_scores_|__|_math")
df <- rename(df, sat_reading_writing = "sat_scores_|__|_reading_and_writing")
df <- rename(df, act_math = "act_scores_|__|_math")
df <- rename(df, act_verbal = "act_scores_|__|_verbal")
df <- rename(df, act_writing = "act_scores_|__|_writing")
df <- rename(df, act_read = "act_scores_|__|_reading")
df <- rename(df, act_science = "act_scores_|__|_science")
df <- rename(df, documented_disability = "disability")
df <- rename(df, first_gen = "1st_gen_college")
df <- rename(df, house_size = "what_is_your_total_household_size")
df <- rename(df, geographic_location = "how_do_you_describe_where_you_live")
df <- rename(df, school_impact = "do_you_believe_your_environment_negatively_impacts_your_educational_opportunities_related_to_obtaining_a_career_in_science_research_please_look_at_the_list_below,_if_you_meet_at_least_two_of_the_following_criteria_you_w...")
df <- rename(df, american_citizen = "are_you_an_american_citizen")
df <- rename(df, stipend = "stipend_eligible...77")













df <- df %>% mutate(gender = tolower(gender))
df <- df %>% mutate(gender = if_else(gender == "male", 1, 0))
df <- df %>% mutate(gpa_weight = tolower(gpa_weight))
df <- df %>% mutate(gpa_weight = if_else(gpa_weight == "yes", 1, 0))


df <- df %>%
  mutate(
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


df$year <- 2020

df_2020 <- df



########################################################
######################## merge
########################################################

# append all df
df <- bind_rows(df_2017, df_2018, df_2019, df_2020)


# merge back dob
dob_df <- read_excel(here('data',"hillman_raw.xlsx"),
  col_types = c(
    "text", "text", "date",
    "numeric"
  )
)


dob_df <- rename(dob_df, year = "hillman_year")
df <- left_join(df, dob_df)


df <- df %>% select(-c(middle_name, high_school_pub_priv, school_district, reduced_lunch, offer_ap, decision, `income_eligible_(reduced_lunch)`, alumni, act_verbal, low_income))


df %>%
  missing_plot()

# create missing indicator 
# create dataframe and make them for all the variables I match on, although coursened should be ok?

# ask david if I can have a clean stipend eligible

df %>%
  filter(year == 2017) |>
  missing_plot()



df <- df %>% mutate(first_name = tolower(first_name))
df <- df %>% mutate(last_name = tolower(last_name))


df$first_name <- gsub("\\(.*", "", df$first_name)
df$first_name <- gsub('\\".*', "", df$first_name)

# age relative to the year they applied
df$year_dt <- lubridate::ymd(df$year, truncated = 2L)
df$age <- trunc((df$dob %--% df$year_dt) / years(1))


hist(df$grade)
hist(df$house_size)

df |> vtable(missing = T)


# fixing gpa
df <- df |> mutate(gpa = case_when(
  gpa > 96 ~ 4.0,
  between(gpa, 90, 96) ~ 3.7,
  TRUE ~ as.numeric(gpa)
))

# fixing sat
which(colnames(df)=="act_writing")
df[, 11:20][df[, 11:20] == 0] <- NA

# fixing house size
df$house_size[df$house_size > 11] <- NA

#age
df$age[df$age == 0] <- NA
df$age[df$age == 7] <- NA



# find duplicates

df %>% group_by(first_name, last_name, year) %>% 
  filter(n()>1) %>% summarize(n=n())



#sat issue with multiple scores
#
#
#
#
#

#dealing with duplicates, taking their latest applicaiton year because they report sat
df <- df %>%
  group_by(first_name, last_name) |>
  mutate(applicant_max = max(year)) 



df %>% group_by(first_name, last_name, year) %>% 
  filter(n()>1) %>% summarize(n=n())

df<-df[!(df$first_name=="amanda" & df$last_name=="lu"),]
df<-df[!(df$first_name=="imani" & df$last_name=="smith"),]



applicants <- df
        
