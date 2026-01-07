# =============================================================================
# Standardized variables
# =============================================================================

merged_clean <- merged_df |>
  mutate(
    # --- GPA: numeric, 0 becomes NA
    gpa = suppressWarnings(as.numeric(gpa)) |> na_if(0),

    # --- PSAT Math: numeric, 0 becomes NA
    psat_math = suppressWarnings(as.numeric(psat_math)) |> na_if(0),

    # --- Stipend: yes/no → 1/0
    stipend = case_when(
      str_detect(
        stipend,
        regex("\\b(yes|stipend eligible|yes student)\\b", ignore_case = TRUE)
      ) ~
        1L,
      TRUE ~ 0L
    ),

    # --- Household size: cap 1–11
    house_size = suppressWarnings(as.integer(house_size)),
    house_size = if_else(between(house_size, 1L, 11L), house_size, NA_integer_),

    # --- First-gen: yes/no → 1/0
    first_gen = case_when(
      str_detect(tolower(as.character(first_gen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(first_gen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Fix 2023 reverse-coded first-gen
    first_gen = if_else(
      year == 2023 & !is.na(first_gen),
      1L - first_gen, # Flip 0→1 and 1→0
      first_gen
    ),

    # --- Race / identity flags -----------------------------------------------
    racially_marginalized = case_when(
      str_detect(
        tolower(self_identity),
        "black|african|latino|latinx|hispanic|native|indian|pacific"
      ) ~
        1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),
    bi_multi_racial = case_when(
      str_detect(tolower(self_identity), "multi|bi-?racial|two or more") ~ 1L,
      is.na(self_identity) ~ NA_integer_,
      TRUE ~ 0L
    ),

    # --- Geography flags ------------------------------------------------------
    urban = if_else(
      str_detect(tolower(geographic_location), "urban"),
      1L,
      0L,
      missing = NA_integer_
    ),
    suburban = if_else(
      str_detect(tolower(geographic_location), "suburban"),
      1L,
      0L,
      missing = NA_integer_
    ),
    rural = if_else(
      str_detect(tolower(geographic_location), "rural"),
      1L,
      0L,
      missing = NA_integer_
    ),

    # --- Disability: yes/no → 1/0
    disability = case_when(
      str_detect(
        documented_disability,
        regex("^\\s*no\\b", ignore_case = TRUE)
      ) ~
        0L,
      !is.na(documented_disability) ~ 1L,
      TRUE ~ NA_integer_
    ),

    # --- Negative school environment flag
    neg_school = case_when(
      str_detect(school_impact, regex("^\\s*no\\b", ignore_case = TRUE)) ~ 0L,
      !is.na(school_impact) ~ 1L,
      TRUE ~ NA_integer_
    ),

    # --- Citizenship: yes/no → 1/0
    us_citizen = case_when(
      str_detect(tolower(as.character(american_citizen)), "yes|1|true") ~ 1L,
      str_detect(tolower(as.character(american_citizen)), "no|0|false") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

merged_clean <- merged_clean |>
  select(
    first_name,
    last_name,
    year,
    hs_grad_year,
    treated_ever,
    treated_in_year,
    years_treated,
    high_school,
    state,
    gender,
    grade,
    gpa,
    psat_math,
    stipend,
    house_size,
    racially_marginalized,
    bi_multi_racial,
    urban,
    suburban,
    rural,
    disability,
    neg_school,
    us_citizen,
    first_gen
  )


rm(list = setdiff(ls(), c("alum", "applicants", "merged_clean")))
