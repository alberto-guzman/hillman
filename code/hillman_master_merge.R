# merge treatment

master_df <- applicants |> left_join(alum)

master_df <- ungroup(master_df)

master_df %>% group_by(first_name, last_name, year) %>% 
  filter(n()>1) %>% summarize(n=n())



master_df <- master_df %>% 
  arrange(first_name, last_name, year)

master_df$treatment[is.na(master_df$treatment)] = 0





master_df %>%
  tabyl(treatment,year)
  
master_df |> 
  select(first_name, last_name, year, treatment) |> 
  group_by(first_name,last_name) |> 
  mutate(sum = sum(treatment)) |> 
  filter(sum > 1)




master_df <- select(master_df,-c(zip,gpa_weight,psat_reading_writing,sat_reading_writing,dob,year_dt,applicant_max,jkcf))



# clean these to prepare for matching 

master_df <- master_df %>% mutate(stipend = recode(stipend, 'Yes'=1, 'No'=0))

#self)identify will be a case_when


master_df <-
  master_df |>
  mutate(
    african_american = case_when(
      self_identity == "African American" | self_identity == "African-America" | self_identity == "Black" ~ 1,
      TRUE ~ 0),
    asian = case_when(
      self_identity == "Asian" ~ 1,
      TRUE ~ 0),
    hawaiian_pacific_islander = case_when(
      self_identity == "Asian Hawaiian or Pacific Islander" | self_identity == "Asian, Hawaiian/Pacific Islander" | self_identity == "Hawaiian/Pacific\r\nIslander" | self_identity == "Hawaiian/Pacific Islander" ~ 1,
      TRUE ~ 0),
    latinx = case_when(
      self_identity == "Hispanic" | self_identity == "Hispanic/ Latinx" | self_identity == "Hispanic/Latino" ~ 1,
      TRUE ~ 0),
    white = case_when(
      self_identity == "Caucasian/White" | self_identity == "White" | self_identity == "White/Caucasian" ~ 1,
      TRUE ~ 0)
    )
      






      
      
master_df <-
  master_df |>
  mutate(
    bi_multi_racial = case_when(
      african_american == 0 &  asian == 0 & hawaiian_pacific_islander == 0 & latinx == 0 & white == 0 ~ 1,
      TRUE ~ 0)
  )

master_df <-
  master_df |>
  mutate(
    racially_marginalized = case_when(
      african_american == 1 | hawaiian_pacific_islander == 1 | latinx == 1 ~ 1,
      TRUE ~ 0)
  )
 
master_df <-
  master_df |>
  mutate(
    urban = case_when(
      geographic_location == "Urban" ~ 1,
      TRUE ~ 0),
    suburban = case_when(
      self_identity == "Suburban" ~ 1,
      TRUE ~ 0),
    rural = case_when(
      geographic_location == "Rural/Small Town" | geographic_location == "Rural/SmallTown" ~ 1,
      TRUE ~ 0)
  )


master_df <- master_df %>% mutate(disability = recode(documented_disability, 'Yes' = 1, 'yes' = 1, 'Disability' = 1, 'No' = 0, 'no' = 0))
                                                      



master_df <- master_df |>
  mutate(
    neg_school = case_when(
      str_detect(school_impact, "yes|Yes") ~ 1,
      str_detect(school_impact, "no|No") ~ 0))


master_df <- master_df |>
  mutate(
    us_citizen = case_when(
      str_detect(american_citizen, "yes|Yes") ~ 1,
      str_detect(american_citizen, "no|No") ~ 0))


master_df <- master_df %>% mutate(first_gen = recode(first_gen, 'Yes' = 1, 'yes' = 1, '1st Gen. College' = 1,'No' = 0, 'no' = 0))




master_df <- select(master_df,-c(self_identity,geographic_location,documented_disability, school_impact, american_citizen))



  
master_df |> vtable(missing = T)




# missing indicators
library(optmatch)


master_df_fill <- fill.NAs(treatment ~ gender + grade + age + gpa + sat_math + sat_verbal + sat_writing + psat_math + psat_verbal + psat_writing +
                             act_math + act_read + act_science + act_writing + stipend + house_size + first_gen + racially_marginalized + 
                             bi_multi_racial + urban + suburban + year +
                             rural + disability + neg_school + us_citizen, data = master_df)



master_df_fill |> vtable(missing = T)


# subset to years we have data for
# playing with matching
# first will need to fix the excel shifting issue
# create only urm and and use bi-multi racial 


library(MatchIt)

m.out <- matchit(treatment ~ gender + grade + age + gpa + sat_math + sat_verbal + sat_writing + psat_math + psat_verbal + psat_writing +
                   act_math + act_read + act_science + act_writing + stipend + house_size + first_gen + racially_marginalized + 
                   bi_multi_racial + urban + suburban +
                   rural + disability + neg_school + us_citizen + 
                   gender.NA + grade.NA + age.NA + gpa.NA + sat_math.NA + sat_verbal.NA + sat_writing.NA + psat_math.NA + psat_verbal.NA + psat_writing.NA +
                   act_math.NA + act_read.NA + act_science.NA + act_writing.NA + stipend.NA + house_size.NA + first_gen.NA + 
                   disability.NA + neg_school.NA + us_citizen.NA,
                 data = master_df_fill,
                 exact = ~ year + stipend,
                 method = "optimal", 
                 distance = "glm", 
                 discard = 'control')

plot(summary(m.out))

bal.plot(m.out, var.name = "distance", which = "both", colors = c("#003594","#FFB81C")) +
  ggtitle("Propensity Score Distribution") + xlab('Propensity Score')


summary(m.out)
