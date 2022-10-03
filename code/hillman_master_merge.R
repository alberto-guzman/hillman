# merge treatment

master_df <- applicants |> left_join(alum)



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



master_df |> vtable(missing = T)

master_df <- select(master_df,-c(zip,gpa_weight,psat_reading_writing,sat_reading_writing,dob,year_dt,age,applicant_max,jkcf))

master_df |> vtable(missing = T)


# clean these to prepare for matching 

master_df <- master_df %>% mutate(stipend = recode(stipend, 'Yes'=1, 'No'=0))

#self)identify will be a case_when
#geographic locaiton will also be case_when

# create ever treated variable

  

