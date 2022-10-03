# merge treatment

master_df <- merge(alum, applicants, by = c('first_name','last_name'), all = T)









test <- master_df %>% select(first_name, last_name, dob, high_school_graduation_expected, year, year1, year2, year3, year4, year_max, year_min, treatment)



test <- test %>% 
  arrange(first_name, last_name, year)

df$treatment[is.na(df$treatment)] = 0

df %>%
  tabyl(treatment,year)

df %>%
  tabyl(treatment,city)

df_alumni %>%
  tabyl(part_year4)

data_long <- gather(df_alumni, year, dob, control:cond2, factor_key=TRUE)

