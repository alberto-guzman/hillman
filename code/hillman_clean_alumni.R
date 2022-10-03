here()


source(here('code','hillman_clean_raw.R'))


#import updated alumni tracker 2022
alum <- read_csv(here('data',"Alumni Tracker SL 5.9.2022.csv"), 
                 col_types = cols(`Source (Survey, LinkedIn, NSC Etc.)` = col_skip(), 
                                  `Last check` = col_skip(), Email = col_skip(), 
                                  `Project title` = col_skip(), `Public Profile` = col_skip(), 
                                  Site = col_skip(), PI = col_skip(), 
                                  `PI Email` = col_skip(), `Parent Name` = col_skip(), 
                                  `Parent Email` = col_skip(), Phone = col_skip(), 
                                  Profession = col_skip(), `Publication link` = col_skip()))



#clean column names
colnames(alum) %<>% str_replace_all("\\s", "_") %<>% tolower()
colnames(alum) %<>% str_replace_all(":", "")
colnames(alum) %<>% str_replace_all("\\?", "")

# removed prefered name to just legal name for matching purposes 
alum$first <- gsub('\\(.*',"",alum$first)
alum$first <- gsub('\\".*',"",alum$first)


#cleaning gender
alum <- alum |> mutate(gender=recode(gender, 
                                      `M`=1,
                                      `F`=0,
                                      'Male'=1,
                                      'Female'=1))




#create max and min participation year
alum <- alum |>
  mutate(year_max = pmax(year1,year2,year3,year4, na.rm = T)
         ,year_min = pmin(year1,year2,year3,year4,na.rm = T))

         
         

#high school graduation
alum$high_school_graduation_expected <- gsub('\\(.*',"",alum$high_school_graduation_expected)


# out <- alum |> 
#   select(last, first, year_max, year_min, high_school_graduation_expected) |> 
#   filter(year_min >= 2017 & year_max <= 2020)
# 

# dob


alum <- alum %>% select(last, first, year1:year4, notes, high_school_graduation_expected, year_max, year_min)


alum <- rename(alum,first_name = first)
alum <- rename(alum,last_name = last)



alum <- alum %>% mutate(first_name = tolower(first_name))
alum <- alum %>% mutate(last_name = tolower(last_name))



#write_csv(out,'hillman_part_year.csv')

alum %>% group_by(first_name, last_name) %>% 
  filter(n()>1) %>% summarize(n=n())

alum <- alum %>% group_by(first_name, last_name) %>% 
  filter(n()==1) |> 
  ungroup()

# pivot to long
alum <- alum |> 
  select(first_name, last_name, year1:year4) |> 
  pivot_longer(
    cols = starts_with('year'),
    names_to = 'year'
  )


alum <- alum |>  select(first_name, last_name, value) |> 
  rename(year = value) 

alum <- drop_na(alum)


alum$treatment <- 1















