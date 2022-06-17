#bring in library
library(tidyverse)
library(readxl)
library(magrittr)
library(janitor)


#set working directory
setwd("~/Projects/inProgress/2020_hillman/data")


#import updated alumni tracker 2022
alum <- read_csv("Alumni Tracker SL 5.9.2022.csv", 
                 col_types = cols(`Source (Survey, LinkedIn, NSC Etc.)` = col_skip(), 
                                  `Last check` = col_skip(), Email = col_skip(), 
                                  `Project title` = col_skip(), `Public Profile` = col_skip(), 
                                  Site = col_skip(), PI = col_skip(), 
                                  `PI Email` = col_skip(), `Parent Name` = col_skip(), 
                                  `Parent Email` = col_skip(), Phone = col_skip(), 
                                  Profession = col_skip(), `Publication link` = col_skip()))



#clean column names
alum <- alum |> 
  clean_names() 

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


alum <- alum %>% select(last, first, year1:year4, notes, high_school_graduation_expected, year_max, year_min)


alum <- rename(alum,first_name = first)
alum <- rename(alum,last_name = last)



alum <- alum %>% mutate(first_name = tolower(first_name))
alum <- alum %>% mutate(last_name = tolower(last_name))

alum$treatment <- 1


write_csv(out,'hillman_part_year.csv')











#years participated
alum <- alum |> 
  separate(years_participated, c("year_a","year_b","year_c","year_d"), sep = "[/,]", remove = FALSE, extra = 'merge', fill = "right") 


















alum |>
  tabyl(gender)





alum$still_in_high_school[is.na(alum$still_in_high_school)] <- 0

alum <- alum |> mutate(still_in_high_school=recode(still_in_high_school, 
                                                    `High School`=1,
                                                    `0`=0))


alum |>
  tabyl(still_in_high_school)



alum |>
  tabyl(upci_participation_year)

alum <- alum |> 
  separate(upci_participation_year, c("year_a","year_b","year_c","year_d"), sep = "[/,]", remove = FALSE, extra = 'merge', fill = "right") 



head(alum)
alum |>
  select(upci_participation_year,year_a,year_b,year_c,year_d) |>
  write_csv("test.csv")




alum |>
  tabyl(high_school)



alum |>
  tabyl(high_school_graduation_year)


alum |>
  select(last_name,first_name,gender,still_in_high_school,upci_participation_year,year_a,year_b,year_c,year_d,high_school,high_school_graduation_year)



alum |>
  tabyl(upci_participation_year)


