#install packages
library(readr)
library(tidyverse)
library(stringr)

#set working directory
setwd("~/Projects/inProgress/2020_hillman/data")


df <- read_csv("nsc/CLEANED_502003_T209672.202110221010_DA.csv", 
               col_types = cols(`Your Unique Identifier` = col_skip(), 
                                `Middle Initial` = col_skip(), `Name Suffix` = col_skip(), 
                                `Requester Return Field` = col_skip(), 
                                `Search Date` = col_skip(), `Enrollment Begin` = col_date(format = "%Y%m%d"), 
                                `Enrollment End` = col_date(format = "%Y%m%d"), 
                                `Graduation Date` = col_date(format = "%Y%m%d")))



#rename 2 and 4 year variable
df <- df |> 
  rename(college_2or4 = `2-year / 4-year`)



df$`First Name` <- str_to_title(df$`First Name`)
df$`Last Name` <- str_to_title(df$`Last_Name`)









#create Fall 2020 seamless enrollment
nsc.working <-
  df |> 
  mutate(
    Enrollment.Status = ifelse(is.na(Enrollment.Status) == TRUE, 'unk', Enrollment.Status)
    , enroll_length = Enrollment.End - Enrollment.Begin
    , enr_any_fall = Enrollment.Begin >= as.Date(paste0(format(Enrollment.Begin, '%Y'), '-08-01'), '%Y-%m-%d') & format(Enrollment.Begin, '%m') < 11
    
    , enr_first_fall20 = format(Enrollment.Begin, '%Y') == 2020 & enr_any_fall
    
    , enr_first_fall20_4y = enr_first_fall20 & college_2or4 == 4
    , enr_first_fall20_2y = enr_first_fall20 & college_2or4 == 2
    
    , enr_first_fall20_pub = enr_first_fall20 & Public...Private == 'Public'
    , enr_first_fall20_pri = enr_first_fall20  & Public...Private == 'Private'
    
    , enr_first_fall20_ft = enr_first_fall20 & Enrollment.Status == 'F'
    , enr_first_fall20_pt = enr_first_fall20 & Enrollment.Status == 'Q' | Enrollment.Status == 'H' | Enrollment.Status == 'L'
    
    , enr_first_fall20_ft4y = enr_first_fall20_ft & college_2or4 == 4
    , enr_first_fall20_ft2y = enr_first_fall20_ft & college_2or4 == 2
    , enr_first_fall20_pt4y = enr_first_fall20_pt & college_2or4 == 4
    , enr_first_fall20_pt2y = enr_first_fall20_pt & college_2or4 == 2
  )



#create Fall 2020 enrollment
nsc.working <-
  nsc.working |> 
  mutate(
    enr_any_spring = Enrollment.Begin >= as.Date(paste0(format(Enrollment.Begin, '%Y'), '-01-01'), '%Y-%m-%d') & format(Enrollment.Begin, '%m') < 8
    , enr_first_spring21 = format(Enrollment.Begin, '%Y') == 2021 & enr_any_spring
    
    , enr_any_20 = format(Enrollment.Begin, '%Y') == 2020 & enr_first_fall20 | enr_first_spring21
    
    , enr_any_20_4y = enr_any_20 & college_2or4 == 4
    , enr_any_20_2y = enr_any_20 & college_2or4 == 2
    
    , enr_any_20_ft = enr_any_20 & Enrollment.Status == 'F'
    , enr_any_20_pt = enr_any_20 & Enrollment.Status == 'Q' | Enrollment.Status == 'H' | Enrollment.Status == 'L'
    
    , enr_any_20_ft4y = enr_any_20_ft & college_2or4 == 4
    , enr_any_20_ft2y = enr_any_20_pt & college_2or4 == 2
    , enr_any_20_pt4y = enr_any_20_pt & college_2or4 == 4
    , enr_any_20_pt2y = enr_any_20_pt & college_2or4 == 2
    
    , enr_any = Enrollment.Begin >=  as.Date('2020-08-01')
    
    , enr_any_4y = enr_any & college_2or4 == 4
    , enr_any_2y = enr_any & college_2or4 == 2
    
    , enr_any_ft = enr_any & Enrollment.Status == 'F'
    , enr_any_ft = enr_any & Enrollment.Status == 'Q' | Enrollment.Status == 'H' | Enrollment.Status == 'L'
    
    , enr_any_ft4y = enr_any_ft & college_2or4 == 4
    , enr_any_ft2y = enr_any_ft & college_2or4 == 2
    , enr_any_pt4y = enr_any_ft & college_2or4 == 4
    , enr_any_pt2y = enr_any_ft & college_2or4 == 2
  )


#create persistence outcomes
nsc.working <-
  nsc.working |> 
  mutate(
    comp_fall20 = enr_first_fall20 & Enrollment.End >= as.Date('2020-12-01') 
    , pers_spring21 = comp_fall20 & enr_first_spring21
    , comp_pers_spring21 = pers_spring21 & Enrollment.End >= as.Date('2021-05-01')
    , pers_fall21 = format(Enrollment.Begin, '%Y') == 2021 & enr_any_fall & comp_pers_spring21
    , comp_pers_fall21 = pers_fall21 & Enrollment.End >= as.Date('2021-12-01')
    , pers_spring22 = format(Enrollment.Begin, '%Y') == 2022 & enr_any_spring & comp_pers_fall21
    , comp_pers_spring22 = pers_spring22 & Enrollment.End >= as.Date('2022-05-01')
  )


#create gap outcomes
nsc.working <-
  nsc.working |> 
  mutate(
    gap_sem = !enr_first_fall20 & format(Enrollment.Begin, '%Y') == 2021 & enr_any_spring
    , gap_year =!enr_first_spring21 & gap_sem & format(Enrollment.Begin, '%Y') == 2021 & enr_any_fall
  )


#replace NA to False
nsc.working <- nsc.working %>% mutate_each(funs(replace(., is.na(.), F)), enroll_length:gap_year)
#convert boolean to integers
nsc.working <- nsc.working %>% mutate_each(funs(as.integer(.)), enroll_length:gap_year)


#create one line per row
test <- nsc.working |> 
  group_by(applicant_id) |> 
  summarise(enroll_fall = max(gap_sem),
            tx = max(sample_intervention))

lm = lm(test$enroll_fall ~ test$tx)
summary(lm)





test <-
  nsc.working |> select(applicant_id, College.Name,Record.Found.Y.N,Enrollment.Begin, Enrollment.End, enr_first_fall20,enroll_length:gap_year)

test <- arrange(test, applicant_id, Enrollment.Begin)                                






test <- df |> 
  spread(applicant_id, College.Code.Branch, Enrollment.Begin) 






df <- df |> 
  clean_names()


df <- df |> 
  rename(record_found = record_found_y_n) 

df$record_found <- dplyr::recode(df$record_found, "Y" = 1, "N" = 0)



df <- select(df,-search_date)

df <- df |> 
  rename(college_2or4 = x2_year_4_year) 




#will need to add hs grad data 
#will need to add ipeds crosswalk data
